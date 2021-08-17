{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}

module Week07.StateMachine  -- similar to game in EvenOdd.hs except using state machine (See EvenOdd.hs for notes on repeated in this module)
    ( Game (..)
    , GameChoice (..)
    , FirstParams (..)
    , SecondParams (..)
    , GameSchema
    , Last (..)
    , ThreadToken
    , Text
    , endpoints
    ) where

import           Control.Monad                hiding (fmap)
import           Data.Aeson                   (FromJSON, ToJSON)
import           Data.Monoid                  (Last (..))
import           Data.Text                    (Text, pack)
import           GHC.Generics                 (Generic)
import           Ledger                       hiding (singleton)
import           Ledger.Ada                   as Ada
import           Ledger.Constraints           as Constraints
import           Ledger.Typed.Tx
import qualified Ledger.Typed.Scripts         as Scripts
import           Plutus.Contract              as Contract
import           Plutus.Contract.StateMachine
import qualified PlutusTx
import           PlutusTx.Prelude             hiding (Semigroup(..), check, unless)
import           Playground.Contract          (ToSchema)
import           Prelude                      (Semigroup (..), Show (..), String)
import qualified Prelude

data Game = Game
    { gFirst          :: !PubKeyHash
    , gSecond         :: !PubKeyHash
    , gStake          :: !Integer
    , gPlayDeadline   :: !POSIXTime
    , gRevealDeadline :: !POSIXTime
    , gToken          :: !ThreadToken
    } deriving (Show, Generic, FromJSON, ToJSON, Prelude.Eq)

PlutusTx.makeLift ''Game

data GameChoice = Zero | One
    deriving (Show, Generic, FromJSON, ToJSON, ToSchema, Prelude.Eq, Prelude.Ord)

instance Eq GameChoice where
    {-# INLINABLE (==) #-}
    Zero == Zero = True
    One  == One  = True
    _    == _    = False

PlutusTx.unstableMakeIsData ''GameChoice

data GameDatum = GameDatum ByteString (Maybe GameChoice) | Finished  -- slight changed from non-state machine approach 
    deriving Show                                                    -- Finished: a 2nd constructor to represent the final state of the state machine
                                                                     --           won't correspond to a UTXO but it is needed for state machine mechanism to work
instance Eq GameDatum where
    {-# INLINABLE (==) #-}
    GameDatum bs mc == GameDatum bs' mc' = (bs == bs') && (mc == mc')
    Finished        == Finished          = True  -- need to account for this constructor as well now
    _               == _                 = False

PlutusTx.unstableMakeIsData ''GameDatum

data GameRedeemer = Play GameChoice | Reveal ByteString | ClaimFirst | ClaimSecond
    deriving Show

PlutusTx.unstableMakeIsData ''GameRedeemer

{-# INLINABLE lovelaces #-}
lovelaces :: Value -> Integer
lovelaces = Ada.getLovelace . Ada.fromValue

{-# INLINABLE gameDatum #-}
gameDatum :: TxOut -> (DatumHash -> Maybe Datum) -> Maybe GameDatum
gameDatum o f = do
    dh      <- txOutDatum o
    Datum d <- f dh
    PlutusTx.fromBuiltinData d

-- this the transition function of the state machine (Somewhat corresponds to the mkGameValidator function in the non-state machine approach)
{-# INLINABLE transition #-}  -- Takes the game, the s is a pair consisting of datum and value, and r is the redeemer
transition :: Game -> State GameDatum -> GameRedeemer -> Maybe (TxConstraints Void Void, State GameDatum) -- Returns Nothing if not allowed or a Just pair of new state transaction constraints 
transition game s r = case (stateValue s, stateData s, r) of  -- stateValue s is value been consumed; stateData s is the datum  
    (v, GameDatum bs Nothing, Play c) -- 1st case: Condition to check that value contained in the consumed UTXO is the stake of the game
        | lovelaces v == gStake game         -> Just ( Constraints.mustBeSignedBy (gSecond game)                    <>   -- result of transition function is a pair of with 2 components:
                                                       Constraints.mustValidateIn (to $ gPlayDeadline game)              --   the first component are the constraints of the transaction 
                                                     , State (GameDatum bs $ Just c) (lovelaceValueOf $ 2 * gStake game) --   the second component is the new state of the resulting UTXO
                                                     )                                  -- NFT is left out of the resukting value because the state machine implicitly takes care of that
    (v, GameDatum _ (Just _), Reveal _) -- 2nd case: Condition to check that value contained in the consumed UTXO is twice the stake of the game
        | lovelaces v == (2 * gStake game)   -> Just ( Constraints.mustBeSignedBy (gFirst game)                     <> -- check that it's signed by first player
                                                       Constraints.mustValidateIn (to $ gRevealDeadline game)          -- 
                                                     , State Finished mempty                                           -- indicate game is over; return state as Final state
                                                     )                                                                 -- NFT gets automatically burned since it is returning a Final state
    (v, GameDatum _ Nothing, ClaimFirst) -- 3rd case: Second player doesn't play hence first player can get funds back; check that value in the consumed UTXO is equal to the stake
        | lovelaces v == gStake game         -> Just ( Constraints.mustBeSignedBy (gFirst game)                     <> -- check that it's signed by first player
                                                       Constraints.mustValidateIn (from $ 1 + gPlayDeadline game)      -- first play can only reclaim funds after play deadline has passsed
                                                     , State Finished mempty
                                                     )
    (v, GameDatum _ (Just _), ClaimSecond) -- 4rd case: Second play has won or first player didn't reveal; check that value in the consumed UTXO is equal to the twice stake
        | lovelaces v == (2 * gStake game)   -> Just ( Constraints.mustBeSignedBy (gSecond game)                    <> -- check that it's signed by second player
                                                       Constraints.mustValidateIn (from $ 1 + gRevealDeadline game)    -- must happen after reveal deadline has passed
                                                     , State Finished mempty                                           -- indicate transition to the finished state
                                                     )
    _                                        -> Nothing   -- all other cases are invalid

{-# INLINABLE final #-}
final :: GameDatum -> Bool  -- final is jsut the finished state
final Finished = True
final _        = False

{-# INLINABLE check #-} -- left out nonce check in transition function because it couldn't be express as a constraint
check :: ByteString -> ByteString -> GameDatum -> GameRedeemer -> ScriptContext -> Bool
check bsZero' bsOne' (GameDatum bs (Just c)) (Reveal nonce) _ =           -- only condition need is that Just c is included and Reveal nonce for the redeemer
    sha2_256 (nonce `concatenate` if c == Zero then bsZero' else bsOne') == bs -- hash the nonce concatenated witht he choice and compare witht the originally provided hash 
check _       _      _                       _              _ = True  -- no additonal checks needed so can jsut return true

{-# INLINABLE gameStateMachine #-} -- define the state machine; has 3 parameters inputs and returns a state machine
gameStateMachine :: Game -> ByteString -> ByteString -> StateMachine GameDatum GameRedeemer
gameStateMachine game bsZero' bsOne' = StateMachine   -- profine the four fields that define a state machine
    { smTransition  = transition game       -- the transition function 
    , smFinal       = final                 -- check to know if it's in a final state
    , smCheck       = check bsZero' bsOne'  -- additional check for the nonce
    , smThreadToken = Just $ gToken game    -- take from the game value
    }

{-# INLINABLE mkGameValidator #-} -- the old mkGameValidator is replaced by this function using mkValidator of gameStateMachine 
mkGameValidator :: Game -> ByteString -> ByteString -> GameDatum -> GameRedeemer -> ScriptContext -> Bool
mkGameValidator game bsZero' bsOne' = mkValidator $ gameStateMachine game bsZero' bsOne'

type Gaming = StateMachine GameDatum GameRedeemer -- before used a different mechanism to bundle DatumType and RedeemerType; StateMachine here does the same

bsZero, bsOne :: ByteString
bsZero = "0"
bsOne  = "1"

-- second version of gameStateMachine where only game is specified, not the 2 ByteStrings for choices
gameStateMachine' :: Game -> StateMachine GameDatum GameRedeemer -- wont work in on-chain code because it doesn't have the in-linable pragma because of missing literal strings
gameStateMachine' game = gameStateMachine game bsZero bsOne      -- works fine for off-chain code without having to prodive the 2 additional parameters

typedGameValidator :: Game -> Scripts.TypedValidator Gaming
typedGameValidator game = Scripts.mkTypedValidator @Gaming
    ($$(PlutusTx.compile [|| mkGameValidator ||])
        `PlutusTx.applyCode` PlutusTx.liftCode game
        `PlutusTx.applyCode` PlutusTx.liftCode bsZero
        `PlutusTx.applyCode` PlutusTx.liftCode bsOne)
    $$(PlutusTx.compile [|| wrap ||])
  where
    wrap = Scripts.wrapValidator @GameDatum @GameRedeemer

gameValidator :: Game -> Validator
gameValidator = Scripts.validatorScript . typedGameValidator

gameAddress :: Game -> Ledger.Address
gameAddress = scriptAddress . gameValidator

gameClient :: Game -> StateMachineClient GameDatum GameRedeemer  -- this is the game state machine client; it is what is need to interact with the state machine from a wallet (off-chain code)
gameClient game = mkStateMachineClient $ StateMachineInstance (gameStateMachine' game) (typedGameValidator game)

data FirstParams = FirstParams
    { fpSecond         :: !PubKeyHash
    , fpStake          :: !Integer
    , fpPlayDeadline   :: !POSIXTime
    , fpRevealDeadline :: !POSIXTime
    , fpNonce          :: !ByteString
    , fpChoice         :: !GameChoice
    } deriving (Show, Generic, FromJSON, ToJSON, ToSchema)

mapError' :: Contract w s SMContractError a -> Contract w s Text a -- state machine contracts use a specific constraint of the error type; T
mapError' = mapError $ pack . show                                 -- define the contract to return the error as Text with pack . show 

waitUntilTimeHasPassed :: AsContractError e => POSIXTime -> Contract w s e ()
waitUntilTimeHasPassed t = void $ awaitTime t >> waitNSlots 1

firstGame :: forall s. FirstParams -> Contract (Last ThreadToken) s Text ()
firstGame fp = do
    pkh <- pubKeyHash <$> Contract.ownPubKey
    tt  <- mapError' getThreadToken -- getThreadToken identifies the UTXO in wallet to use for minting the NFT; apply mapError' to convert to Text error messages
    let game   = Game               -- must not do any transactions after invoking getThreadToken and before using it for state machine (It could chnage the UTXOs in the wallet)
            { gFirst          = pkh
            , gSecond         = fpSecond fp
            , gStake          = fpStake fp
            , gPlayDeadline   = fpPlayDeadline fp
            , gRevealDeadline = fpRevealDeadline fp
            , gToken          = tt  -- use the ThreadToken 
            }
        client = gameClient game -- define the game client
        v      = lovelaceValueOf (fpStake fp)  -- define the value for the stake
        c      = fpChoice fp  -- define the user's choice
        bs     = sha2_256 $ fpNonce fp `concatenate` if c == Zero then bsZero else bsOne -- defien the hash used to commit for the game choice move
    void $ mapError' $ runInitialise client (GameDatum bs Nothing) v   --  runInitialise takes the client, the datum, and the value
    logInfo @String $ "made first move: " ++ show (fpChoice fp)        -- runInitialise will first mint the NFT corresponding to the ThreadToken, and then create 
    tell $ Last $ Just tt -- use tell writer mechanism to communicate the ThreadToken;       a UTXO at thestate machine address and out NFT to identify it
                          -- tell is used in order for second player to find the game with the ThreadToken
    waitUntilTimeHasPassed $ fpPlayDeadline fp

    m <- mapError' $ getOnChainState client -- getOnChainState takes the client and returns something of Maybe OnChainState if it finds it and a UTXO map
    case m of                               -- OnChainState is a tuple consiting of TypedScriptTxOut (Typed version provides the TxOut and also the Datum) and TypedScriptTxOutRef
        Nothing             -> throwError "game output not found"    
        Just ((o, _), _) -> case tyTxOutData o of -- only interested in the o of TypedScriptTxOut which is the UTXO itself; tyTxOutData gets the datum 

            GameDatum _ Nothing -> do  -- case that the second player hasn't move
                logInfo @String "second player did not play"
                void $ mapError' $ runStep client ClaimFirst    -- runStep is the important function here; It creates a transaction and submits to transition the state machine
                logInfo @String "first player reclaimed stake"  -- runStep takes as input the client and the redeemer; it will return a transition result whether it failed or suceeded with the new state

            GameDatum _ (Just c') | c' == c -> do  -- case that the second player has moved and first player believes it won
                logInfo @String "second player played and lost"
                void $ mapError' $ runStep client $ Reveal $ fpNonce fp   -- runStep doesnt need any constraint and lookups or helper functions
                logInfo @String "first player revealed and won"           -- runStep uses the constraints specified in the gameStateMachine and transition function

            _ -> logInfo @String "second player played and won"

data SecondParams = SecondParams
    { spFirst          :: !PubKeyHash
    , spStake          :: !Integer
    , spPlayDeadline   :: !POSIXTime
    , spRevealDeadline :: !POSIXTime
    , spChoice         :: !GameChoice
    , spToken          :: !ThreadToken
    } deriving (Show, Generic, FromJSON, ToJSON)

secondGame :: forall w s. SecondParams -> Contract w s Text ()
secondGame sp = do
    pkh <- pubKeyHash <$> Contract.ownPubKey
    let game   = Game
            { gFirst          = spFirst sp
            , gSecond         = pkh
            , gStake          = spStake sp
            , gPlayDeadline   = spPlayDeadline sp
            , gRevealDeadline = spRevealDeadline sp
            , gToken          = spToken sp
            }
        client = gameClient game
    m <- mapError' $ getOnChainState client
    case m of
        Nothing          -> logInfo @String "no running game found"
        Just ((o, _), _) -> case tyTxOutData o of -- 
            GameDatum _ Nothing -> do -- this case is when second player hasn't played yet
                logInfo @String "running game found"
                void $ mapError' $ runStep client $ Play $ spChoice sp 
                logInfo @String $ "made second move: " ++ show (spChoice sp)

                waitUntilTimeHasPassed $ spRevealDeadline sp

                m' <- mapError' $ getOnChainState client -- get the new state
                case m' of
                    Nothing -> logInfo @String "first player won" -- if there is none it measn the first player has won
                    Just _  -> do  -- if there is something it means the first player hasn't won
                        logInfo @String "first player didn't reveal"
                        void $ mapError' $ runStep client ClaimSecond
                        logInfo @String "second player won"

            _ -> throwError "unexpected datum"

type GameSchema = Endpoint "first" FirstParams .\/ Endpoint "second" SecondParams

endpoints :: Contract (Last ThreadToken) GameSchema Text ()
endpoints = (first `select` second) >> endpoints
  where
    first  = endpoint @"first"  >>= firstGame
    second = endpoint @"second" >>= secondGame
