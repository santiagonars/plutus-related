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
-- The idea of this game is that if the sum of the choices is even, the first player wins, if it's odd then the second player wins
module Week07.EvenOdd
    ( Game (..)
    , GameChoice (..)
    , FirstParams (..)
    , SecondParams (..)
    , GameSchema
    , endpoints
    ) where

import           Control.Monad        hiding (fmap)
import           Data.Aeson           (FromJSON, ToJSON)
import qualified Data.Map             as Map
import           Data.Text            (Text)
import           GHC.Generics         (Generic)
import           Ledger               hiding (singleton)
import           Ledger.Constraints   as Constraints
import qualified Ledger.Typed.Scripts as Scripts
import           Ledger.Ada           as Ada
import           Ledger.Value
import           Playground.Contract  (ToSchema)
import           Plutus.Contract      as Contract
import qualified PlutusTx
import           PlutusTx.Prelude     hiding (Semigroup(..), unless)
import           Prelude              (Semigroup (..), Show (..), String)
import qualified Prelude

data Game = Game  -- data Game is used as a parameter for the contract
    { gFirst          :: !PubKeyHash  -- First player identified by the PubKeyHash 
    , gSecond         :: !PubKeyHash  -- Second player
    , gStake          :: !Integer     -- Integer to denote number of lovelaces to be used as stake in the game by each player
    , gPlayDeadline   :: !POSIXTime   -- What time the second player has to make a move before first player can reclaim stake
    , gRevealDeadline :: !POSIXTime   -- If the second has made a move, how much time the first has to claim victory by reveling the nonce
    , gToken          :: !AssetClass  -- An arbitrary NFT used to identify the right intance of a UTXO been used
    } deriving (Show, Generic, FromJSON, ToJSON, Prelude.Eq, Prelude.Ord)

PlutusTx.makeLift ''Game

data GameChoice = Zero | One  -- the two moves the player can make
    deriving (Show, Generic, FromJSON, ToJSON, ToSchema, Prelude.Eq, Prelude.Ord)

instance Eq GameChoice where  -- Need to define Eq for Plutus equivant 
    {-# INLINABLE (==) #-}  -- need to include in-linable pragma for == operation for it to work in template haskell
    Zero == Zero = True
    One  == One  = True
    _    == _    = False

PlutusTx.unstableMakeIsData ''GameChoice

-- data GameDatum is used as state for the contract; this is th4e Datum for the valiadation contract
data GameDatum = GameDatum ByteString (Maybe GameChoice)  -- ByteString is the hash the first player submits; Maybe GameChoice is the move by the second player
    deriving Show                                         -- GameChoice is a Maybe because in the beginning the second player hasn't made a moved yet

instance Eq GameDatum where  -- Need a Plutus equality (Eq) definition for GameDatum
    {-# INLINABLE (==) #-}
    GameDatum bs mc == GameDatum bs' mc' = (bs == bs') && (mc == mc') -- the two are equal if the hash and the game choice are equal

PlutusTx.unstableMakeIsData ''GameDatum

-- Custom type for redeemer
data GameRedeemer = Play GameChoice | Reveal ByteString | ClaimFirst | ClaimSecond  -- Play is for second player to select game choice 0 or 1
    deriving Show -- Reveal for the when the first player has won and must prove it by reveling the nonce as a ByteString
                  -- ClaimFirst is for when the second player doesn't make a move, the first player can claim back stake
                  -- ClaimSecond is for when first player doesn't reveal because it has lost, so the second player can claim the winnings
PlutusTx.unstableMakeIsData ''GameRedeemer

{-# INLINABLE lovelaces #-}
lovelaces :: Value -> Integer  -- Given a value, extract the number of lovelaves contained in it
lovelaces = Ada.getLovelace . Ada.fromValue

{-# INLINABLE gameDatum #-} -- Given an output of a transaction and a function that takes a DatumHash and returns a Maybe Datum, then it could return a Maybe Datum
gameDatum :: TxOut -> (DatumHash -> Maybe Datum) -> Maybe GameDatum
gameDatum o f = do
    dh      <- txOutDatum o  -- try to get the DatumHash from an output (it may fail)
    Datum d <- f dh          -- turn the DatumHash into a Datum
    PlutusTx.fromBuiltinData d  -- pass Datum as something of type GameDatum

{-# INLINABLE mkGameValidator #-} -- mkGameValidator has the core business logic
mkGameValidator :: Game -> ByteString -> ByteString -> GameDatum -> GameRedeemer -> ScriptContext -> Bool
mkGameValidator game bsZero' bsOne' dat red ctx =      -- 1st arg is the Game type; 2nd & 3rd args represent ByteString of digits 0 and 1 correspondingly
    traceIfFalse "token missing from input" (assetClassValueOf (txOutValue ownInput) (gToken game) == 1) && -- check that input been validated contains the state token; this condition applies to all cases
    case (dat, red) of -- for the different situations
        (GameDatum bs Nothing, Play c) ->   -- case where the first player has moved, and the second player is moving now for this transaction
            traceIfFalse "not signed by second player"   (txSignedBy info (gSecond game))                                   && -- check moved is signed by second player
            traceIfFalse "first player's stake missing"  (lovelaces (txOutValue ownInput) == gStake game)                   && -- check first player put down a stake
            traceIfFalse "second player's stake missing" (lovelaces (txOutValue ownOutput) == (2 * gStake game))            && -- check second player adds a stake
            traceIfFalse "wrong output datum"            (outputDatum == GameDatum bs (Just c))                             && -- Datum must be same as before except Noithign is now replaced by Just c, where is the second player's choice
            traceIfFalse "missed deadline"               (to (gPlayDeadline game) `contains` txInfoValidRange info)         && -- move has to happen first deadline
            traceIfFalse "token missing from output"     (assetClassValueOf (txOutValue ownOutput) (gToken game) == 1)         -- NFT must be passed to the new UTXO

        (GameDatum bs (Just c), Reveal nonce) -> -- case where both players have moved and the first player has won, and wants to prove it to get the winnings
            traceIfFalse "not signed by first player"    (txSignedBy info (gFirst game))                                    && -- check moved is signed by first player
            traceIfFalse "commit mismatch"               (checkNonce bs nonce c)                                            && -- nonce must be same as the one submitted before
            traceIfFalse "missed deadline"               (to (gRevealDeadline game) `contains` txInfoValidRange info)       && -- claim winning before gRevealDeadline
            traceIfFalse "wrong stake"                   (lovelaces (txOutValue ownInput) == (2 * gStake game))             && -- input must contain stake for both players
            traceIfFalse "NFT must go to first player"   nftToFirst                                                            -- return NFT to first player

        (GameDatum _ Nothing, ClaimFirst) ->  -- case where first players wants wants stake back because second player hasn't moved yet and the gameplace deadline has passed
            traceIfFalse "not signed by first player"    (txSignedBy info (gFirst game))                                    && -- must be signed by first player
            traceIfFalse "too early"                     (from (1 + gPlayDeadline game) `contains` txInfoValidRange info)   && -- must happen after first dealine has passed
            traceIfFalse "first player's stake missing"  (lovelaces (txOutValue ownInput) == gStake game)                   && -- check first player has provided stake
            traceIfFalse "NFT must go to first player"   nftToFirst                                                            -- return NFT to first player

        (GameDatum _ (Just _), ClaimSecond) -> -- case  where both players have moved and for second player can retrieve winnings as first player missed reveal deadline because he/she lost
            traceIfFalse "not signed by second player"   (txSignedBy info (gSecond game))                                   && -- must be signed by second player
            traceIfFalse "too early"                     (from (1 + gRevealDeadline game) `contains` txInfoValidRange info) && -- can happen after reveal dealine has passed
            traceIfFalse "wrong stake"                   (lovelaces (txOutValue ownInput) == (2 * gStake game))             && -- check input UTXO has correct amount of stake
            traceIfFalse "NFT must go to first player"   nftToFirst                                                            -- return NFT to first player

        _                              -> False
  where
    info :: TxInfo
    info = scriptContextTxInfo ctx

    ownInput :: TxOut -- valiate the usage of UTXO
    ownInput = case findOwnInput ctx of  
        Nothing -> traceError "game input missing"
        Just i  -> txInInfoResolved i  -- txInInfoResolved provides the corresponding outputs of a TxInput of a transaction

    ownOutput :: TxOut  -- if the game is not over, we want there to be a new UTXO carrying the NFT with the updated Datum and updated value
    ownOutput = case getContinuingOutputs ctx of -- check outputs that go to the same address 
        [o] -> o  -- only succeeds if there is exactly 1 output
        _   -> traceError "expected exactly one game output"

    outputDatum :: GameDatum -- tries to provide the Datum whem there is 1 output 
    outputDatum = case gameDatum ownOutput (`findDatum` info) of
        Nothing -> traceError "game output datum not found"
        Just d  -> d
    -- checkNonce function is only revelent in the case the first player knows it was won and wants to reveal the nonce and prove that the hash submitted fits the nonce provided
    checkNonce :: ByteString -> ByteString -> GameChoice -> Bool -- Arg 1 is the hash submitted; 2nd arg is the nonce revealed; 3rd arg is the move both players made
    checkNonce bs nonce cSecond = sha2_256 (nonce `concatenate` cFirst) == bs  -- validate that the hash submitted by firt player is the same as the compute hash of the nonce
      where                                                                    -- compute hash by applying sha2_256 to the concatenation of the nonce with the GameChoice
        cFirst :: ByteString -- needs to be in ByteString to concatenate with the nonce; cSecond is passed as type GameChoice 
        cFirst = case cSecond of
            Zero -> bsZero'  -- use bsZero' for the Zero GameChoice
            One  -> bsOne'   -- use bsOne' for the Zero GameChoice

    nftToFirst :: Bool  -- first player gets the NFT back since he/she kicked it off 
    nftToFirst = assetClassValueOf (valuePaidTo info $ gFirst game) (gToken game) == 1 -- valuePaidTo add up all values that go to a PubKeyHash in an output of a transaction

data Gaming
instance Scripts.ValidatorTypes Gaming where
    type instance DatumType Gaming = GameDatum
    type instance RedeemerType Gaming = GameRedeemer

bsZero, bsOne :: ByteString  -- defien the ByteString for choices Zero and One
bsZero = "0"
bsOne  = "1"

typedGameValidator :: Game -> Scripts.TypedValidator Gaming
typedGameValidator game = Scripts.mkTypedValidator @Gaming   -- mkGameValidator is parametirized by game, a ByteString for bsZero, and a ByteString for bsOne
    ($$(PlutusTx.compile [|| mkGameValidator ||])
        `PlutusTx.applyCode` PlutusTx.liftCode game
        `PlutusTx.applyCode` PlutusTx.liftCode bsZero  -- this is a ByteString constant
        `PlutusTx.applyCode` PlutusTx.liftCode bsOne)  -- this is a ByteString constant
    $$(PlutusTx.compile [|| wrap ||])
  where
    wrap = Scripts.wrapValidator @GameDatum @GameRedeemer

gameValidator :: Game -> Validator
gameValidator = Scripts.validatorScript . typedGameValidator

gameAddress :: Game -> Ledger.Address
gameAddress = scriptAddress . gameValidator

-- A helper function for the off-chain code to find the UTXO that carries the NFT of the game in the adress of the gameValidator t
findGameOutput :: Game -> Contract w s Text (Maybe (TxOutRef, TxOutTx, GameDatum)) -- gets the Game and tries to find the UTXO in the contract monad
findGameOutput game = do
    utxos <- utxoAt $ gameAddress game
    return $ do
        (oref, o) <- find f $ Map.toList utxos -- find returns the first UTXO in the list that passes the f predicate; Map.toList creates a map list of utxos as (TxOutRef, TxOutTx)
        dat       <- gameDatum (txOutTxOut o) (`Map.lookup` txData (txOutTxTx o)) -- get the Datum of the UTXO
        return (oref, o, dat) -- return a triple with the UTXO identifier, the UTXO itself, and the Datum of the UTXO
  where
    f :: (TxOutRef, TxOutTx) -> Bool
    f (_, o) = assetClassValueOf (txOutValue $ txOutTxOut o) (gToken game) == 1  -- check that out contains the token

-- A helper function for the off-chain code to wait until a given POSIXTime has passed and waits until the next slot
waitUntilTimeHasPassed :: AsContractError e => POSIXTime -> Contract w s e ()
waitUntilTimeHasPassed t = do
    s1 <- currentSlot  -- get the current slot
    logInfo @String $ "current slot: " ++ show s1 ++ ", waiting until " ++ show t -- log slot captured as ss1
    void $ awaitTime t >> waitNSlots 1 -- awaitTime is provided by the contract monad to wait a given POSIXTime; then wait for one slot
    s2 <- currentSlot -- get the new current slot
    logInfo @String $ "waited until: " ++ show s2 -- log the slot captured as s2

data FirstParams = FirstParams
    { fpSecond         :: !PubKeyHash     -- first player nees to pass the PubKeyHash of the second player
    , fpStake          :: !Integer        -- define the stake of the game
    , fpPlayDeadline   :: !POSIXTime      -- define the first deadline for game play
    , fpRevealDeadline :: !POSIXTime      -- define the second deadline for first player to reclaim funds if it has won
    , fpNonce          :: !ByteString     -- need the nonce the first player wants to use to consele the game choice
    , fpCurrency       :: !CurrencySymbol -- add the the NFT currency symbol
    , fpTokenName      :: !TokenName      -- add the the NFT token name
    , fpChoice         :: !GameChoice     -- the choice the first player wants to make
    } deriving (Show, Generic, FromJSON, ToJSON, ToSchema)

firstGame :: forall w s. FirstParams -> Contract w s Text () -- contract for the first player
firstGame fp = do
    pkh <- pubKeyHash <$> Contract.ownPubKey -- get own public key hash
    let game = Game  -- define the value of the Game type; use params defined in FirstParams except for gFirst
            { gFirst          = pkh -- first player will be the owner of the wallet that invokes this contract
            , gSecond         = fpSecond fp
            , gStake          = fpStake fp
            , gPlayDeadline   = fpPlayDeadline fp
            , gRevealDeadline = fpRevealDeadline fp
            , gToken          = AssetClass (fpCurrency fp, fpTokenName fp) -- assemble CurrencySymbol and TokenName into an asset class 
            }
        v    = lovelaceValueOf (fpStake fp) <> assetClassValue (gToken game) 1  -- the value is the stake to put in the UTXO plus the NFT
        c    = fpChoice fp                                                      -- the choice selected by first player (the user invoking this contract)
        bs   = sha2_256 $ fpNonce fp `concatenate` if c == Zero then bsZero else bsOne -- create the hash of the nonce concatenated with user's choice
        tx   = Constraints.mustPayToTheScript (GameDatum bs Nothing) v -- constraint to produce a script output with the given Datum of 
    ledgerTx <- submitTxConstraints (typedGameValidator game) tx -- submit transaction to with constraint to typedGameValidator
    void $ awaitTxConfirmed $ txId ledgerTx
    logInfo @String $ "made first move: " ++ show (fpChoice fp)

    waitUntilTimeHasPassed $ fpPlayDeadline fp -- wait until first deadline (for game play) has passed

    m   <- findGameOutput game -- check if UTXO with game NFT is there
    now <- currentTime
    case m of
        Nothing             -> throwError "game output not found"
        Just (oref, o, dat) -> case dat of   -- if UTXO with game NFT is found, then it has a triple with the UTXO identifier, the UTXO itself, and the Datum of the UTXO
            GameDatum _ Nothing -> do  -- for the first case if the second player hasn't move and the gameplay deadline has passed
                logInfo @String "second player did not play" -- invoke a transaction for ClaimFirst reddemmer for first player to get stake back
                let lookups = Constraints.unspentOutputs (Map.singleton oref o) <> -- must provide the UTXO
                              Constraints.otherScript (gameValidator game) -- must provide the validator of the game
                    tx'     = Constraints.mustSpendScriptOutput oref (Redeemer $ PlutusTx.toBuiltinData ClaimFirst) <> -- costraint to spend the UTXO with ClaimFirst as redeemer
                              Constraints.mustValidateIn (from now) -- must submit transaction after currentTime
                ledgerTx' <- submitTxConstraintsWith @Gaming lookups tx'
                void $ awaitTxConfirmed $ txId ledgerTx'
                logInfo @String "reclaimed stake"

            GameDatum _ (Just c') | c' == c -> do -- for the second case is if second player did play and selected the same choice as the first player, hence first player wins
                                                  -- first player wins (this case), then it has to reveal the nonce to the winnings
                logInfo @String "second player played and lost"  -- 
                let lookups = Constraints.unspentOutputs (Map.singleton oref o) <> -- must provide the UTXO
                              Constraints.otherScript (gameValidator game) -- must provide the validator 
                    tx'     = Constraints.mustSpendScriptOutput oref (Redeemer $ PlutusTx.toBuiltinData $ Reveal $ fpNonce fp) <> -- use the Reveal fpNonce redeemer
                              Constraints.mustValidateIn (to $ now + 1000) -- must submit transaction before deadline for revealing has passed
                ledgerTx' <- submitTxConstraintsWith @Gaming lookups tx'
                void $ awaitTxConfirmed $ txId ledgerTx'
                logInfo @String "victory"

            _ -> logInfo @String "second player played and won" -- last case is that the second player did move and won

data SecondParams = SecondParams -- 
    { spFirst          :: !PubKeyHash
    , spStake          :: !Integer
    , spPlayDeadline   :: !POSIXTime
    , spRevealDeadline :: !POSIXTime
    , spCurrency       :: !CurrencySymbol
    , spTokenName      :: !TokenName
    , spChoice         :: !GameChoice
    } deriving (Show, Generic, FromJSON, ToJSON, ToSchema)

secondGame :: forall w s. SecondParams -> Contract w s Text () -- contract for the second player
secondGame sp = do
    pkh <- pubKeyHash <$> Contract.ownPubKey -- get own public key hash (this would be for second player which is invokling this secondGame)
    let game = Game -- define the value of the Game type
            { gFirst          = spFirst sp
            , gSecond         = pkh
            , gStake          = spStake sp
            , gPlayDeadline   = spPlayDeadline sp
            , gRevealDeadline = spRevealDeadline sp
            , gToken          = AssetClass (spCurrency sp, spTokenName sp)
            }
    m <- findGameOutput game -- find the UTXO with the game NFT
    case m of
        Just (oref, o, GameDatum bs Nothing) -> do  -- in this case if the UTXO with the game NFT is found with the 2nd argument for GameDatum as Nothing (second player hasn't played yet)
            logInfo @String "running game found"
            now <- currentTime
            let token   = assetClassValue (gToken game) 1 -- token is the game NFT
            let v       = let x = lovelaceValueOf (spStake sp) in x <> x <> token -- the value to put in the new output; must contain the stake the first player added, the new stake, and the NFT
                c       = spChoice sp  -- the choice second player uses for the game
                lookups = Constraints.unspentOutputs (Map.singleton oref o)                                   <> -- must provide lookups for the UTXO
                          Constraints.otherScript (gameValidator game)                                        <> -- must provide the validator to consume the new UTXO
                          Constraints.typedValidatorLookups (typedGameValidator game)                            -- must provide script instance of the validator to produce a UTXO
                tx      = Constraints.mustSpendScriptOutput oref (Redeemer $ PlutusTx.toBuiltinData $ Play c) <> -- must spent existing UTXO and invoke the Play redeemer for ther choice
                          Constraints.mustPayToTheScript (GameDatum bs $ Just c) v                            <> -- create a new UTXO with updated datum for Just c but same bs and the new value
                          Constraints.mustValidateIn (to now)   -- must do this before dealine passes to make a play move
            ledgerTx <- submitTxConstraintsWith @Gaming lookups tx
            let tid = txId ledgerTx
            void $ awaitTxConfirmed tid
            logInfo @String $ "made second move: " ++ show (spChoice sp)
            
            waitUntilTimeHasPassed $ spRevealDeadline sp -- at this point it's the first player's turn; need to wait until reveal deadline has passed 

            m'   <- findGameOutput game -- try to find the game UTXO, which could now be a different one 
            now' <- currentTime
            case m' of
                Nothing             -> logInfo @String "first player won" -- if game UTXO wasn't found, it means first player has already revealed the nonce and has won
                Just (oref', o', _) -> do -- if UTXO was found, it mean the first player didn't reveal because he/she left the game and missed the deadline or loss the game
                    logInfo @String "first player didn't reveal"  -- second player can now claim the winnings
                    let lookups' = Constraints.unspentOutputs (Map.singleton oref' o')                                     <> -- must provide the UTXO
                                   Constraints.otherScript (gameValidator game)                                               -- must provide the validator
                        tx'      = Constraints.mustSpendScriptOutput oref' (Redeemer $ PlutusTx.toBuiltinData ClaimSecond) <> -- must spend UTXO found with ClaimSecond as redeemer
                                   Constraints.mustValidateIn (from now')                                                  <> -- must spend after spRevealDeadline has passed
                                   Constraints.mustPayToPubKey (spFirst sp) token                                             -- must hand back NFT to the first player
                    ledgerTx' <- submitTxConstraintsWith @Gaming lookups' tx'
                    void $ awaitTxConfirmed $ txId ledgerTx'
                    logInfo @String "second player won"

        _ -> logInfo @String "no running game found"  -- in this case the UTXO with game NFT was not found, hence nothing can be done

type GameSchema = Endpoint "first" FirstParams .\/ Endpoint "second" SecondParams -- define GameSchema to make the game accessible

endpoints :: Contract () GameSchema Text () -- define a contract called endpoints to offer the choice between the two endpoints
endpoints = (first `select` second) >> endpoints -- after it's run, it recursive offers the choices again
  where
    first  = endpoint @"first"  >>= firstGame
    second = endpoint @"second" >>= secondGame
