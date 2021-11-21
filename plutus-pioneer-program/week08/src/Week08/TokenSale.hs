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

module Week08.TokenSale
    ( TokenSale (..)
    , TSRedeemer (..)
    , TSStartSchema
    , TSUseSchema
    , startEndpoint
    , useEndpoints
    ) where

import           Control.Monad                hiding (fmap)
import           Data.Aeson                   (FromJSON, ToJSON)
import           Data.Monoid                  (Last (..))
import           Data.Text                    (Text, pack)
import           GHC.Generics                 (Generic)
import           Plutus.Contract              as Contract
import           Plutus.Contract.StateMachine
import qualified PlutusTx
import           PlutusTx.Prelude             hiding (Semigroup(..), check, unless)
import           Ledger                       hiding (singleton)
import           Ledger.Ada                   as Ada
import           Ledger.Constraints           as Constraints
import qualified Ledger.Typed.Scripts         as Scripts
import           Ledger.Value
import           Prelude                      (Semigroup (..), Show (..), uncurry)
import qualified Prelude

data TokenSale = TokenSale   -- TokenSale is the parameter to use for the contract
    { tsSeller :: !PubKeyHash  -- the public key hash of the seller
    , tsToken  :: !AssetClass  -- the token been sold given by it's asset class
    , tsTT     :: !(Maybe ThreadToken) -- The ThreadToken will identify the correct UTXO at the contract address that contains the tokens
    } deriving (Show, Generic, FromJSON, ToJSON, Prelude.Eq)

PlutusTx.makeLift ''TokenSale

data TSRedeemer =  -- provide contract operations (as showned in the lecture diagram)
      SetPrice Integer   -- set price to new value; this is token price in lovelaces 
    | AddTokens Integer  -- the argument gives the amount of tokens to add
    | BuyTokens Integer  -- the argument gives the amount of tokens to buy
    | Withdraw Integer Integer  -- first argument says how many tokens to withdraw; second argument says how many lovelaces to withdraw
    deriving (Show, Prelude.Eq)

PlutusTx.unstableMakeIsData ''TSRedeemer

{-# INLINABLE lovelaces #-}
lovelaces :: Value -> Integer  -- helper function that provides the amount fo lovelaces given a value
lovelaces = Ada.getLovelace . Ada.fromValue

-- the transition function of the state machine
{-# INLINABLE transition #-}  -- 1st arg is the parameter; 2nd arg is the state, which for datum it uses Integer; 3rd arg is the redeemer
transition :: TokenSale -> State Integer -> TSRedeemer -> Maybe (TxConstraints Void Void, State Integer) -- returns a Maybe pair of the constraints and the new state (datum and value)
transition ts s r = case (stateValue s, stateData s, r) of  -- split given state in value and datum;
    (v, _, SetPrice p)   | p >= 0           -> Just ( Constraints.mustBeSignedBy (tsSeller ts) -- only allow to set price if it's greater than 0; must be signed by token seller
                                                    , State p v       -- for the new state, the new datum will be the new price set and value stays the same
                                                    )
    (v, p, AddTokens n)  | n > 0            -> Just ( mempty     -- the amount of tokens to add must be positive; no constraints (seller will create state machine)
                                                    , State p $  -- for the new state, price doesn't change but the value does
                                                      v                                       <>  -- the original value 
                                                      assetClassValue (tsToken ts) n              --  n amount of tokens are added to the value of the new state
                                                    )
    (v, p, BuyTokens n)  | n > 0            -> Just ( mempty      -- the amount of tokens to add must be positive; no constraints
                                                    , State p $   -- for the new state, price doesn't change but the value does
                                                      v                                       <>  -- the original value 
                                                      assetClassValue (tsToken ts) (negate n) <>  -- subtract number of tokens bought
                                                      lovelaceValueOf (n * p)   -- add also lovelaces paid for the tokens (the amount of tokens times current price)
                                                    )
    (v, p, Withdraw n l) | n >= 0 && l >= 0 -> Just ( Constraints.mustBeSignedBy (tsSeller ts)  -- onoy seller cna withdraw from contract
                                                    , State p $      -- price doesn't change
                                                      v                                       <> -- the original value
                                                      assetClassValue (tsToken ts) (negate n) <> -- minus n amount of tokens that are taken out
                                                      lovelaceValueOf (negate l)                 -- minus the lovelaves that are taken out
                                                    )
    _                                       -> Nothing    -- in all other cases return nothing

{-# INLINABLE tsStateMachine #-} -- define state machine; use smart constructor called mkStateMachine
tsStateMachine :: TokenSale -> StateMachine Integer TSRedeemer -- no need to do any checks in the state machine hence can use mkStateMachine
tsStateMachine ts = mkStateMachine (tsTT ts) (transition ts) (const False) -- takes 3 arguments; Maybe ThreadToken, transition function, and function to decide if state is Final or not
                                                                           -- (const False) is a constant function that always returns False
{-# INLINABLE mkTSValidator #-} -- turn state machine into a validator function
mkTSValidator :: TokenSale -> Integer -> TSRedeemer -> ScriptContext -> Bool
mkTSValidator = mkValidator . tsStateMachine

type TS = StateMachine Integer TSRedeemer

tsTypedValidator :: TokenSale -> Scripts.TypedValidator TS     -- the typed validator
tsTypedValidator ts = Scripts.mkTypedValidator @TS
    ($$(PlutusTx.compile [|| mkTSValidator ||]) `PlutusTx.applyCode` PlutusTx.liftCode ts)
    $$(PlutusTx.compile [|| wrap ||])
  where
    wrap = Scripts.wrapValidator @Integer @TSRedeemer

tsValidator :: TokenSale -> Validator    -- get the validator from the typed validator
tsValidator = Scripts.validatorScript . tsTypedValidator

tsAddress :: TokenSale -> Ledger.Address  -- get the address from the validator
tsAddress = scriptAddress . tsValidator

tsClient :: TokenSale -> StateMachineClient Integer TSRedeemer  -- define the state machine client to interact with the state machine off-chain code in the wallet
tsClient ts = mkStateMachineClient $ StateMachineInstance (tsStateMachine ts) (tsTypedValidator ts)  -- client allows to start and step the state machine

mapErrorSM :: Contract w s SMContractError a -> Contract w s Text a  -- helper function to make Text error message
mapErrorSM = mapError $ pack . show  -- pack . show comverts errors to text 

-- Below  are the definitions for off-chain code
startTS :: AssetClass -> Bool -> Contract (Last TokenSale) s Text () -- start token sale function takes the token for sale (AssetClass) as the first parameter
startTS token useTT = do                                -- for 2nd parameter, it's a Boolen that uses ThreadToken mechanism of state machines if True (Use True in production code)
    pkh <- pubKeyHash <$> Contract.ownPubKey  -- look up public key hash of seller
    tt  <- if useTT then Just <$> mapErrorSM getThreadToken else return Nothing  -- getThreadToken function returns a ThreadToken; mapErrorSM converts error type to Text
    let ts = TokenSale
            { tsSeller = pkh
            , tsToken  = token
            , tsTT     = tt
            }
        client = tsClient ts
    void $ mapErrorSM $ runInitialise client 0 mempty -- kick of state machine by creating first UTXO at state machine address; runInitialise takes client, datum (initial price), and initial value of zero using mempty
    tell $ Last $ Just ts -- communicate the token sale value to be obserable from outside parties with Tell
    logInfo $ "started token sale " ++ show ts

-- Just use runstep for all the cases below to step a transition in the state machine
setPrice :: TokenSale -> Integer -> Contract w s Text ()
setPrice ts p = void $ mapErrorSM $ runStep (tsClient ts) $ SetPrice p  -- runStep takes the client and the redeemer which depends on the case 

addTokens :: TokenSale -> Integer -> Contract w s Text ()
addTokens ts n = void $ mapErrorSM $ runStep (tsClient ts) $ AddTokens n

buyTokens :: TokenSale -> Integer -> Contract w s Text ()
buyTokens ts n = void $ mapErrorSM $ runStep (tsClient ts) $ BuyTokens n

withdraw :: TokenSale -> Integer -> Integer -> Contract w s Text ()
withdraw ts n l = void $ mapErrorSM $ runStep (tsClient ts) $ Withdraw n l -- withdraw n amount of tokens and l amount of loverlaces

-- define schemas to make usable from the outside
type TSStartSchema =
        Endpoint "start"      (CurrencySymbol, TokenName, Bool)  -- takes a triple for the assetclass (split into CurrencySymbol and TokenName) and the bool
type TSUseSchema =
        Endpoint "set price"  Integer
    .\/ Endpoint "add tokens" Integer
    .\/ Endpoint "buy tokens" Integer
    .\/ Endpoint "withdraw"   (Integer, Integer)

startEndpoint :: Contract (Last TokenSale) TSStartSchema Text ()
startEndpoint = forever     -- given some monadic computation, it forever repeats it; instead of calling startEndpoint recursively (>> startEndpoint)
              $ handleError logError -- wrap contract into an error handle so if something goes wrong, the contract wont crash and just log the error instead
              $ awaitPromise -- turns the Promise returned back into a contract
              $ endpoint @"start" $ \(cs, tn, useTT) -> startTS (AssetClass (cs, tn)) useTT -- endpoint has changed from the lecture before; now has an argument for continuation
                                                                                            -- continuation function defines what to do with provided parameter values
                                                                                            -- overall result of endpoint operation is of type Promise w s e b
                                                                                            -- Promise is a wrapper indicating that this contract starts with a waiting action (use with `select`)
                                                                                            -- Promise is a contract that first waits for external input
useEndpoints :: TokenSale -> Contract () TSUseSchema Text ()
useEndpoints ts = forever
                $ handleError logError
                $ awaitPromise
                $ setPrice' `select` addTokens' `select` buyTokens' `select` withdraw' -- use `select` which now takes Promises; waits until a case gets an input
  where                                                                                -- once an operation is finished, it will offer all four again
    setPrice'  = endpoint @"set price"  $ setPrice ts
    addTokens' = endpoint @"add tokens" $ addTokens ts
    buyTokens' = endpoint @"buy tokens" $ buyTokens ts
    withdraw'  = endpoint @"withdraw"   $ uncurry (withdraw ts) -- different because it takes a tuple of 2 integers; uncurry handles the additional integers
