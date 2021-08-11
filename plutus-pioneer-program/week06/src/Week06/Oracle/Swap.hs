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

module Week06.Oracle.Swap
    ( SwapSchema
    , swap
    ) where

import           Control.Monad        hiding (fmap)
import           Data.List            (find)
import qualified Data.Map             as Map
import           Data.Maybe           (mapMaybe)
import           Data.Monoid          (Last (..))
import           Data.Text            (Text)
import           Plutus.Contract      as Contract
import qualified PlutusTx
import           PlutusTx.Prelude     hiding (Semigroup(..), (<$>), unless, mapMaybe, find)
import           Ledger               hiding (singleton)
import           Ledger.Constraints   as Constraints
import qualified Ledger.Typed.Scripts as Scripts
import           Ledger.Ada           as Ada hiding (divide)
import           Ledger.Value         as Value
import           Prelude              (Semigroup (..), Show (..), String, (<$>))

import           Week06.Oracle.Core
import           Week06.Oracle.Funds

{-# INLINABLE price #-}
price :: Integer -> Integer -> Integer  -- helper function to get the token price
price lovelace exchangeRate = (lovelace * exchangeRate) `divide` 1000000

{-# INLINABLE lovelaces #-}
lovelaces :: Value -> Integer  -- helper function to extract the amount of lovelaces given a value
lovelaces = Ada.getLovelace . Ada.fromValue  -- functions provided  by Plutus library

{-# INLINABLE mkSwapValidator #-} -- contains 2 parameters- the Oracle and the address of the Oracle
mkSwapValidator :: Oracle -> Address -> PubKeyHash -> () -> ScriptContext -> Bool -- For Datum is the PubKeyHash of the seller
mkSwapValidator oracle addr pkh () ctx =
    txSignedBy info pkh ||   -- first condition is if the seller signed the transaction, then funds can be retrieved
    (traceIfFalse "expected exactly two script inputs" hasTwoScriptInputs &&  -- The Oracle and the swap itself
     traceIfFalse "price not paid"                     sellerPaid)  -- check that the seller gets paid

  where
    info :: TxInfo
    info = scriptContextTxInfo ctx  -- get transaction info

    oracleInput :: TxOut  -- find the Oracle input
    oracleInput =
      let
        ins = [ o                       -- list comprehension to get all inputs with corresponding output that are contained in the Oracle address 
              | i <- txInfoInputs info  -- txInfoInputs returns a list of all the inputs from the info field; i loops through all transaction inputs
              , let o = txInInfoResolved i  -- txInInfoResolved is used to get the corresponding output for every i (every input)
              , txOutAddress o == addr  -- filter condition to only keep i values that where conditon is true; txOutAddress returns the address of the output
              ]                         -- the address where the outputs (of the inputs) sit at has to be the same as the oracle address (addr)
      in
        case ins of
            [o] -> o   -- check that there is exactly one oracle input; 
            _   -> traceError "expected exactly one oracle input"

    oracleValue' = case oracleValue oracleInput (`findDatum` info) of  -- Use `findDatum` to get the Datum hash of the oracle output given by oracleInput; 
        Nothing -> traceError "oracle value not found"                 -- oracleValue provides the Datum value given a Datum hash
        Just x  -> x  -- if it succeeds, the Integer value of the datum is returned; this is the exchange rate

    hasTwoScriptInputs :: Bool
    hasTwoScriptInputs =   -- txInInfoResolved gives the output of the input; txOutAddress gives the address of that output
      let                  -- toValidatorHash gives the validator hash of the script out but nothing if it's a public key; isJust is true if it's a Just, and False if it's Nothings
        xs = filter (isJust .   . txOutAddress . txInInfoResolved) $ txInfoInputs info  -- xs is a list of all the script inputs
      in
        length xs == 2  -- check that it's exactly 2, the Oracle and the swap itself

    minPrice :: Integer -- compute the minimum price that has to be paid
    minPrice =
      let
        lovelaceIn = case findOwnInput ctx of  -- findOwnInput gives the input that is being validated, the swap input
            Nothing -> traceError "own input not found"
            Just i  -> lovelaces $ txOutValue $ txInInfoResolved i -- get how many lovelaces are locked in the output/UTXO (of the swap input)
      in
        price lovelaceIn oracleValue' -- compute the minimum price the seller must be paid given the amount lovelaces and the Oracle value, which is the exchange rate (the datum)

    sellerPaid :: Bool
    sellerPaid =
      let
        pricePaid :: Integer  -- valuePaidTo adds up all the values of all outputs that goes to the address pkh which is the address of the seller
        pricePaid =  assetClassValueOf (valuePaidTo info pkh) (oAsset oracle) -- assetClassValueOf provides the oAsset of the oracle;
      in                                                                      --                   gives the number of USD tokens sent to the seller by this transaction
        pricePaid >= minPrice

data Swapping
instance Scripts.ValidatorTypes Swapping where
    type instance DatumType Swapping = PubKeyHash  -- the PubKeyHash of the seller
    type instance RedeemerType Swapping = ()

typedSwapValidator :: Oracle -> Scripts.TypedValidator Swapping
typedSwapValidator oracle = Scripts.mkTypedValidator @Swapping
    ($$(PlutusTx.compile [|| mkSwapValidator ||])
        `PlutusTx.applyCode` PlutusTx.liftCode oracle
        `PlutusTx.applyCode` PlutusTx.liftCode (oracleAddress oracle))  -- oracleAddress can be used here but not inside the mkSwapValidator 
    $$(PlutusTx.compile [|| wrap ||])
  where
    wrap = Scripts.wrapValidator @PubKeyHash @()

swapValidator :: Oracle -> Validator
swapValidator = Scripts.validatorScript . typedSwapValidator

swapAddress :: Oracle -> Ledger.Address
swapAddress = scriptAddress . swapValidator

-- offerSwap contract is for the seller
offerSwap :: forall w s. Oracle -> Integer -> Contract w s Text ()
offerSwap oracle amt = do  -- takes 2 parameters; the oracle and the amount that the seller wants to offer
    pkh <- pubKeyHash <$> Contract.ownPubKey                              -- We want a transaction that locks an amount of lovelaces in a swap contract
    let tx = Constraints.mustPayToTheScript pkh $ Ada.lovelaceValueOf amt  -- Just need one constraint that takes the datum (the PubKeyHash of the seller) and the amount
    ledgerTx <- submitTxConstraints (typedSwapValidator oracle) tx
    awaitTxConfirmed $ txId ledgerTx
    logInfo @String $ "offered " ++ show amt ++ " lovelace for swap"

-- Helper function that finds all swaps that satisfy a specific predicate; gives a list of all UTXOs at sit at the swap address
-- TxOutRef = reference of the UTXO, TxOutTx = the UTXO itself, PubKeyHash = this public key hash is the datum of the UTXO
findSwaps :: Oracle -> (PubKeyHash -> Bool) -> Contract w s Text [(TxOutRef, TxOutTx, PubKeyHash)]
findSwaps oracle p = do
    utxos <- utxoAt $ swapAddress oracle -- gives all UTXOs sitting at the swap address
    return $ mapMaybe g $ Map.toList utxos -- mapMaybe takes a function from a to Maybe b, takes a list of a's, and returns a list of b's
  where                                    -- 
    f :: TxOutTx -> Maybe PubKeyHash  -- takes a UTXO and returns a Maybe PubKeyHash
    f o = do
        dh        <- txOutDatumHash $ txOutTxOut o  -- lookup the Datum hash of the UTXO; txOutTxOut returns TxOut given TxOutTx; It's a Maybe Monad hence could return Nothing
        (Datum d) <- Map.lookup dh $ txData $ txOutTxTx o  -- Use Datum hash to lookup the corresponding Datum in TxData; Datum d is of type Data
        PlutusTx.fromBuiltinData d  -- deserialize from Data to PubKeyHash

    g :: (TxOutRef, TxOutTx) -> Maybe (TxOutRef, TxOutTx, PubKeyHash) -- takes a pair key-value of a UTXO, and returns a Maybe Triple key-value and PubKeyHash
    g (oref, o) = do
        pkh <- f o     -- get the PubKeyHash if it's there (this is the Datum)
        guard $ p pkh  -- guard takes a boolean and fails of it's False and continues if it's True; p predicate takes a PubKeyHash and returns True it's there or else it returns False
        return (oref, o, pkh) -- if guard was satisfied, return the reference to the UTXO, the UTXO itself, and the corresponding PubKeyHash

-- The contract retrieveSwaps is for the seller to retrieve a swap offer
retrieveSwaps :: Oracle -> Contract w s Text ()
retrieveSwaps oracle = do
    pkh <- pubKeyHash <$> ownPubKey -- get the owner's public key hash
    xs  <- findSwaps oracle (== pkh) -- as predicate, check the swaps that belong to the owner; (== pkh) is the predicate function passed to findSwaps
    case xs of  -- xs will have the swap that belong to the owner
        [] -> logInfo @String "no swaps found"
        _  -> do  -- if it's not an empty list, construct a transaction that retrieves them 
            let lookups = Constraints.unspentOutputs (Map.fromList [(oref, o) | (oref, o, _) <- xs]) <> -- must provide all UTXO for tx to work; Map.fromList turns list of key-value pairs into an action map
                          Constraints.otherScript (swapValidator oracle) -- must include validator script in the transaction to consume those those script outputs
                tx      = mconcat [Constraints.mustSpendScriptOutput oref $ Redeemer $ PlutusTx.toBuiltinData () | (oref, _, _) <- xs]  -- combine list of constraints to spend each UTXO
            ledgerTx <- submitTxConstraintsWith @Swapping lookups tx
            awaitTxConfirmed $ txId ledgerTx
            logInfo @String $ "retrieved " ++ show (length xs) ++ " swap(s)"

-- useSwap is to use a swap
useSwap :: forall w s. Oracle -> Contract w s Text ()
useSwap oracle = do
    funds <- ownFunds  -- ownFunds checks own wallets and adds up all funds available owned; ownFunds comes fron Funds.hs module
    let amt = assetClassValueOf funds $ oAsset oracle   -- check how many USD tokens are available to pay for the swap
    logInfo @String $ "available assets: " ++ show amt  -- log as information the amount of tokens available in wallet

    m <- findOracle oracle  -- findOracle finds the UTXO that contains the oracle and the value; findOracle comes from Core.hs module
    case m of
        Nothing           -> logInfo @String "oracle not found"
        Just (oref, o, x) -> do  -- x is the exchange rate which was the value of the Datum of the oracle UTXO
            logInfo @String $ "found oracle, exchange rate " ++ show x
            pkh   <- pubKeyHash <$> Contract.ownPubKey
            swaps <- findSwaps oracle (/= pkh)  -- look for all swap where not owned by the user
            case find (f amt x) swaps of -- find tries to find a swap that user can afford; find is helper function part of the haskell prelude (Data.Lists)
                Nothing                -> logInfo @String "no suitable swap found"
                Just (oref', o', pkh') -> do  -- if a swap was found that can afforded, the first one is used (Realistically would be to specify the amount to swap)
                    let v       = txOutValue (txOutTxOut o) <> lovelaceValueOf (oFee oracle) -- Need to add fees to the value in the oracle
                        p       = assetClassValue (oAsset oracle) $ price (lovelaces $ txOutValue $ txOutTxOut o') x -- p is the price to pay in USD token
                        lookups = Constraints.otherScript (swapValidator oracle)                     <> -- must provide the validator of the swap contract
                                  Constraints.otherScript (oracleValidator oracle)                   <> -- must provide the validator of the oracle contract
                                  Constraints.unspentOutputs (Map.fromList [(oref, o), (oref', o')])  -- must provide UTXOs to consume, for the oracle and the swap 
                        tx      = Constraints.mustSpendScriptOutput oref  (Redeemer $ PlutusTx.toBuiltinData Use) <> -- must use the oracle UTXO as input; use the Use Redeemer
                                  Constraints.mustSpendScriptOutput oref' (Redeemer $ PlutusTx.toBuiltinData ())  <> -- consume the swap output as an input
                                  Constraints.mustPayToOtherScript             -- must pay to the oracle the value that was compute before for v
                                    (validatorHash $ oracleValidator oracle)   -- provide the oracle hash
                                    (Datum $ PlutusTx.toBuiltinData x)         -- use the existing Datum; must not change the exchange rate when usign it
                                    v                                   <>     -- this was the exiting value in the oracle and the fee that needs to be paid
                                  Constraints.mustPayToPubKey pkh' p           -- must pay the seller of the lovelace
                    ledgerTx <- submitTxConstraintsWith @Swapping lookups tx   -- submit it
                    awaitTxConfirmed $ txId ledgerTx
                    logInfo @String $ "made swap with price " ++ show (Value.flattenValue p)
  where
    getPrice :: Integer -> TxOutTx -> Integer -- takes exhange rate and an output
    getPrice x o = price (lovelaces $ txOutValue $ txOutTxOut o) x

    f :: Integer -> Integer -> (TxOutRef, TxOutTx, PubKeyHash) -> Bool -- returns whether the swap is suitable or not
    f amt x (_, o, _) = getPrice x o <= amt -- check price is lower than tokens owned; amt is the amount of USD tokens available in user's wallet to pay for the swap

type SwapSchema =         -- schema defined with 4 endpoints
            Endpoint "offer"    Integer   -- to offer a swap with an amount of ADA in lovelaces
        .\/ Endpoint "retrieve" ()        -- to retrieve all swaps
        .\/ Endpoint "use"      ()        -- to do a swap 
        .\/ Endpoint "funds"    ()        -- provides currently available funds

-- `select` operator  
-- contract when there are several endpoints, it waits until one is picked and executes accordingly
swap :: Oracle -> Contract (Last Value) SwapSchema Text ()
swap oracle = (offer `select` retrieve `select` use `select` funds) >> swap oracle  -- after an endpoint is selected and executed, in a sequence it gets recursively called again
  where
    offer :: Contract (Last Value) SwapSchema Text ()
    offer = h $ do
        amt <- endpoint @"offer"  -- if this get's called, it pauses here until an integer amount is provided
        offerSwap oracle amt  -- call the offerSwap oracle amount contract that was defined

    retrieve :: Contract (Last Value) SwapSchema Text ()
    retrieve = h $ do
        endpoint @"retrieve"
        retrieveSwaps oracle

    use :: Contract (Last Value) SwapSchema Text ()
    use = h $ do
        endpoint @"use"
        useSwap oracle

    funds :: Contract (Last Value) SwapSchema Text ()
    funds = h $ do
        endpoint @"funds" -- gets blocked until funds endpointis invoked
        v <- ownFunds  -- ownFunds module provides the funds owned
        tell $ Last $ Just v  -- tell allows passing information outside a contract to the outside world

    h :: Contract (Last Value) SwapSchema Text () -> Contract (Last Value) SwapSchema Text () -- an error handler
    h = handleError logError
