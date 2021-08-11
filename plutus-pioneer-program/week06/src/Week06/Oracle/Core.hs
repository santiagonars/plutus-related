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

module Week06.Oracle.Core
    ( Oracle (..)
    , OracleRedeemer (..)
    , oracleTokenName
    , oracleValue
    , oracleAsset
    , typedOracleValidator
    , oracleValidator
    , oracleAddress
    , OracleSchema
    , OracleParams (..)
    , runOracle
    , findOracle
    ) where

import           Control.Monad             hiding (fmap)
import           Data.Aeson                (FromJSON, ToJSON)
import qualified Data.Map                  as Map
import           Data.Monoid               (Last (..))
import           Data.Text                 (Text, pack)
import           GHC.Generics              (Generic)
import           Plutus.Contract           as Contract
import qualified PlutusTx
import           PlutusTx.Prelude          hiding (Semigroup(..), unless)
import           Ledger                    hiding (singleton)
import           Ledger.Constraints        as Constraints
import qualified Ledger.Typed.Scripts      as Scripts
import           Ledger.Value              as Value
import           Ledger.Ada                as Ada
import           Plutus.Contracts.Currency as Currency
import           Prelude                   (Semigroup (..), Show (..), String)
import qualified Prelude

data Oracle = Oracle
    { oSymbol   :: !CurrencySymbol   -- Currency symbol of the NFT; The NFT to uniquely identify the oracle
    , oOperator :: !PubKeyHash       -- The onwer (operator) of the oracle that caa make updates
    , oFee      :: !Integer          -- The fees in lovelaves that are due each time someone uses the oracle
    , oAsset    :: !AssetClass       -- Identifies the target of the oracle (Could be asset class to represent some USD token)
    } deriving (Show, Generic, FromJSON, ToJSON, Prelude.Eq, Prelude.Ord)

PlutusTx.makeLift ''Oracle  -- type class for the Oracle to be serializable and liftable

data OracleRedeemer = Update | Use  -- define a data type for the redeemer to support two operations
    deriving Show

PlutusTx.unstableMakeIsData ''OracleRedeemer  -- use template Haskell to use isData for the redeemer type

{-# INLINABLE oracleTokenName #-}
oracleTokenName :: TokenName -- helper function to assign a token name; use an empty string for the NFT
oracleTokenName = TokenName emptyByteString

{-# INLINABLE oracleAsset #-} 
oracleAsset :: Oracle -> AssetClass   -- helper function to identify the NFT asset class to uniquely identify the UTXO with the oracle value
oracleAsset oracle = AssetClass (oSymbol oracle, oracleTokenName)

{-# INLINABLE oracleValue #-}  -- helper function to get the value of the UTXO that holds ther oracle
oracleValue :: TxOut -> (DatumHash -> Maybe Datum) -> Maybe Integer      -- TxOut will be the output of the UTXO that holds the oracle 
oracleValue o f = do                                                     -- Use the Datum of that oracle to turn it into an Integer
    dh      <- txOutDatum o -- Datum could be missing                    -- Uses an Integer because of json conversion issues
    Datum d <- f dh -- Datum is a builtin wrapper around Datum data      -- Basically the Integer was multiplied (exchange rate) by 1,000,000 and then rounded
    PlutusTx.fromBuiltinData d  -- Maybe turn the d into an Integer; Datum could be there but as another data type

--  This is the oracle validator which represents the business logic of the oracle
{-# INLINABLE mkOracleValidator #-} -- Takes the parameter data as Oracle, the datum as an Interger, reddemer as OracleRedeemer, and the context
mkOracleValidator :: Oracle -> Integer -> OracleRedeemer -> ScriptContext -> Bool
mkOracleValidator oracle x r ctx =
    traceIfFalse "token missing from input"  inputHasToken  && -- For both cases need to check that the input holds the NFT 
    traceIfFalse "token missing from output" outputHasToken && --     and that there is an output that again holds the NFT
    case r of  -- distinguishe the two cases
        Update -> traceIfFalse "operator signature missing" (txSignedBy info $ oOperator oracle) &&  -- check that operator signed the transaction
                  traceIfFalse "invalid output datum"       validOutputDatum                         -- check that it carries valid Integer as output Datum  
        Use    -> traceIfFalse "oracle value changed"       (outputDatum == Just x)              &&  -- do not allow the Datum to change
                  traceIfFalse "fees not paid"              feesPaid                                 -- check fees are paid
  where
    info :: TxInfo -- take the context and extract the transaction info from it
    info = scriptContextTxInfo ctx

    ownInput :: TxOut  -- provides the oracle output that is to be consumed
    ownInput = case findOwnInput ctx of -- The findOwnInput plutus function provides the input given the context
        Nothing -> traceError "oracle input missing"
        Just i  -> txInInfoResolved i -- when there is an input or type TxIn or TxInput, txInInfoResolved provides the corresponding TxOut
    -- Ledger.Value module has a the function assetClassValueOf
    inputHasToken :: Bool  -- assetClassValueOf takes a value and an asset class to return an integer of how many coins of that asset class are contained in the value
    inputHasToken = assetClassValueOf (txOutValue ownInput) (oracleAsset oracle) == 1

    ownOutput :: TxOut  -- getContinuingOutputs function gets the context and returns a list of all the outputs that go to the same script address presently been validated
    ownOutput = case getContinuingOutputs ctx of   -- summary: will given all the outputs of the transaction that again go to the oracle address
        [o] -> o  -- if getContinuingOutputs returns a list with one element, it returns that element
        _   -> traceError "expected exactly one oracle output" -- if list has zero or more outputs going to the oracle address, produce an error

    outputHasToken :: Bool  -- checks that there is an oracle output that also holds the NFT
    outputHasToken = assetClassValueOf (txOutValue ownOutput) (oracleAsset oracle) == 1

    outputDatum :: Maybe Integer -- x is the old oracle value, but now we want the datum value attached to the oracle output, given by ownOutput
    outputDatum = oracleValue ownOutput (`findDatum` info) -- `findDatum` takes the info and the datum hash and tries to find the corresponding Just datum value of the oracle

    validOutputDatum :: Bool
    validOutputDatum = isJust outputDatum  -- make sure that the return Datum value is a Just and not Nothing

    feesPaid :: Bool
    feesPaid =
      let
        inVal  = txOutValue ownInput   -- value that was attached the oracle input
        outVal = txOutValue ownOutput  -- value that is attached to own output
      in    -- output value should be as large as the input plus the fees
        outVal `geq` (inVal <> Ada.lovelaceValueOf (oFee oracle)) -- output value should be `greater or equal` than the input value plus the fees

data Oracling  -- combine redeemer type and the datum type
instance Scripts.ValidatorTypes Oracling where
    type instance DatumType Oracling = Integer
    type instance RedeemerType Oracling = OracleRedeemer

typedOracleValidator :: Oracle -> Scripts.TypedValidator Oracling
typedOracleValidator oracle = Scripts.mkTypedValidator @Oracling
    ($$(PlutusTx.compile [|| mkOracleValidator ||]) `PlutusTx.applyCode` PlutusTx.liftCode oracle)
    $$(PlutusTx.compile [|| wrap ||])
  where
    wrap = Scripts.wrapValidator @Integer @OracleRedeemer

oracleValidator :: Oracle -> Validator -- turn the typedOracleValidator into a Validator
oracleValidator = Scripts.validatorScript . typedOracleValidator

oracleAddress :: Oracle -> Ledger.Address -- turn the Validator into an address; provides the script oracle address
oracleAddress = scriptAddress . oracleValidator

-- Off-chain section mainly needs endpoints to start the oracle and update it, not to use it (That is not the responsibility of the author of the oracle contract)
-- Parameters are needed to start the oracle
data OracleParams = OracleParams
    { opFees   :: !Integer         -- fees to charge
    , opSymbol :: !CurrencySymbol  -- Currency symbol of asset to track (Example: The USD)
    , opToken  :: !TokenName       -- Token name of asset to track (Example: The USD)
    } deriving (Show, Generic, FromJSON, ToJSON)

-- The NFT is minted in this startOracle function; the initial value is no provided here
startOracle :: forall w s. OracleParams -> Contract w s Text Oracle
startOracle op = do
    pkh <- pubKeyHash <$> Contract.ownPubKey  -- mapError with pack compose with show turns a contracts CurrencyError messages to Text error messages
    osc <- mapError (pack . show) (mintContract pkh [(oracleTokenName, 1)] :: Contract w s CurrencyError OneShotCurrency) -- mintContract mints tokens given a PubKeyHash
    let cs     = Currency.currencySymbol osc  -- currencySymbol is a function takes a one shot currency and returns a currency symbol (part of OneShotCurrency)
        oracle = Oracle   -- fill in the Oracle type
            { oSymbol   = cs
            , oOperator = pkh
            , oFee      = opFees op
            , oAsset    = AssetClass (opSymbol op, opToken op)
            }
    logInfo @String $ "started oracle " ++ show oracle
    return oracle  -- return type is of type Oracle

-- For updateOracle, need to deal with 2 cases; one where Oracle value already is already present or the Oracle was just started but and there is not UTXO yet
updateOracle :: forall w s. Oracle -> Integer -> Contract w s Text ()  -- takes the Oracle and the Integer is the new value to update the Oracle to
updateOracle oracle x = do
    m <- findOracle oracle  -- try to find the oracle; findOracle returns a triple with the UTXO identifier, the UTXO, and the Datum value
    let c = Constraints.mustPayToTheScript x $ assetClassValue (oracleAsset oracle) 1 -- constraint the new transaction must create an an output with new value that pays to the oracle script address
    case m of                                              -- arg1: x is Datum, the value to set the UTXO to; arg2: Value is the NFT we want to attach to the UTXO
        Nothing -> do  -- if no oracle was found, it was just started but initial value was not provided yet; Need to provided output at the oracle address 
            ledgerTx <- submitTxConstraints (typedOracleValidator oracle) c  -- constraint the constructed transaction
            awaitTxConfirmed $ txId ledgerTx
            logInfo @String $ "set initial oracle value to " ++ show x
        Just (oref, o,  _) -> do  -- There is an existing UTXO
            let lookups = Constraints.unspentOutputs (Map.singleton oref o)     <>   -- provides maps of UTXOs to consume; Map.singleton creates a map of one key-value pair
                          Constraints.typedValidatorLookups (typedOracleValidator oracle) <> -- provide oracle instances; to consume the input
                          Constraints.otherScript (oracleValidator oracle) -- provide oracle validator; to pay to the output
                tx      = c <> Constraints.mustSpendScriptOutput oref (Redeemer $ PlutusTx.toBuiltinData Update) -- 
            ledgerTx <- submitTxConstraintsWith @Oracling lookups tx -- @Oracling tells what DatumType and RedeemerType the script is has
            awaitTxConfirmed $ txId ledgerTx
            logInfo @String $ "updated oracle value to " ++ show x

-- helper function for updateOracle to lookup existing Oracle UTXO; will fail if the Oracle was just started
findOracle :: forall w s. Oracle -> Contract w s Text (Maybe (TxOutRef, TxOutTx, Integer)) -- Returns a Just Triple; TxOutRef is the identifier of a UTXO
findOracle oracle = do                                                                     -- TxOutTx is the UTXO itself; Integer is the current exchange rate
    utxos <- Map.filter f <$> utxoAt (oracleAddress oracle) -- get all UTXOs sitting at the oracleAddress; becomes a map from all TxOutRef to TxOutTx in this address
    return $ case Map.toList utxos of  -- convert the map into a list of key-value pairs (there should only be one entry for the UTXO found)
        [(oref, o)] -> do  -- case that exatly one key-value pair was found
            x <- oracleValue (txOutTxOut o) $ \dh -> Map.lookup dh $ txData $ txOutTxTx o  -- txOutTxOut returns TxOut given TxOutTx; txOutTxTx gives the transaction
            return (oref, o, x) -- oref :: TxOutRef, o :: TxOutTx, x :: Integer            -- TxData is map from Datum hashes to Datums; Map.lookup finds Datum (Integer) given Datum hash
        _           -> Nothing  -- all other cases; where nothing is found or more than one UTXO was found 
  where
    f :: TxOutTx -> Bool -- filter out to keep only those entries in the map which return true
    f o = assetClassValueOf (txOutValue $ txOutTxOut o) (oracleAsset oracle) == 1  -- cannot be more than once because NFT can onky be there once

type OracleSchema = Endpoint "update" Integer

-- function combines start and update operations into one contract
runOracle :: OracleParams -> Contract (Last Oracle) OracleSchema Text ()
runOracle op = do
    oracle <- startOracle op   -- startOracle mints ther NFT and returns the oracle parameters
    tell $ Last $ Just oracle  -- tell is used to write the oracle value to allow passing information outside a contract to the outside world; Last provides the last oracle values
    go oracle   -- go loops forever 
  where
    go :: Oracle -> Contract (Last Oracle) OracleSchema Text a
    go oracle = do
        x <- endpoint @"update"  -- blocks at the update endpoint; if someone provides an integer, it will continue
        updateOracle oracle x    -- updateOracle will be called with the oracle value
        go oracle -- loop again
