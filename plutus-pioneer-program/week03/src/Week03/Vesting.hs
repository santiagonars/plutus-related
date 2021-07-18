{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE DeriveAnyClass      #-} -- additional GHC extension
{-# LANGUAGE DeriveGeneric       #-} -- additional GHC extension
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE TypeFamilies        #-}
{-# LANGUAGE TypeOperators       #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

module Week03.Vesting where

import           Control.Monad        hiding (fmap)
import           Data.Aeson           (ToJSON, FromJSON)
import           Data.Map             as Map
import           Data.Text            (Text)
import           Data.Void            (Void)
import           GHC.Generics         (Generic)
import           Plutus.Contract
import           PlutusTx             (Data (..))
import qualified PlutusTx
import           PlutusTx.Prelude     hiding (Semigroup(..), unless)
import           Ledger               hiding (singleton)
import           Ledger.Constraints   as Constraints
import qualified Ledger.Typed.Scripts as Scripts
import           Ledger.Ada           as Ada
import           Playground.Contract  (printJson, printSchemas, ensureKnownCurrencies, stage, ToSchema)
import           Playground.TH        (mkKnownCurrencies, mkSchemaDefinitions)
import           Playground.Types     (KnownCurrency (..))
import           Prelude              (IO, Semigroup (..), Show (..), String)
import           Text.Printf          (printf)

data VestingDatum = VestingDatum
    { beneficiary :: PubKeyHash
    , deadline    :: POSIXTime
    } deriving Show  -- Show might be useful for debugging

PlutusTx.unstableMakeIsData ''VestingDatum

{-# INLINABLE mkValidator #-}
mkValidator :: VestingDatum -> () -> ScriptContext -> Bool  -- need to check 2 conditions; must define these
mkValidator dat () ctx = traceIfFalse "beneficiary's signature missing" signedByBeneficiary && -- signed by the beneficiary
                         traceIfFalse "deadline not reached" deadlineReached  -- current time is after the deadline  
  where
    info :: TxInfo
    info = scriptContextTxInfo ctx -- to get the TxInfo from the script, must use scriptContextTxInfo

    signedByBeneficiary :: Bool
    signedByBeneficiary = txSignedBy info $ beneficiary dat -- takes the TxInfo and a PubKeyHash and tells whether it has been signed

    deadlineReached :: Bool
    deadlineReached = contains (from $ deadline dat) $ txInfoValidRange info -- check that the interval of type POSIXTimeRange is after deadline

data Vesting
instance Scripts.ValidatorTypes Vesting where
    type instance DatumType Vesting = VestingDatum
    type instance RedeemerType Vesting = ()

typedValidator :: Scripts.TypedValidator Vesting
typedValidator = Scripts.mkTypedValidator @Vesting
    $$(PlutusTx.compile [|| mkValidator ||])
    $$(PlutusTx.compile [|| wrap ||])
  where
    wrap = Scripts.wrapValidator @VestingDatum @()

validator :: Validator
validator = Scripts.validatorScript typedValidator

valHash :: Ledger.ValidatorHash
valHash = Scripts.validatorHash typedValidator

scrAddress :: Ledger.Address
scrAddress = scriptAddress validator

data GiveParams = GiveParams
    { gpBeneficiary :: !PubKeyHash
    , gpDeadline    :: !POSIXTime
    , gpAmount      :: !Integer
    } deriving (Generic, ToJSON, FromJSON, ToSchema)

type VestingSchema =
            Endpoint "give" GiveParams
        .\/ Endpoint "grab" () -- doesn't need any parameters because the beneficiary will look for utxos at the vesting address and check if he/she is the beneficiary

give :: AsContractError e => GiveParams -> Contract w s e ()
give gp = do
    let dat = VestingDatum
                { beneficiary = gpBeneficiary gp -- from the GiveParams
                , deadline    = gpDeadline gp -- from the GiveParams
                }
        tx  = mustPayToTheScript dat $ Ada.lovelaceValueOf $ gpAmount gp -- need a constraint to create a transaction with the output at the script address; must provide Datum defined and value  
    ledgerTx <- submitTxConstraints typedValidator tx
    void $ awaitTxConfirmed $ txId ledgerTx
    logInfo @String $ printf "made a gift of %d lovelace to %s with deadline %s"
        (gpAmount gp)
        (show $ gpBeneficiary gp)
        (show $ gpDeadline gp)

grab :: forall w s e. AsContractError e => Contract w s e ()
grab = do
    now   <- currentTime
    pkh   <- pubKeyHash <$> ownPubKey -- lookup up own's public key and compute it's hash
    utxos <- Map.filter (isSuitable pkh now) <$> utxoAt scrAddress -- look at all utxos at the script address & filter for only utxos with match beneficiary and deadline has been reached
    if Map.null utxos
        then logInfo @String $ "no gifts available"
        else do -- if there is at least one utxo, then contruct a transaction that consumes them all as inputs
            let orefs   = fst <$> Map.toList utxos
                lookups = Constraints.unspentOutputs utxos  <>  -- need to provide the UTXOs
                          Constraints.otherScript validator     -- need to provide the validator script
                tx :: TxConstraints Void Void
                tx      = mconcat [mustSpendScriptOutput oref $ Redeemer $ PlutusTx.toData () | oref <- orefs] <>  -- goers through list of utxos to spend
                          mustValidateIn (from now) -- interval provided must be after the deadline; 
            ledgerTx <- submitTxConstraintsWith @Void lookups tx
            void $ awaitTxConfirmed $ txId ledgerTx
            logInfo @String $ "collected gifts"
  where
    isSuitable :: PubKeyHash -> POSIXTime -> TxOutTx -> Bool
    isSuitable pkh now o = case txOutDatumHash $ txOutTxOut o of  -- first need to check the DatumHash and if found it's the h
        Nothing -> False
        Just h  -> case Map.lookup h $ txData $ txOutTxTx o of  -- try look up the corresponding Datum; we can find it because it was optionally added to the script in the give function
            Nothing        -> False
            Just (Datum e) -> case PlutusTx.fromData e of -- must deserialize the Datum; it is of the Data type but we need it in Vesting type
                Nothing -> False
                Just d  -> beneficiary d == pkh && deadline d <= now -- check the benefiary of the datum is myself and that the current time has passed the deadline

endpoints :: Contract () VestingSchema Text ()
endpoints = (give' `select` grab') >> endpoints
  where
    give' = endpoint @"give" >>= give
    grab' = endpoint @"grab" >>  grab

mkSchemaDefinitions ''VestingSchema

mkKnownCurrencies []
