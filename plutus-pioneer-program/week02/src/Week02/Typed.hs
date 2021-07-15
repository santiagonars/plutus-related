{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE TypeFamilies        #-}
{-# LANGUAGE TypeOperators       #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

module Week02.Typed where

import           Control.Monad        hiding (fmap)
import           Data.Map             as Map
import           Data.Text            (Text)
import           Data.Void            (Void)
import           Plutus.Contract
import           PlutusTx             (Data (..))
import qualified PlutusTx
import           PlutusTx.Prelude     hiding (Semigroup(..), unless)
import           Ledger               hiding (singleton)
import           Ledger.Constraints   as Constraints
import qualified Ledger.Typed.Scripts as Scripts -- contains validatorHash
import           Ledger.Ada           as Ada
import           Playground.Contract  (printJson, printSchemas, ensureKnownCurrencies, stage)
import           Playground.TH        (mkKnownCurrencies, mkSchemaDefinitions)
import           Playground.Types     (KnownCurrency (..))
import           Prelude              (IO, Semigroup (..), String)
import           Text.Printf          (printf)

{-# INLINABLE mkValidator #-}
mkValidator :: () -> Integer -> ScriptContext -> Bool  -- Datum -> Redeemer -> Context -> True or False
-- mkValidator _ r _ = r == 42    -- <=this approach fails if redeemer is not 42 but does not provide an error message
mkValidator _ r _ = traceIfFalse "wrong redeemer" $ r == 42

data Typed -- a dummy variable
instance Scripts.ValidatorTypes Typed where   -- create an instance of the class ValidatorTypes
    type instance DatumType Typed = ()  -- gives a unit type instance for the DatumType; basically allows a pass of the DatumType
    type instance RedeemerType Typed = Integer  -- gives an integer type instance for the RedeemerType; basically allows a pass of the RedeemerType

typedValidator :: Scripts.TypedValidator Typed
typedValidator = Scripts.mkTypedValidator @Typed -- Use mkTypedValidator instead of mkValidatorScript; need to give a type argument of Typed before @
    $$(PlutusTx.compile [|| mkValidator ||]) -- wrap haskell espression in oxford signs
    $$(PlutusTx.compile [|| wrap ||]) -- converts the typedValidator into un untype version
  where
    wrap = Scripts.wrapValidator @() @Integer  -- needs two type arguments, the DatumType and the RedeemerType

validator :: Validator
validator = Scripts.validatorScript typedValidator -- get the validator from a TypeValidator; takes a type Validator and turns it into an untype Validator

valHash :: Ledger.ValidatorHash
valHash = Scripts.validatorHash typedValidator --  validatorHash produces a validator hash from a typedValidator

scrAddress :: Ledger.Address
scrAddress = scriptAddress validator

type GiftSchema =
            Endpoint "give" Integer
        .\/ Endpoint "grab" Integer

give :: AsContractError e => Integer -> Contract w s e ()
give amount = do
    let tx = mustPayToTheScript () $ Ada.lovelaceValueOf amount -- just need to provide the Datum with the the type used in the typedValidator
    ledgerTx <- submitTxConstraints typedValidator tx
    void $ awaitTxConfirmed $ txId ledgerTx
    logInfo @String $ printf "made a gift of %d lovelace" amount

grab :: forall w s e. AsContractError e => Integer -> Contract w s e ()
grab r = do
    utxos <- utxoAt scrAddress
    let orefs   = fst <$> Map.toList utxos
        lookups = Constraints.unspentOutputs utxos      <>
                  Constraints.otherScript validator
        tx :: TxConstraints Void Void
        tx      = mconcat [mustSpendScriptOutput oref $ Redeemer $ I r | oref <- orefs]
    ledgerTx <- submitTxConstraintsWith @Void lookups tx
    void $ awaitTxConfirmed $ txId ledgerTx
    logInfo @String $ "collected gifts"

endpoints :: Contract () GiftSchema Text ()
endpoints = (give' `select` grab') >> endpoints
  where
    give' = endpoint @"give" >>= give
    grab' = endpoint @"grab" >>= grab

mkSchemaDefinitions ''GiftSchema

mkKnownCurrencies []
