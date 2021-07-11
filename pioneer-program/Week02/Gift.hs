{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE TypeFamilies        #-}
{-# LANGUAGE TypeOperators       #-}

module Week02.Gift where

import           Control.Monad       hiding (fmap)
import           Data.Map            as Map
import           Data.Text           (Text)
import           Data.Void           (Void)
import           Plutus.Contract
import           PlutusTx            (Data (..))
import qualified PlutusTx
import           PlutusTx.Prelude    hiding (Semigroup(..), unless)
import           Ledger              hiding (singleton)
import           Ledger.Constraints  as Constraints
import qualified Ledger.Scripts      as Scripts
import           Ledger.Ada          as Ada
import           Playground.Contract (printJson, printSchemas, ensureKnownCurrencies, stage)
import           Playground.TH       (mkKnownCurrencies, mkSchemaDefinitions)
import           Playground.Types    (KnownCurrency (..))
import           Prelude             (IO, Semigroup (..), String)
import           Text.Printf         (printf)

{-# OPTIONS_GHC -fno-warn-unused-imports #-}
-- pragma allows the inside of the compiler to inline the definition of of mkValidator inside the oxford brackets
{-# INLINABLE mkValidator #-}
mkValidator :: Data -> Data -> Data -> () 
mkValidator _ _ _ = () -- the main validator function
-- creates a validator script that can be compiled on-chain as plutus-core
validator :: Validator
validator = mkValidatorScript $$(PlutusTx.compile [|| mkValidator ||])
-- validatorHash provides the hash of the validator
valHash :: Ledger.ValidatorHash
valHash = Scripts.validatorHash validator
-- scriptAddress turns the validator into an address
scrAddress :: Ledger.Address
scrAddress = scriptAddress validator

-- specify the of-chain endpoints for the transaction
type GiftSchema =
            Endpoint "give" Integer
        .\/ Endpoint "grab" ()

give :: AsContractError e => Integer -> Contract w s e ()
give amount = do
    let tx = mustPayToOtherScript valHash (Datum $ Constr 0 []) $ Ada.lovelaceValueOf amount -- create transaction
    ledgerTx <- submitTx tx -- submit transation
    void $ awaitTxConfirmed $ txId ledgerTx -- wait to confirm transaction
    logInfo @String $ printf "made a gift of %d lovelace" amount -- add to log

grab :: forall w s e. AsContractError e => Contract w s e ()
grab = do
    utxos <- utxoAt scrAddress -- find all UTXOs at the script address
    let orefs   = fst <$> Map.toList utxos -- get refences to the UTXOs; lookups tell the wallet how to construct the transaction
        lookups = Constraints.unspentOutputs utxos      <>   -- where to find all the utxos
                  Constraints.otherScript validator          -- spending transaction has to provide the validator
        tx :: TxConstraints Void Void
        tx      = mconcat [mustSpendScriptOutput oref $ Redeemer $ I 17 | oref <- orefs] -- constructs a transaction to consume all the script outputs
    ledgerTx <- submitTxConstraintsWith @Void lookups tx -- allows wallet to construct transaction with the necessary information
    void $ awaitTxConfirmed $ txId ledgerTx
    logInfo @String $ "collected gifts"

endpoints :: Contract () GiftSchema Text ()
endpoints = (give' `select` grab') >> endpoints -- `select` offers the user choices; give prime (give') or grab prime (grab')
  where
    give' = endpoint @"give" >>= give --blocks execution and waits for user to provide an integer
    grab' = endpoint @"grab" >>  grab

mkSchemaDefinitions ''GiftSchema -- generate the schema

mkKnownCurrencies [] -- so there is currencies in the playground
