{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE DeriveAnyClass      #-}
{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE TypeFamilies        #-}
{-# LANGUAGE TypeOperators       #-}

module Week05.NFT where

import           Control.Monad          hiding (fmap)
import qualified Data.Map               as Map
import           Data.Text              (Text)
import           Data.Void              (Void)
import           Plutus.Contract        as Contract
import           Plutus.Trace.Emulator  as Emulator
import qualified PlutusTx
import           PlutusTx.Prelude       hiding (Semigroup(..), unless)
import           Ledger                 hiding (mint, singleton)
import           Ledger.Constraints     as Constraints
import qualified Ledger.Typed.Scripts   as Scripts
import           Ledger.Value           as Value
import           Playground.Contract    (printJson, printSchemas, ensureKnownCurrencies, stage, ToSchema)
import           Playground.TH          (mkKnownCurrencies, mkSchemaDefinitions)
import           Playground.Types       (KnownCurrency (..))
import           Prelude                (IO, Semigroup (..), Show (..), String)
import           Text.Printf            (printf)
import           Wallet.Emulator.Wallet

-- mkPolicy needs the reference of the UTXO and the token name
-- Alternatively the TxOutRef and TokenName can be passed as one parameter
{-# INLINABLE mkPolicy #-}
mkPolicy :: TxOutRef -> TokenName -> () -> ScriptContext -> Bool
mkPolicy oref tn () ctx = traceIfFalse "UTxO not consumed"   hasUTxO           &&
                          traceIfFalse "wrong amount minted" checkMintedAmount
  where
    info :: TxInfo
    info = scriptContextTxInfo ctx

    hasUTxO :: Bool -- any is a standard haskell function checks whether any that is at least one element of a list satisfies a condition; first is condtion then the list
    hasUTxO = any (\i -> txInInfoOutRef i == oref) $ txInfoInputs info -- checks if there are any inputs in trasanction info equals the given output / utxo (oref) 

    checkMintedAmount :: Bool  -- txInfoForge gives which value has been forged; flattenValue turns the nested map into a flat list of triples
    checkMintedAmount = case flattenValue (txInfoForge info) of    -- check currency symbol is the one being validated, that token name is the one given, and the amount is equal to 1
        [(cs, tn', amt)] -> cs  == ownCurrencySymbol ctx && tn' == tn && amt == 1  -- ownCurrencySymbol gives access to the currency symbol that is being currently checked
        --[(_, tn', amt)] -> cs  == tn' == tn && amt == 1  -- simplied version: since we know the value been forge has exactly only one currency symbol, it can be ignored 
        _                -> False                                                                

-- Compile the mkPolicy function to a policy
policy :: TxOutRef -> TokenName -> Scripts.MintingPolicy
policy oref tn = mkMintingPolicyScript $
    $$(PlutusTx.compile [|| \oref' tn' -> Scripts.wrapMintingPolicy $ mkPolicy oref' tn' ||])
    `PlutusTx.applyCode`
    PlutusTx.liftCode oref -- apply a lift for each parameter
    `PlutusTx.applyCode`
    PlutusTx.liftCode tn

-- Compute the currency symbol with the policy
curSymbol :: TxOutRef -> TokenName -> CurrencySymbol
curSymbol oref tn = scriptCurrencySymbol $ policy oref tn

type NFTSchema = Endpoint "mint" TokenName -- Define the schema with one endpoint called mint that takes the token name

mint :: TokenName -> Contract w NFTSchema Text ()
mint tn = do
    pk    <- Contract.ownPubKey  -- look up own public key; need to find a suitable UTXO to consume
    utxos <- utxoAt (pubKeyAddress pk) -- pubKeyAddress takes a public key and computes the corresponding address; utxoAt finds all UTXOs at a wallet address
    case Map.keys utxos of  
        []       -> Contract.logError @String "no utxo found"  -- if there is no UTXO
        oref : _ -> do     -- *** For simplicity, only the first UTXO is used in the policy as parameter for the NFT
            let val     = Value.singleton (curSymbol oref tn) tn 1  -- compute value to forge
                lookups = Constraints.mintingPolicy (policy oref tn) <> Constraints.unspentOutputs utxos  -- lookup constraints; can also just provide the one utxos oref instead all of them 
                tx      = Constraints.mustMintValue val <> Constraints.mustSpendPubKeyOutput oref -- transaction constraints
            ledgerTx <- submitTxConstraintsWith @Void lookups tx
            void $ awaitTxConfirmed $ txId ledgerTx
            Contract.logInfo @String $ printf "forged %s" (show val)

endpoints :: Contract () NFTSchema Text ()
endpoints = mint' >> endpoints
  where
    mint' = endpoint @"mint" >>= mint

mkSchemaDefinitions ''NFTSchema

mkKnownCurrencies []  -- boilerplate for playgroun

test :: IO () -- a simple emulatorTrace to test
test = runEmulatorTraceIO $ do
    let tn = "ABC"
    h1 <- activateContractWallet (Wallet 1) endpoints
    h2 <- activateContractWallet (Wallet 2) endpoints
    callEndpoint @"mint" h1 tn
    callEndpoint @"mint" h2 tn
    void $ Emulator.waitNSlots 1

-- Both wallets end with 1 token each that share the same name but a different currency symbol
-- Currency symbols are different because each wallet used a different UTXO as parameter for the currency symbol for the policy
--                                both are NFTs but they are different because the currency symbol is different
-- The expected log message also appear
