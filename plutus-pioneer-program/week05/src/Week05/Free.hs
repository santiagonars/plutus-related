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

module Week05.Free where

import           Control.Monad          hiding (fmap)
import           Data.Aeson             (ToJSON, FromJSON)
import           Data.Text              (Text)
import           Data.Void              (Void)
import           GHC.Generics           (Generic)
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
import           Prelude                (IO, Show (..), String)
import           Text.Printf            (printf)
import           Wallet.Emulator.Wallet

-- Haskell fuction for the validator that represents the policy; Only takes 2 arguments- the redeemer and the context
{-# INLINABLE mkPolicy #-}  -- adding inlinable pragma to wrap policy in the oxord brackets to be able compile in Plutus
mkPolicy :: () -> ScriptContext -> Bool -- low level Plutus uses BuildinData types or Data types all arguments
mkPolicy () _ = True  -- represents a minting policy that allows all minting and burning for the currency symbol given by this policy without restrictions
                      -- currency symbol represents the hash of the script

-- Compile policy to a Plutus Script; need to call policy of type MintingPolicy
policy :: Scripts.MintingPolicy               -- wrapMintingPolicy converts it to untyped version that use BuiltinData for args and retuns a ()
policy = mkMintingPolicyScript $$(PlutusTx.compile [|| Scripts.wrapMintingPolicy mkPolicy ||])  

curSymbol :: CurrencySymbol
curSymbol = scriptCurrencySymbol policy  -- get currency symbol by applying the function criptCurrencySymbol to the policy

data MintParams = MintParams
    { mpTokenName :: !TokenName
    , mpAmount    :: !Integer  -- if the amount is positive, it will forger/mint, if it's negative, it wil burn
    } deriving (Generic, ToJSON, FromJSON, ToSchema)

type FreeSchema = Endpoint "mint" MintParams

mint :: MintParams -> Contract w FreeSchema Text ()
mint mp = do
    let val     = Value.singleton curSymbol (mpTokenName mp) (mpAmount mp) -- compute Value to forge/burn
        lookups = Constraints.mintingPolicy policy -- specify minting policy; transaction has to contain the policy for validation to be successful
        tx      = Constraints.mustMintValue val  -- constraint that this transaction must mint a value that was computed; redeemer is not added because it's a ()
    ledgerTx <- submitTxConstraintsWith @Void lookups tx -- submit will automatically find an input in wallet to cover fees and 
    void $ awaitTxConfirmed $ txId ledgerTx              -- transfer minted value to wallet (if positive) or attempt to find sufficient tokens in user's wallet that can be burn                                     
    Contract.logInfo @String $ printf "forged %s" (show val) --log message that this value was forged

endpoints :: Contract () FreeSchema Text ()  -- define the ednpoints
endpoints = mint' >> endpoints -- call mint and then recursive call endpoints again
  where
    mint' = endpoint @"mint" >>= mint -- uses endpoint function to actually expose the endpoint mint usign the bind into the mint function

mkSchemaDefinitions ''FreeSchema

mkKnownCurrencies []

-- Can just run emulatorTrace on the console, hence no need to test it on the playground
test :: IO ()
test = runEmulatorTraceIO $ do
    let tn = "ABC" -- define TokenName
    h1 <- activateContractWallet (Wallet 1) endpoints -- run endpoints contract on wallet 1 & 2
    h2 <- activateContractWallet (Wallet 2) endpoints
    callEndpoint @"mint" h1 $ MintParams -- call endpoint mint on wallet 1
        { mpTokenName = tn   -- using tn which is "ABC"
        , mpAmount    = 555  -- mint 555 coins of "ABC" token
        }
    callEndpoint @"mint" h2 $ MintParams -- call endpoint mint on wallet 1
        { mpTokenName = tn
        , mpAmount    = 444 -- mint 555 coins of "ABC" token
        }
    void $ Emulator.waitNSlots 1
    callEndpoint @"mint" h1 $ MintParams
        { mpTokenName = tn
        , mpAmount    = -222  -- burn 222 "ABC" tokens
        }
    void $ Emulator.waitNSlots 1
