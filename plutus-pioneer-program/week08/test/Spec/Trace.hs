{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE NumericUnderscores    #-}    -- this extension allows adding underscores to make it easier to read large numbers
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}

module Spec.Trace
    ( tests
    , runMyTrace
    ) where

import           Control.Lens
import           Control.Monad              hiding (fmap)
import           Control.Monad.Freer.Extras as Extras
import           Data.Default               (Default (..))
import qualified Data.Map                   as Map
import           Data.Monoid                (Last (..))
import           Ledger
import           Ledger.Value
import           Ledger.Ada                 as Ada
import           Plutus.Contract.Test
import           Plutus.Trace.Emulator      as Emulator
import           PlutusTx.Prelude
import           Prelude                    (IO, String, Show (..))
import           Test.Tasty

import           Week08.TokenSale

tests :: TestTree
tests = checkPredicateOptions
    (defaultCheckOptions & emulatorConfig .~ emCfg)  -- these are the default checkOptions; modify emulatorConfig to have the value from emCfg
    "token sale trace"  -- this argument is the name of the test    ** Next chain together 3 trace predicates; walletFundsChange checks that the wallet funds have changed (ignores fees)
    (     walletFundsChange (Wallet 1) (Ada.lovelaceValueOf   10_000_000  <> assetClassValue token (-60)) -- means wallet 1 should have 10 ada more and 60 tokens less
     .&&. walletFundsChange (Wallet 2) (Ada.lovelaceValueOf (-20_000_000) <> assetClassValue token   20)  -- means wallet 2 should have 20 ada less and 20 tokens more
     .&&. walletFundsChange (Wallet 3) (Ada.lovelaceValueOf (- 5_000_000) <> assetClassValue token    5)  -- means wallet 3 should have 5 ada less and 5 tokens more
    )
    myTrace

runMyTrace :: IO ()
runMyTrace = runEmulatorTraceIO' def emCfg myTrace

emCfg :: EmulatorConfig  -- set up initial values for the wallets
emCfg = EmulatorConfig (Left $ Map.fromList [(Wallet w, v) | w <- [1 .. 3]]) def def
  where
    v :: Value
    v = Ada.lovelaceValueOf 1_000_000_000 <> assetClassValue token 1000  -- add underscores to make it easier to read large numbers

currency :: CurrencySymbol
currency = "aa"

name :: TokenName
name = "A"

token :: AssetClass
token = AssetClass (currency, name)

myTrace :: EmulatorTrace ()
myTrace = do
    h <- activateContractWallet (Wallet 1) startEndpoint  -- activate start endpoint on wallet 1 as it will run the token sale
    callEndpoint @"start" h (currency, name, True) -- specify True to use the ThreadToken NFT mechanism for the state machine
    void $ Emulator.waitNSlots 5  -- wait to give time to start the state machine
    Last m <- observableState h  -- ask for the observableState which is the TokenSale value that has been started; value also contains the ThreadToken
    case m of
        Nothing -> Extras.logError @String "error starting token sale"
        Just ts -> do
            Extras.logInfo $ "started token sale " ++ show ts

            h1 <- activateContractWallet (Wallet 1) $ useEndpoints ts -- useEndpoints is parametatized by TokenSale value
            h2 <- activateContractWallet (Wallet 2) $ useEndpoints ts
            h3 <- activateContractWallet (Wallet 3) $ useEndpoints ts

            callEndpoint @"set price" h1 1_000_000  -- wallets 1 sets the price to 1 ADA
            void $ Emulator.waitNSlots 5

            callEndpoint @"add tokens" h1 100  -- wallets 1 adds 100 tokens to the token sale UTXO
            void $ Emulator.waitNSlots 5

            callEndpoint @"buy tokens" h2 20   -- wallets 2 buys 20 tokens; should cost 20 ADA bc price is 1 ADA per token
            void $ Emulator.waitNSlots 5

            callEndpoint @"buy tokens" h3 5    -- wallet 3 buys 5 tokens
            void $ Emulator.waitNSlots 5

            callEndpoint @"withdraw" h1 (40, 10_000_000)  -- wallet 1 withdraws 40 tokens & 10 ada; 35 tokens left & 15 ada should be left
            void $ Emulator.waitNSlots 5
