{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE NumericUnderscores    #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}
-- This tests the contracts using the emulator monad; run it on cabal repl by just entering test
module Week06.Oracle.Test where

import           Control.Monad              hiding (fmap)
import           Control.Monad.Freer.Extras as Extras
import           Data.Default               (Default (..))
import qualified Data.Map                   as Map
import           Data.Monoid                (Last (..))
import           Data.Text                  (Text)
import           Ledger
import           Ledger.Value               as Value
import           Ledger.Ada                 as Ada
import           Plutus.Contract            as Contract
import           Plutus.Trace.Emulator      as Emulator
import           PlutusTx.Prelude           hiding (Semigroup(..), unless)
import           Prelude                    (IO, Semigroup(..), Show (..))
import           Wallet.Emulator.Wallet

import           Week06.Oracle.Core
import           Week06.Oracle.Funds
import           Week06.Oracle.Swap

assetSymbol :: CurrencySymbol -- need some assets to do testing
assetSymbol = "ff" -- not the hash of a policy script but just like in the playground, it can be done for the emulatorTrace

assetToken :: TokenName
assetToken = "USDT"

-- Use the prime version of runEmulatorTraceIO which take 3 more arguments to have more control in the emualator environment; 
-- Arguments for runEmulatorTraceIO' :
-- 1st (TraceConfig): determines how the various log messages are display; def is usign default which is the same as the non-prime version of the function
-- 2nd (EmulatorConfig): to configure the initial distribution; the main reason for using runEmulatorTraceIO' instead of runEmulatorTraceIO
test :: IO ()
test = runEmulatorTraceIO' def emCfg def myTrace
  where
    emCfg :: EmulatorConfig -- specify that every wallet has 100 ADA in the beginning along with 100 of the USDT tokens
    emCfg = EmulatorConfig $ Left $ Map.fromList [(Wallet i, v) | i <- [1 .. 10]]

    v :: Value
    v = Ada.lovelaceValueOf                    100_000_000 <>
        Value.singleton assetSymbol assetToken 100_000_000

checkOracle :: Oracle -> Contract () Empty Text a -- a helper contract to permanently check the oracle value and log it
checkOracle oracle = do
    m <- findOracle oracle
    case m of
        Nothing        -> return () -- stops if the oracle is not found
        Just (_, _, x) -> Contract.logInfo $ "Oracle value: " ++ show x -- log the oracle value if it was found
    Contract.waitNSlots 1 >> checkOracle oracle -- wait for 1 slot and then call again recursively

myTrace :: EmulatorTrace ()
myTrace = do
    let op = OracleParams  -- define parameters for the oracle
                { opFees = 1_000_000      -- fees of 1,0000,000 lovelace (1 ADA)
                , opSymbol = assetSymbol  -- the asset for the asset defined in the initial distribution
                , opToken  = assetToken   
                }

    h1 <- activateContractWallet (Wallet 1) $ runOracle op -- On wallet 1, start the oracle and return the handle 
    void $ Emulator.waitNSlots 1 -- wait for 1 slot
    oracle <- getOracle h1  -- getOracle is a helper function defined below; once an oracle is created, it tells it and this state needs to be retrieved; this is the oracle value
                            -- once the oravle value is retrieved, it can now be used
    void $ activateContractWallet (Wallet 2) $ checkOracle oracle -- On wallet 2, use checkOracle to check the oracle value in every slot

    callEndpoint @"update" h1 1_500_000  -- initilize the oracle to 1.5 USDT per ADA
    void $ Emulator.waitNSlots 3         -- wait for 3 slots

    void $ activateContractWallet (Wallet 1) ownFunds' -- call ownFunds' function on wallets 1 through 5 for initial balances
    void $ activateContractWallet (Wallet 3) ownFunds'
    void $ activateContractWallet (Wallet 4) ownFunds'
    void $ activateContractWallet (Wallet 5) ownFunds'

    h3 <- activateContractWallet (Wallet 3) $ swap oracle -- start the swap contract on wallets 3, 4, and 5
    h4 <- activateContractWallet (Wallet 4) $ swap oracle
    h5 <- activateContractWallet (Wallet 5) $ swap oracle
    -- the rest is to try some scenarios
    callEndpoint @"offer" h3 10_000_000  -- wallet 3 offers 10 ADA for the swap
    callEndpoint @"offer" h4 20_000_000  -- wallet 4 offers 20 ADA for the swap
    void $ Emulator.waitNSlots 3

    callEndpoint @"use" h5 ()  -- wallet 5 picks one fo the two swaps and pays USDT for it; it's no obvious which one, just the one it finds first
    void $ Emulator.waitNSlots 3

    callEndpoint @"update" h1 1_700_000 -- wallet 1 updates oracle value to 1.7 USDT per ADA
    void $ Emulator.waitNSlots 3

    callEndpoint @"use" h5 ()  -- wallet 5 tries again and grabs the remaining swap; it will pay a different price
    void $ Emulator.waitNSlots 3

    callEndpoint @"update" h1 1_800_000 -- wallet 1 updates oracle value to 1.8 USDT per ADA; fees are extracted
    void $ Emulator.waitNSlots 3

    callEndpoint @"retrieve" h3 ()  -- retrieve remaining swaps though it shouldn't do anything becasue they were used by h5
    callEndpoint @"retrieve" h4 ()
    void $ Emulator.waitNSlots 3
  where
    getOracle :: ContractHandle (Last Oracle) OracleSchema Text -> EmulatorTrace Oracle
    getOracle h = do
        l <- observableState h  -- observableState takes handle to provide the observable state of the runOracle contract which should contain the oracle
        case l of
            Last Nothing       -> Emulator.waitNSlots 1 >> getOracle h  -- if oracle is not there, wait for 1 slot and then try again
            Last (Just oracle) -> Extras.logInfo (show oracle) >> return oracle -- if found, log for debbuggin purposes and then return it
