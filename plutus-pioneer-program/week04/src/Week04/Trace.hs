{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE DataKinds        #-}

module Week04.Trace where

import Control.Monad.Freer.Extras as Extras
import Data.Default               (Default (..))
import Data.Functor               (void)
import Ledger
import Ledger.TimeSlot
import Plutus.Trace.Emulator      as Emulator
import Wallet.Emulator.Wallet

import Week04.Vesting

-- Contract w s e a
-- EmulatorTrace a

test :: IO ()  -- to test the trace
test = runEmulatorTraceIO myTrace

myTrace :: EmulatorTrace () -- define a trace
myTrace = do -- start a contract in all wallets; the playground does this automatically
    h1 <- activateContractWallet (Wallet 1) endpoints -- activateContractWallet - takes a wallet and the contract to activate (called endpoint to use in playground)
    h2 <- activateContractWallet (Wallet 2) endpoints --                        -  returns a Handle; it is binded to h1
    callEndpoint @"give" h1 $ GiveParams --call "give" endpoint on handle of wallet 1 and specify the parameter values to pass to this endpoint
        { gpBeneficiary = pubKeyHash $ walletPubKey $ Wallet 2
        , gpDeadline    = slotToBeginPOSIXTime def 20
        , gpAmount      = 10000000
        }
    void $ waitUntilSlot 20 -- wait until a specific slot; returns the reached slot; use void from data Functor to prevent a warning
    callEndpoint @"grab" h2 () -- no parameters for "grab" endpoint
    s <- waitNSlots 1 -- wait for a number of slots
    Extras.logInfo $ "reached " ++ show s  -- explicitly log inside the trace
