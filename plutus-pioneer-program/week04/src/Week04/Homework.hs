{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications  #-}
{-# LANGUAGE TypeOperators     #-}

module Week04.Homework where

import Data.Aeson            (FromJSON, ToJSON)
import Data.Functor          (void)
import Data.Text             (Text)
import GHC.Generics          (Generic)
import Ledger
import Ledger.Ada            as Ada
import Ledger.Constraints    as Constraints
import Plutus.Contract       as Contract
import Plutus.Trace.Emulator as Emulator

data PayParams = PayParams
    { ppRecipient :: PubKeyHash
    , ppLovelace  :: Integer
    } deriving (Show, Generic, FromJSON, ToJSON)

type PaySchema = Endpoint "pay" PayParams

payContract :: Contract () PaySchema Text ()
payContract = do
    pp <- endpoint @"pay"                                                        -- mustPayToPubKey adds the constraint that it should pay to a PubKey; value is the 2nd argument 
    let tx = mustPayToPubKey (ppRecipient pp) $ lovelaceValueOf $ ppLovelace pp  -- lovelaceValueOf turns an integer into a value given in lovelaces
    void $ submitTx tx  -- submitTx will try to construct a transaction that satisfies the constraints (find output in wallet large enough to cover payment + fee, and contruct a change output going back to wallet)
    payContract -- recursively call the contract again to be able to call the "pay" endpoint again and again

-- A trace that invokes the pay endpoint of payContract on Wallet 1 twice, each time with Wallet 2 as
-- recipient, but with amounts given by the two arguments. There should be a delay of one slot
-- after each endpoint call.
-- Expected behavior: start the contract with wallet 1, and then 2 "pay" endpoint calls where the recipient is wallet 2
--                    Amounts to be paid are given by the 2 parameters; First parameter for 1st endpoint call, Second parameter for 2nd endpoint call
--                    After each of the endpoint calls, wait for 1 slot
-- TODO: 1st- implement expected behavior 
--       2nd- Modify the payContract to handle the exception instead of crashing, it should be caught and just produces log message with exception
payTrace :: Integer -> Integer -> EmulatorTrace ()
payTrace _ _ = undefined -- IMPLEMENT ME!

payTest1 :: IO ()
payTest1 = runEmulatorTraceIO $ payTrace 1000000 2000000

payTest2 :: IO ()
payTest2 = runEmulatorTraceIO $ payTrace 1000000000 2000000 -- shoiuld raise an exception erroe because of lack of ADA in Wallet 1
