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
-- this module uses the emulatorTrace monad to test the game defined in EvenOdd.hs
module Week07.Test where

import           Control.Monad              hiding (fmap)
import           Control.Monad.Freer.Extras as Extras
import           Data.Default               (Default (..))
import qualified Data.Map                   as Map
import           Ledger
import           Ledger.TimeSlot
import           Ledger.Value
import           Ledger.Ada                 as Ada
import           Plutus.Trace.Emulator      as Emulator
import           PlutusTx.Prelude
import           Prelude                    (IO, Show (..))
import           Wallet.Emulator.Wallet

import           Week07.EvenOdd

test :: IO ()
test = do  -- different combinations in sequence of test' using different choice for the first and second player
    test' Zero Zero
    test' Zero One
    test' One Zero
    test' One One

test' :: GameChoice -> GameChoice -> IO () -- simulates a game; the two arguments for GameChoice as the choices by first player and second player, respectively
test' c1 c2 = runEmulatorTraceIO' def emCfg def $ myTrace c1 c2 -- using prime version runEmulatorTraceIO to specify initial distribution in emCfg (EmulatorConfig)
  where
    emCfg :: EmulatorConfig
    emCfg = EmulatorConfig $ Left $ Map.fromList
        [ (Wallet 1, v <> assetClassValue (AssetClass (gameTokenCurrency, gameTokenName)) 1) -- provide wallet 1 with the game NFT; in production code, an NFT must be minted instead
        , (Wallet 2, v)
        ]

    v :: Value
    v = Ada.lovelaceValueOf 1_000_000_000

gameTokenCurrency :: CurrencySymbol
gameTokenCurrency = "ff"  -- made up currency symbol

gameTokenName :: TokenName
gameTokenName = "STATE TOKEN" -- made up token name

myTrace :: GameChoice -> GameChoice -> EmulatorTrace ()
myTrace c1 c2 = do -- the parameters c1 and c2 are the choices for the first and the second player
    Extras.logInfo $ "first move: " ++ show c1 ++ ", second move: " ++ show c2

    h1 <- activateContractWallet (Wallet 1) endpoints -- start an instance of the endpoints contract for wallet 1
    h2 <- activateContractWallet (Wallet 2) endpoints -- start an instance of the endpoints contract for wallet 2

    let pkh1      = pubKeyHash $ walletPubKey $ Wallet 1
        pkh2      = pubKeyHash $ walletPubKey $ Wallet 2
        stake     = 5_000_000                    -- define to use a stake of 5 ADA 
        deadline1 = slotToBeginPOSIXTime def 5   -- the game play deadline
        deadline2 = slotToBeginPOSIXTime def 10  -- the revealing deadline

        fp = FirstParams
                { fpSecond         = pkh2
                , fpStake          = stake
                , fpPlayDeadline   = deadline1
                , fpRevealDeadline = deadline2
                , fpNonce          = "SECRETNONCE"  -- should be a random string instead
                , fpCurrency       = gameTokenCurrency
                , fpTokenName      = gameTokenName
                , fpChoice         = c1
                }
        sp = SecondParams
                { spFirst          = pkh1
                , spStake          = stake
                , spPlayDeadline   = deadline1
                , spRevealDeadline = deadline2
                , spCurrency       = gameTokenCurrency
                , spTokenName      = gameTokenName
                , spChoice         = c2
                }

    callEndpoint @"first" h1 fp -- call the first endpoint on wallet 1 with fp parameters

    void $ Emulator.waitNSlots 3

    callEndpoint @"second" h2 sp -- call the second endpoint on wallet 2 with sp parameters

    void $ Emulator.waitNSlots 10
