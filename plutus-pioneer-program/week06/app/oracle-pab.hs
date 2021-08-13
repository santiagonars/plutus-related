{-# LANGUAGE DataKinds          #-}
{-# LANGUAGE DeriveAnyClass     #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts   #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RankNTypes         #-}
{-# LANGUAGE TypeApplications   #-}
{-# LANGUAGE TypeFamilies       #-}
{-# LANGUAGE TypeOperators      #-}

module Main
    ( main
    ) where

import           Control.Monad                       (forM_, void, when)
import           Control.Monad.Freer                 (Eff, Member, interpret, type (~>))
import           Control.Monad.Freer.Error           (Error)
import           Control.Monad.Freer.Extras.Log      (LogMsg)
import           Control.Monad.IO.Class              (MonadIO (..))
import           Data.Aeson                          (FromJSON, Result (..), fromJSON)
import           Data.Default                        (Default (..))
import           Data.Monoid                         (Last (..))
import           Data.Text                           (Text, pack)
import           Ledger
import           Ledger.Constraints
import qualified Ledger.Value                        as Value
import           Plutus.Contract
import           Plutus.PAB.Effects.Contract         (ContractEffect (..))
import           Plutus.PAB.Effects.Contract.Builtin (Builtin, SomeBuiltin (..), endpointsToSchemas, handleBuiltin)
import           Plutus.PAB.Monitoring.PABLogMsg     (PABMultiAgentMsg)
import           Plutus.PAB.Simulator                (SimulatorEffectHandlers)
import qualified Plutus.PAB.Simulator                as Simulator
import           Plutus.PAB.Types                    (PABError (..))
import qualified Plutus.PAB.Webserver.Server         as PAB.Server
import qualified Plutus.Contracts.Currency           as Currency

import           Wallet.Emulator.Types               (Wallet (..), walletPubKey)
import           Wallet.Types                        (ContractInstanceId (..))

import qualified Week06.Oracle.Core                  as Oracle
import           Week06.Oracle.PAB                   (OracleContracts (..))
import qualified Week06.Oracle.Swap                  as Oracle
-- main is the actual PAB code; this is an IO() so it's a proper main program so it will be a proper executable
main :: IO () -- this takes an IO action and lifts it into the monad in question
main = void $ Simulator.runSimulationWith handlers $ do  -- Simulator is monad specific to the PAB (similar to emulatorTrace); handlers is defined below
    Simulator.logString @(Builtin OracleContracts) "Starting Oracle PAB webserver. Press enter to exit."
    shutdown <- PAB.Server.startServerDebug -- startServerDebug actually starts the server and the return value is an action that can be used to shutdown the server

    cidInit <- Simulator.activateContract (Wallet 1) Init -- activateContract gets a wallet to start that instance and a value of defined contract type; will run handleOracleContracts on wallet 1
    cs      <- waitForLast cidInit -- get the currency symbol; waitForLast is used on the handle of the Init contract; it will wait until the Init contract is done  
    _       <- Simulator.waitUntilFinished cidInit -- wait until the Init contract is finished

    cidOracle <- Simulator.activateContract (Wallet 1) $ Oracle cs -- start the Oracle on wallet 1 and get the handle to the contract (the contract instance id of the Oracle)
    liftIO $ writeFile "oracle.cid" $ show $ unContractInstanceId cidOracle  -- write the contract identifier to a file to allow the web interface to later talk to the contract
    oracle <- waitForLast cidOracle -- get the oracle value                  -- unContractInstanceId extracts the uuid
                                                      -- *** applying lifeIO to an IO action moves it to this simulator monad
    forM_ wallets $ \w ->   -- activate the swap contract for the wallets 
        when (w /= Wallet 1) $ do  -- exclude wallet 1 that runs the oracle
            cid <- Simulator.activateContract w $ Swap oracle
            liftIO $ writeFile ('W' : show (getWallet w) ++ ".cid") $ show $ unContractInstanceId cid -- write the contract instance ids to a file for each

    void $ liftIO getLine -- block until the user presses enter
    shutdown -- shutdown the server

-- waitForLast is an example of getting information of a contract
waitForLast :: FromJSON a => ContractInstanceId -> Simulator.Simulation t a   -- waitForLast takes a contract instance id (cid)
waitForLast cid =      -- waitForState takes the contract instance id and a predicate that takes a json and returns a Maybe a
    flip Simulator.waitForState cid $ \json -> case fromJSON json of -- it reads the state of the contract; fromJSON parses the json
        Success (Last (Just x)) -> Just x  -- if it succeeds the json parsing was good, and should get a Success with the value it pass which is a Last Just of the state value
        _                       -> Nothing -- if the result is Nothing, it will wait until the state has used tell of the value and returned it
                                           -- it could fail if the parsing failed or it returned a Last Nothing
wallets :: [Wallet]
wallets = [Wallet i | i <- [1 .. 5]]  -- define number of wallets

usdt :: TokenName
usdt = "USDT"

oracleParams :: CurrencySymbol -> Oracle.OracleParams -- takes currency symbol for USDT and defines example oracle params
oracleParams cs = Oracle.OracleParams
    { Oracle.opFees   = 1_000_000  -- fee of 1 ada
    , Oracle.opSymbol = cs
    , Oracle.opToken  = usdt
    }
-- handleOracleContracts contain mostly boilerplate code
handleOracleContracts ::
    ( Member (Error PABError) effs
    , Member (LogMsg (PABMultiAgentMsg (Builtin OracleContracts))) effs
    )
    => ContractEffect (Builtin OracleContracts)
    ~> Eff effs
handleOracleContracts = handleBuiltin getSchema getContract where
    getSchema = \case
        Init     -> endpointsToSchemas @Empty -- wont have a any schemas, onkly blockchain actions
        Oracle _ -> endpointsToSchemas @Oracle.OracleSchema -- Oracle uses the oracle schema
        Swap _   -> endpointsToSchemas @Oracle.SwapSchema   -- Swap uses the swap schema
    getContract = \case
        Init        -> SomeBuiltin   initContract -- will run the init contract (defined below)
        Oracle cs   -> SomeBuiltin $ Oracle.runOracle $ oracleParams cs
        Swap oracle -> SomeBuiltin $ Oracle.swap oracle -- given an oracle value, ist will run the swap contract with this value as parameter

handlers :: SimulatorEffectHandlers (Builtin OracleContracts)
handlers =
    Simulator.mkSimulatorHandlers @(Builtin OracleContracts) def []
    $ interpret handleOracleContracts

initContract :: Contract (Last CurrencySymbol) Empty Text () -- this will generate the initial funds
initContract = do
    ownPK <- pubKeyHash <$> ownPubKey
    cur   <-
        mapError (pack . show)
        (Currency.mintContract ownPK [(usdt, fromIntegral (length wallets) * amount)] -- use the forge contract mintContract; give each wallet 100 UDST by multiple number of wallets by amount
        :: Contract (Last CurrencySymbol) Empty Currency.CurrencyError Currency.OneShotCurrency)
    let cs = Currency.currencySymbol cur    -- look up the currency symbol
        v  = Value.singleton cs usdt amount -- the value of how much each wallet should get
    forM_ wallets $ \w -> do  
        let pkh = pubKeyHash $ walletPubKey w
        when (pkh /= ownPK) $ do
            tx <- submitTx $ mustPayToPubKey pkh v  -- create transactions that sends each wallet the value amount (except myself because I already have them)
            awaitTxConfirmed $ txId tx
    tell $ Last $ Just cs -- tell the currency symbol (cs) that was minted
  where
    amount :: Integer
    amount = 100_000_000  -- 100 UDST
