{-# LANGUAGE DeriveAnyClass     #-}
{-# LANGUAGE DeriveGeneric      #-}
-- PAB is the Plutus Application Backend; 
-- This PAB module is basically just a one type definition; ***The ideas is that it defines the contract instances to run
module Week06.Oracle.PAB
    ( OracleContracts (..)
    ) where

import           Data.Aeson                (FromJSON, ToJSON)
import           Data.Text.Prettyprint.Doc (Pretty (..), viaShow)
import           GHC.Generics              (Generic)
import           Ledger

import qualified Week06.Oracle.Core        as Oracle
-- the contract instances to run
data OracleContracts = Init | Oracle CurrencySymbol | Swap Oracle.Oracle -- 
    deriving (Eq, Ord, Show, Generic, FromJSON, ToJSON) -- define the data type where each value of the data type corresponds to a contract to eventually run

instance Pretty OracleContracts where
    pretty = viaShow

-- OracleContracts definition:
-- *** Init is used setup an environment where is a USDT token available
-- *** the Oracle constructor corresponds to the runOracle contract that will start and run the oracle and provide the the "update" endpoint
--           -> the CurrencySymbol parameter is used to communicate the currency symbol of the USDT token
-- *** The Swap is parameterized by the oracle and will be used to run the swap contract; it offers various endpoints

--       >>>>> The OracleContracts definition will be used from the PAB and in the front-end

-- PAB allows turning everything done for the oracle and turn it into an executable application that runs the constracts
-- It runs on a simulated blockchain, but if testnet or mainnet was running, it could be deployed there once available
