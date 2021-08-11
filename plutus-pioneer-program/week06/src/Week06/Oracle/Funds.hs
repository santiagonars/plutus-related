{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}
-- This module provides 2 contracts
module Week06.Oracle.Funds
    ( ownFunds
    , ownFunds'
    ) where

import           Control.Monad    hiding (fmap)
import qualified Data.Map         as Map
import           Data.Monoid      (Last (..))
import           Data.Text        (Text)
import           Plutus.Contract  as Contract
import           PlutusTx.Prelude hiding ((<$>))
import           Prelude          (Show (..), String, (<$>))
import           Ledger           hiding (singleton)
import           Ledger.Value     as Value
-- OwnFunds function sum up all value in own wallet's UTXOs 
ownFunds :: Contract w s Text Value
ownFunds = do
    pk    <- ownPubKey -- get own public key
    utxos <- utxoAt $ pubKeyAddress pk -- get a map of UTXOs (references) sitting in own wallet; look up UTXOs at the address given by public key
    let v = mconcat $ Map.elems $ txOutValue . txOutTxOut <$> utxos -- first get the output belonging to a UTXO, and then the value. This become a map from UTXO references to values
    logInfo @String $ "own funds: " ++ show (Value.flattenValue v)  -- Map.elems ignores the keys and gives a list of all values; mconcat combines the list the same type (adds up all the values)
    return v  -- v is the sum of all the values of all UTXOs in own wallet

-- A variation of ownFunds; Instead of returning the value, it permanantly tells it
ownFunds' :: Contract (Last Value) Empty Text ()
ownFunds' = do
    handleError logError $ ownFunds >>= tell . Last . Just -- use monadic bind (>>=) to tells it; so it write the sum of funds owned to the log
    void $ Contract.waitNSlots 1 -- wait 1 slot
    ownFunds' -- call recursively; it will run forever and every slot check own funds available and write it into the log
