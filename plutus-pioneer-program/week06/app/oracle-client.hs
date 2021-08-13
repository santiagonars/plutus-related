{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings  #-}

module Main
    ( main
    ) where

import Control.Concurrent
import Control.Monad          (when)
import Control.Monad.IO.Class (MonadIO (..))
import Data.ByteString        (ByteString)
import Data.ByteString.Char8  (unpack)
import Data.Proxy             (Proxy (..))
import Data.Text              (pack)
import Data.UUID
import Network.HTTP.Req
import Text.Regex.TDFA

main :: IO ()
main = do
    uuid <- read <$> readFile "oracle.cid" -- read the file to get the uuid for the oracle contract instance
    putStrLn $ "oracle contract instance id: " ++ show uuid
    go uuid Nothing
  where
    go :: UUID -> Maybe Integer -> IO a
    go uuid m = do
        x <- getExchangeRate -- look up current exchange rate for USD on coinmarketcap.com
        let y = Just x
        when (m /= y) $          -- check whether the value has changed; m is the old value
            updateOracle uuid x  -- if the exchnage rate has changed, it calls updateOracle to call the endpoint in the contract
        threadDelay 5_000_000    -- wait for 5 seconds; even though it might be too fast since blocks on cardano appear every 20 secionds on average
        go uuid y                -- call go again 

updateOracle :: UUID -> Integer -> IO () -- interact with a running contract
updateOracle uuid x = runReq defaultHttpConfig $ do -- defaultHttpConfig comes the REQ package
    v <- req    -- prepare a post request
        POST    -- use the local server with the endpoint in the API module
        (http "127.0.0.1" /: "api"  /: "new" /: "contract" /: "instance" /: pack (show uuid) /: "endpoint" /: "update")
        (ReqBodyJson x) -- provide the request body in JSON format of the value to update to
        (Proxy :: Proxy (JsonResponse ())) -- this just means that a JsonResponse is expected
        (port 8080) -- this provides the port
    liftIO $ putStrLn $ if responseStatusCode v == 200 -- if the request is succesful, log a message accordingly
        then "updated oracle to " ++ show x 
        else "error updating oracle"

getExchangeRate :: IO Integer  -- get the ADA/USD exchange rate from coinmarketcap.com
getExchangeRate = runReq defaultHttpConfig $ do
    v <- req
        GET
        (https "coinmarketcap.com" /: "currencies" /: "cardano")
        NoReqBody
        bsResponse
        mempty
    let priceRegex      = "priceValue___11gHJ \">\\$([\\.0-9]*)" :: ByteString  -- use a regular expression to the the exchange rate (NOT A GOOD APPROACH FOR PRODUCTION CODE)
        (_, _, _, [bs]) = responseBody v =~ priceRegex :: (ByteString, ByteString, ByteString, [ByteString])
        d               = read $ unpack bs :: Double
        x               = round $ 1_000_000 * d  -- multiply by 1 million and round it to be an Interger value
    liftIO $ putStrLn $ "queried exchange rate: " ++ show d -- log the exchange rate
    return x
