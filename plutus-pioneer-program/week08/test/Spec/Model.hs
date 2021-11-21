{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NumericUnderscores    #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE StandaloneDeriving    #-} -- allows deriving instance Eq & Show for ContractInstanceKey
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}

module Spec.Model
    ( tests
    , test
    , TSModel (..)
    )  where

import           Control.Lens                       hiding (elements)
import           Control.Monad                      (void, when)
import           Data.Default                       (Default (..))
import           Data.Map                           (Map)
import qualified Data.Map                           as Map
import           Data.Maybe                         (isJust, isNothing)
import           Data.Monoid                        (Last (..))
import           Data.String                        (IsString (..))
import           Data.Text                          (Text)
import           Plutus.Contract.Test                -- provides basic support for testing Plutus
import           Plutus.Contract.Test.ContractModel  -- to use QuickCheck approach; has all capabilities to define model and link it to real contract
import           Plutus.Trace.Emulator
import           Ledger                             hiding (singleton)
import           Ledger.Ada                         as Ada
import           Ledger.Value
import           Test.QuickCheck
import           Test.Tasty                         -- for automated testing
import           Test.Tasty.QuickCheck              -- provides a link between QuickCheck and Tasty; to use QuickCheck properties in Tasty test suite
     
import           Week08.TokenSale                   (TokenSale (..), TSStartSchema, TSUseSchema, startEndpoint, useEndpoints)

data TSState = TSState  -- define data model for Token Sale state; represetn state of one token sale instance
    { _tssPrice    :: !Integer  -- current price
    , _tssLovelace :: !Integer  -- current supply of lovelace in the utxo in the contract
    , _tssToken    :: !Integer  -- current supply of tokens in the contract
    } deriving Show

makeLenses ''TSState -- implement lenses for it; need optics to interact to interact with Plutus.Contract.Test

newtype TSModel = TSModel {_tsModel :: Map Wallet TSState} -- define the model; it's a map from model to TokenSale state
    deriving Show

makeLenses ''TSModel

tests :: TestTree
tests = testProperty "token sale model" prop_TS

instance ContractModel TSModel where -- here define how model should behave
    -- associate data type (advanced Haskell feature); associate Action type represent the actions that QuickCheck will  generate
    data Action TSModel =   -- it has a constructor for each of the endpoints; there are arguments to keep track which of the wallets performs an action
              Start Wallet  -- means that this wallet started the contract
            | SetPrice Wallet Wallet Integer -- 2nd wallet sets the price operated by the 1st wallet; fails with the 2 wallets are not the same
            | AddTokens Wallet Wallet Integer -- 2nd wallet adds tokens to the sale of the 1st wallet's token sale
            | Withdraw Wallet Wallet Integer Integer -- 2nd wallet withdraw tokens & lovelaces from token sale ran by 1st wallet
            | BuyTokens Wallet Wallet Integer -- 2nd wallet wants to buy tokens (specied as Integer) from the 1st wallet's token sale
        deriving (Show, Eq)

    data ContractInstanceKey TSModel w s e where -- need a key to identify each instance of contract that is running; this is a generalized algebraic data type (GADT) to allow having different type parameters
        StartKey :: Wallet           -> ContractInstanceKey TSModel (Last TokenSale) TSStartSchema Text -- takes wallet as argument and produces something of the type on the right
        UseKey   :: Wallet -> Wallet -> ContractInstanceKey TSModel ()               TSUseSchema   Text
            -- above take 2 wallet parameters; 1st one owns token sale, 2nd one runs the contract
    -- instanceTag takes key and wallet(ignored) 
    instanceTag key _ = fromString $ "instance tag for: " ++ show key -- this function should result in a diff tag for each instance ran; will have 3 contract instances per wallet (to start to use, and to use other wallets)
    -- arbitraryAction gets model state as argument (ignore here)
    arbitraryAction _ = oneof $ -- will tell the system how to generate a random action; ; oneof gets a list of arbitrary actions & randomly picks one from the lines below
        (Start <$> genWallet) :   -- genWallet returns a random wallet and then the action start to start that wallet
        [ SetPrice  <$> genWallet <*> genWallet <*> genNonNeg ]               ++  -- genNonNeg generates a random non-negative integer
        [ AddTokens <$> genWallet <*> genWallet <*> genNonNeg ]               ++  -- <*> is an applicative monad binds the results
        [ BuyTokens <$> genWallet <*> genWallet <*> genNonNeg ]               ++
        [ Withdraw  <$> genWallet <*> genWallet <*> genNonNeg <*> genNonNeg ]

    initialState = TSModel Map.empty -- initial state of the model; model starts with empty map bc there wont be any token sale yet
    -- The following functions describe the effect the actions should have on the model 
    nextState (Start w) = do -- create an entry in the map for wallet w; tsModel is wrapper around a map of wallets; this will change the model & the entry in the map at wallet w to a TSState with all zero values
        (tsModel . at w) $= Just (TSState 0 0 0) -- $= comes from the Spec monad that takes a lens from LHS and puts focus on it on the RHS entry
        wait 1                                   -- `at` (from lens library) zooms in to the value (t state) of element at that key; target type of the `at` lens is a Maybe value; so it would be either Just or Nothing
                                                 -- If `at`is set to Just, it creates an entry, else if it's set to Nothing, it deletes the entry at that key
                                                 -- wait comes the spec monad; it waits for 1 step (until the next block)
    nextState (SetPrice v w p) = do -- set price to token sale at wallet v; price setter is wallet w; p is new price
        when (v == w) $ -- `when` comes from control monad; update model is owner tries to set the price
            (tsModel . ix v . tssPrice) $= p --  `ix` is a traversal that only focuses on the value of a key if it's there; tssPrice focuses on the price given in the TSState value
        wait 1                               -- explanation: if wallet v is there, it will set the price to p 

    nextState (AddTokens v w n) = do -- wallet w tries to add tokens the token sale started by wallet v; n is the amount of tokens
        started <- hasStarted v                                     -- has the token sale started? (use helper function )
        when (n > 0 && started) $ do -- if started is True and want to add postive amount of tokens
            bc <- askModelState $ view $ balanceChange w -- askModelState asks for Model State; `view` zooms into the value like an optic ; balanceChange indicates how much the balance of a wallet w has changed; 
            let token = tokens Map.! v -- get the token that wallet v is selling
            when (tokenAmt + assetClassValueOf bc token >= n) $ do  -- does the wallet have the tokens to give? how many tokens wallet w has at this momment; has to be greater or equal to amoutn of token it wants to add
                withdraw w $ assetClassValue token n -- `withdraw` is a function in spec monad to withdraw n tokens from it to add to contract
                (tsModel . ix v . tssToken) $~ (+ n) -- ix allows zooming in to the token sale run by v; tssToken is the field how many tokens there are in the Token Sale
        wait 1                                       -- $~ allows to specify a function to apply; it increases in the model the number of tokens run by wallet v

    nextState (BuyTokens v w n) = do
        when (n > 0) $ do  -- check that n is positive
            m <- getTSState v -- get the Token Sale state
            case m of
                Just t
                    | t ^. tssToken >= n -> do -- there has to be at least n tokens on sale
                        let p = t ^. tssPrice -- look up price in the token sale
                            l = p * n  -- (total price) is the (price of token) times (the # of tokens)
                        withdraw w $ lovelaceValueOf l -- take total price in lovelaces out of wallet w
                        deposit w $ assetClassValue (tokens Map.! v) n -- put the n # of bought tokens into wallet w
                        (tsModel . ix v . tssLovelace) $~ (+ l) -- increase amount of lovelaces in the model of the amount paid by wallet w
                        (tsModel . ix v . tssToken)    $~ (+ (- n)) -- decrease amount of tokens in the model of the amount sold to wallet w
                _ -> return () -- don't do anything if TokenSale state is Nothing, meaning wallet v hasn't started the token sale
        wait 1

    nextState (Withdraw v w n l) = do -- token sale run by `v`; wallet w wants to withdraw `n` tokens and `l` lovelace
        when (v == w) $ do -- only owner of token sale can withdraw
            m <- getTSState v -- look up the token sale state
            case m of
                Just t
                    | t ^. tssToken >= n && t ^.   >= l -> do  -- check that there are at least `n` tokens and `l` lovelace
                        deposit w $ lovelaceValueOf l <> assetClassValue (tokens Map.! w) n -- deposit tokens & lovelace in wallet 
                        (tsModel . ix v . tssLovelace) $~ (+ (- l)) -- update the state by removing the lovelace that were withdrawn
                        (tsModel . ix v . tssToken) $~ (+ (- n)) -- update the state by removing the token that were withdrawn
                _ -> return ()
        wait 1

    perform h _ cmd = case cmd of -- `perform` links the simulated contract model to the actual contract endpoints; the handle h gets a key and provides the handle needed to call the correct endpoint instance; passd this handle to all endpoint calls
        (Start w)          -> callEndpoint @"start"      (h $ StartKey w) (tokenCurrencies Map.! w, tokenNames Map.! w, False) >> delay 1 -- also pass triple (currency symbol, token name, use Thread Token?)
        (SetPrice v w p)   -> callEndpoint @"set price"  (h $ UseKey v w) p                                                    >> delay 1 -- wallet w wants to set price in token sale provided by v
        (AddTokens v w n)  -> callEndpoint @"add tokens" (h $ UseKey v w) n                                                    >> delay 1 
        (BuyTokens v w n)  -> callEndpoint @"buy tokens" (h $ UseKey v w) n                                                    >> delay 1
        (Withdraw v w n l) -> callEndpoint @"withdraw"   (h $ UseKey v w) (n, l)                                               >> delay 1
    -- `precondition` sets conditions that certain actions shouldn't be randomly generated; `s` is the model state 
    precondition s (Start w)          = isNothing $  ' s w -- so that `start` is only possible if the token sale hasn't started
    precondition s (SetPrice v _ _)   = isJust    $ getTSState' s v -- all other preconditions state that thoise actions are onky possible if the token sale has started
    precondition s (AddTokens v _ _)  = isJust    $ getTSState' s v -- this is because a token sale that has started will return either a Just or Nothing
    precondition s (BuyTokens v _ _)  = isJust    $ getTSState' s v
    precondition s (Withdraw v _ _ _) = isJust    $ getTSState' s v

deriving instance Eq (ContractInstanceKey TSModel w s e) -- need Eq and Show instances for ContractInstanceKey
deriving instance Show (ContractInstanceKey TSModel w s e)

getTSState' :: ModelState TSModel -> Wallet -> Maybe TSState
getTSState' s v = s ^. contractState . tsModel . at v -- contractState goes from contract ModelState to actual model; using model state, operator ^. takes a lens zoom in and look up the value returned from `at v`
                                                      -- tsModel takes the model and gives the map; `at` gives Maybe the value
getTSState :: Wallet -> Spec TSModel (Maybe TSState) -- takes a wallet; 
getTSState v = do  -- takes wallet v
    s <- getModelState -- getModelState is an operation in the Spec monad that returns the contract model state `ModelState TSModel`
    return $ getTSState' s v

hasStarted :: Wallet -> Spec TSModel Bool -- tells us in an entry in a map corresponding to a wallet v is in the model
hasStarted v = isJust <$> getTSState v -- only care if the TSState is a Just or not; use the fmap <$> bc getTSState is in the  Spec monad

w1, w2 :: Wallet
w1 = Wallet 1
w2 = Wallet 2

wallets :: [Wallet] -- wallets utilized 
wallets = [w1, w2]

tokenCurrencies :: Map Wallet CurrencySymbol   
tokenCurrencies = Map.fromList $ zip wallets ["aa", "bb"]  -- Wallet 1 will have a different token sale from Wallet 2

tokenNames :: Map Wallet TokenName
tokenNames = Map.fromList $ zip wallets ["A", "B"]

tokens :: Map Wallet AssetClass -- map of asset classes
tokens = Map.fromList [(w, AssetClass (tokenCurrencies Map.! w, tokenNames Map.! w)) | w <- wallets]

tss :: Map Wallet TokenSale -- token sale parameter for the two wallets
tss = Map.fromList
    [ (w, TokenSale { tsSeller = pubKeyHash $ walletPubKey w -- the wallet's public key
                    , tsToken  = tokens Map.! w  -- look up in `tokens` map the correct token (it's asset class) that's on sale 
                    , tsTT     = Nothing -- not using ThreadToken mechanism
                    })
    | w <- wallets
    ]

delay :: Int -> EmulatorTrace ()
delay = void . waitNSlots . fromIntegral

instanceSpec :: [ContractInstanceSpec TSModel] -- provide the instance Specs to run; this is a list of contract instance to run; It links the keys to actual contracts
instanceSpec =                                 -- ContractInstanceSpec takes 3 arguments: 1st arg= StartKey or UseKey
    [ContractInstanceSpec (StartKey w) w startEndpoint | w <- wallets] ++  -- 2nd arg= wallet; 3rd arg= associated contracts are startEndpoint and useEndpoints 
    [ContractInstanceSpec (UseKey v w) w $ useEndpoints $ tss Map.! v | v <- wallets, w <- wallets] -- useEndpoints takes token sale (tss) belong to wallet v; results in 2 instances from w and 4 intances from v

genWallet :: Gen Wallet -- uses a random generation monad to generate a random wallet
genWallet = elements wallets -- elements is a combinator provided by QuickCheck that takes a list of type to generate and picks a random of those elements
                             -- wallets is a list; genWallet will pick either w1 or w2
genNonNeg :: Gen Integer -- generates a random non-negative integer
genNonNeg = getNonNegative <$> arbitrary -- arbitrary gets a random  integer and getNonNegative extracts it; provided by QuickCheck library

tokenAmt :: Integer
tokenAmt = 1_000 -- constant token amount to specify inital ditribution to wallets in the simulation for both tokens

prop_TS :: Actions TSModel -> Property -- define QuickCheck property to link the entire model to QuickCheck
prop_TS = withMaxSuccess 100 . propRunActionsWithOptions -- function part of Plutus.Contract.Test
    (defaultCheckOptions & emulatorConfig .~ EmulatorConfig (Left d) def def) -- specify initial coin distribution
    instanceSpec -- this argument is defined above with the list of contract instance to run; what will be tested?
    (const $ pure True) -- no additional checks are added; return a pure True makes it an applicative
  where
    d :: InitialDistribution
    d = Map.fromList $ [ ( w
                         , lovelaceValueOf 1_000_000_000 <> -- give each wallet 1000 ADA and 
                           mconcat [assetClassValue t tokenAmt | t <- Map.elems tokens]) --  1000 tokens of both A & B
                       | w <- wallets
                       ]

test :: IO ()
test = quickCheck prop_TS
