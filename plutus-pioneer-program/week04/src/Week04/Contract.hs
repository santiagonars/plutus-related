{-# LANGUAGE OverloadedStrings #-}  -- because of this, it makes Strings of more general types, resulting in having to specify a type to use for logInfo
{-# LANGUAGE TypeApplications  #-}  -- allows to specify for polymorphic functions like logInfo which type type is used
{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE TypeOperators     #-} -- to use the type operator .\/ 

module Week04.Contract where

import Control.Monad.Freer.Extras as Extras
import Data.Functor               (void)
import Data.Text                  (Text, unpack)
import Data.Void                  (Void)
import Plutus.Contract            as Contract
import Plutus.Trace.Emulator      as Emulator
import Wallet.Emulator.Wallet

-- Contract w s e a     -- The type Contract takes 4 type parameters
-- EmulatorTrace a

-- for w, use () to not write a state or log messages
-- for s, no endpoints are needed so just use Empty which means no endpoints are available
-- for e, use Text for error messages; Text is a more efficient type than String for textual data / large amounts fo data
-- for a, use () as no resuklt is desired
myContract1 :: Contract () Empty Text () 
myContract1 = do
    void $ Contract.throwError "BOOM!" -- throwError allows throwing an exception of type e, which is Text; literal string will be passed as Text
    Contract.logInfo @String "hello from the contract"  -- can use anything that is serializable to json for logging
                                                        -- Use logInfo from Plutus.Contract to use log info from inside the contract
myTrace1 :: EmulatorTrace () -- define the trace that tests contract
myTrace1 = void $ activateContractWallet (Wallet 1) myContract1 -- activate contract; activateContractWallet would result a Handle but the result os throw away

test1 :: IO () -- to test the contract
test1 = runEmulatorTraceIO myTrace1

-- for e, use Void which is a Haskell type defined in Haskell.Void that has no value so it can't raise anyu exception errors; it's different from () which has one value
myContract2 :: Contract () Empty Void ()
myContract2 = Contract.handleError -- need to specify the Handle and the contract to run
    (\err -> Contract.logError $ "caught: " ++ unpack err)  -- unpack converts Text to String; no need to use @String because unpack tells it; it is defined in Data.Text
    myContract1 -- the contract to handle errors for

myTrace2 :: EmulatorTrace ()
myTrace2 = void $ activateContractWallet (Wallet 1) myContract2

test2 :: IO ()
test2 = runEmulatorTraceIO myTrace2

-- Example for s which specifies the endpoints
-- Use MySchema for s which is just the conventional name to use
-- Endpoint is a type; "foo" is a type level String which is a string used at the type level; this is Haskell extension enabled by Datakinds
-- .\/ is a type operator to chain endpoints together; takes one or more types and combines them to a new type
type MySchema = Endpoint "foo" Int .\/ Endpoint "bar" String
 
myContract3 :: Contract () MySchema Text ()
myContract3 = do           -- endpoint results in a Contract of type a, as long as MySchema s has an Endpoint with parameters l (for label) and a (for result type)
    n <- endpoint @"foo"   -- endpoint is a monadic computation that will block contract execution and wait for a value to be provided of type Int
    Contract.logInfo n     -- n is serializable so it can just be used as is; compiler knows it's an Int
    s <- endpoint @"bar"
    Contract.logInfo s

myTrace3 :: EmulatorTrace ()
myTrace3 = do
    h <- activateContractWallet (Wallet 1) myContract3  -- to invoke the nedpoint friom the trace, a Handle is needed wich is bind to h
    callEndpoint @"foo" h 42  -- callEndpoint takes the label, the handle, and the Int
    callEndpoint @"bar" h "Haskell"

test3 :: IO ()
test3 = runEmulatorTraceIO myTrace3

-- Example for w which specifies
-- w must be an instance of the class type called Monoid; a example is a list of Ints
myContract4 :: Contract [Int] Empty Text ()
myContract4 = do
    void $ Contract.waitNSlots 10
    tell [1]  -- use tell to write to the state to this log; tell comes from the Monad Writer class and Contract is an instance of it
    void $ Contract.waitNSlots 10
    tell [2]
    void $ Contract.waitNSlots 10

myTrace4 :: EmulatorTrace ()
myTrace4 = do
    h <- activateContractWallet (Wallet 1) myContract4

    void $ Emulator.waitNSlots 5 
    xs <- observableState h   -- Use observableState to read the state of the running contract; Just takes a Handle as argument; returns the state
    Extras.logInfo $ show xs  -- this should be before the first tell; before the tell, the state us mempty of the Monoid for []

    void $ Emulator.waitNSlots 10 
    ys <- observableState h
    Extras.logInfo $ show ys  -- this should be after the first tell but before the second tell; the state will contain [1]

    void $ Emulator.waitNSlots 10  
    zs <- observableState h
    Extras.logInfo $ show zs  -- this should be after the second tell has happened; the state will contain [1, 2]; each tell with uses mappend of the Monoid used as w

test4 :: IO ()
test4 = runEmulatorTraceIO myTrace4
