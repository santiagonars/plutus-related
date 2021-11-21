module Main
    ( main
    ) where

import qualified Spec.Model             -- imports the modules that contain the test
import qualified Spec.ModelWithClose
import qualified Spec.Trace             -- imports the modules that contain the test
import qualified Spec.TraceWithClose
import           Test.Tasty

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "token sale"
    [ Spec.Trace.tests           -- uses the test defined for the emulatortrace
    , Spec.TraceWithClose.tests  
    , Spec.Model.tests           -- uses the test defined for the model
    , Spec.ModelWithClose.tests
    ]
