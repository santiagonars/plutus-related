module Week04.Writer where

import Control.Monad -- for helper function liftM
import Week04.Monad
-- Thesis: The idea of creating  computations that can also produce lock output
data Writer a = Writer a [String] -- constructor takes 2 arguments; Writer a & a list of lock messages
    deriving Show

number :: Int -> Writer Int
number n = Writer n $ ["number: " ++ show n] -- given n, produces that number and as log output

tell :: [String] -> Writer () -- gets a list of lock messages and produces a Writer unit with the log messages
tell = Writer ()

-- combine 3 computations that add the 3 Int and concatenate the log message of each one to get an overall message
foo :: Writer Int -> Writer Int -> Writer Int -> Writer Int 
foo (Writer k xs) (Writer l ys) (Writer m zs) =
  let
    s = k + l + m
    Writer _ us = tell ["sum: " ++ show s] -- same as 'Write () us' where us is the log message
  in
    Writer s $ xs ++ ys ++ zs ++ us

-- given a Writer that produces an a and a function from a to Writer b, are combine to produce a Writer b
bindWriter :: Writer a -> (a -> Writer b) -> Writer b
bindWriter (Writer a xs) f =
  let
    Writer b ys = f a -- example: Writer b ys = (\k -> Writer 2 ["number: 2"]) 1 = Writer 2 ["number: 2"]
  in
    Writer b $ xs ++ ys -- binds log messages from a and b

-- The bindWriter in foo' has a logic of sequencing log outputs producing computations by combining the log outputs
foo' :: Writer Int -> Writer Int -> Writer Int -> Writer Int
foo' x y z = x `bindWriter` \k ->
             y `bindWriter` \l ->
             z `bindWriter` \m ->
             let s = k + l + m
             in tell ["sum: " ++ show s] `bindWriter` \_ ->
                Writer s []

-- Monad implementation using 'do' 
foo'' :: Writer Int -> Writer Int -> Writer Int -> Writer Int
foo'' x y z = do
    s <- threeInts x y z
    tell ["sum: " ++ show s]
    return s

-- Monad implementation (alternative approach) using explicit binding
foo''' :: Writer Int -> Writer Int -> Writer Int -> Writer Int
foo''' x y z = threeInts x y z           >>= \s ->
               tell ["sum: " ++ show s]  >>
               return s

-- Standard Functor instance for the Monad Writer type
instance Functor Writer where
    fmap = liftM -- makes use of return and bind to implement fmap; liftM is a helper function from the prelude from Control.Monad

-- Standard Applicative instance for the Monad Writer type
instance Applicative Writer where
    pure = return
    (<*>) = ap

-- A Monad instance for the Writer type
instance Monad Writer where
    return a = Writer a []
    (>>=) = bindWriter
