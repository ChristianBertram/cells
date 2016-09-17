-- This is an implementation of elementary cellular automata, with a 2-color,
-- range 1 (3 neighbor cell on top) rule space. This means 256 different rules
-- exist.

import Data.Maybe
import Control.Monad
import Text.Read

type Line = [Bool]

data Rule = Rule { rule7 :: Bool -- Color of field with neighbors True-True-True
                 , rule6 :: Bool -- Color of field with neighbors True-True-False
                 , rule5 :: Bool -- Color of field with neighbors True-False-True
                 , rule4 :: Bool -- Color of field with neighbors True-False-False
                 , rule3 :: Bool -- Color of field with neighbors False-True-True
                 , rule2 :: Bool -- Color of field with neighbors False-True-False
                 , rule1 :: Bool -- Color of field with neighbors False-FalseTrue
                 , rule0 :: Bool -- Color of field with neighbors False-False-False
                 } deriving Show

ruleGet :: Int -> Rule -> Bool
ruleGet 7 = rule7
ruleGet 6 = rule6
ruleGet 5 = rule5
ruleGet 4 = rule4
ruleGet 3 = rule3
ruleGet 2 = rule2
ruleGet 1 = rule1
ruleGet 0 = rule0
ruleGet _ = const False

boolToInt :: Bool -> Int
boolToInt False = 0
boolToInt True  = 1

intToBool :: Int -> Bool
intToBool x
  | x <= 0 = False
  | x >  0 = True

ruleResult :: Bool -> Bool -> Bool -> Rule -> Bool
ruleResult a b c = ruleGet (4 * boolToInt a + 2 * boolToInt b + boolToInt c)

nextLine :: Bool -> Line -> Rule -> Line
nextLine last (a:b:xs) rule = ruleResult last a b     rule : nextLine a (b:xs) rule
nextLine last [a]      rule = ruleResult last a False rule : nextLine a       [] rule
nextLine _    []       rule = []

lineToString :: Line -> String
lineToString [] = ""
lineToString (False:xs) = ' ' : lineToString xs
lineToString (True:xs)  = '■' : lineToString xs

getInt :: Int -> Int -> IO Int
getInt from to = do
  input <- getLine
  let int' = readMaybe input :: Maybe Int

  case int' of
    Nothing -> do
      putStr "\nNot an integer. Try again: "
      getInt from to
    Just int ->
      if from < int && int <= to
        then return int
        else do
          putStr $ "\nRule not from " ++ show from ++ " to " ++ show to ++ ". Try again: "
          getInt from to

genLoop :: Int -> Line -> Rule -> IO ()
genLoop it line rule = do
  let line' = nextLine False line rule
  putStrLn $ lineToString line'
  Control.Monad.when (it <= length line `div` 2) $ genLoop (it+1) line' rule

main :: IO ()
main = do
  putStr "Enter how many itterations should be completed (Don't go above half of the character width of your terminal): "
  its <- getInt 0 maxBound
  putStr "\nEnter a rule from 0 to 255: "
  ruleInt <- getInt 0 255
  let rule = Rule { rule7 = intToBool $ div (mod ruleInt 256) 128
                  , rule6 = intToBool $ div (mod ruleInt 128) 64
                  , rule5 = intToBool $ div (mod ruleInt 64) 32
                  , rule4 = intToBool $ div (mod ruleInt 32) 16
                  , rule3 = intToBool $ div (mod ruleInt 16) 8
                  , rule2 = intToBool $ div (mod ruleInt 8) 4
                  , rule1 = intToBool $ div (mod ruleInt 4) 2
                  , rule0 = intToBool $ mod ruleInt 2
                  }

  let firstLine = replicate (its-1) False ++ [True] ++ replicate (its-1) False
  putStrLn $ lineToString firstLine
  genLoop 2 firstLine rule
