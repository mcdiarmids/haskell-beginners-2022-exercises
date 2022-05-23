{-# LANGUAGE InstanceSigs #-}

{- |
Module                  : Lecture4
Copyright               : (c) 2021-2022 Haskell Beginners 2022 Course
SPDX-License-Identifier : MPL-2.0
Maintainer              : Haskell Beginners 2022 Course <haskell.beginners2022@gmail.com>
Stability               : Stable
Portability             : Portable

Exercises for the Lecture 4 of the Haskell Beginners course.

In this task you're going to implement a complete Haskell Program!

The purpose of the program is to read information about various
product trades from a file, calculate some stats about buys and sells
and pretty-print them in the terminal.

The content of the file looks like this:


Name,Type,Amount
Apples,Sell,25
Tomatoes,Sell,10
Pineapples,Buy,50


Specifically:

  1. The first line contains names of columns.
  2. Each line contains exactly 3 comma-separated values.
  3. The first value is a name of a product: a non-empty string
     containing any characters except comma.
  4. Second value is the type of trade. It' either a "Buy" or "Sell" string.
  5. The last, third value, is a non-negative integer number: the cost
     of the product.
  6. Each value might be surrounded by any amount of spaces.
  7. You don't need to trim spaces in the product name. But you need
     to parse the other two values even if they contain leading and
     trailing spaces.

Your program takes a path to a file and it should output several stats
about all the trades. The list of parameters to output is always the
same. Only values can change depending on file content.

For example, for the file content above, the program should print the following:


Total positions:       : 3
Total final balance    : -15
Biggest absolute cost  : 50
Smallest absolute cost : 10
Max earning            : 25
Min earning            : 10
Max spending           : 50
Min spending           : 50
Longest product name   : Pineapples


To run the program, use the following command for specifying the
path (the repository already contains a small test file):


cabal run lecture4 -- test/products.csv


You can assume that the file exists so you don't need to handle such
exceptional situations. But you get bonus points for doing so :)

However, the file might contain data in an invalid format.
All possible content errors:

  * There might not be the first line with column names
  * Names of columns might be different
  * Each line can have less than 3 or more than 3 values
  * The product name string can be empty
  * The second value might be different from "Buy" or "Sell"
  * The number can be negative or not integer or not even a number

In this task, for simplicity reasons, you don't need to report any
errors. You can just ignore invalid rows.

Exercises for Lecture 4 also contain tests and you can run them as usual.
-}

module Lecture4
    ( -- * Main running function
      main

      -- * Types
    , TradeType (..)
    , Row (..)
    , MaxLen (..)
    , Stats (..)

      -- * Internal functions
    , parseRow
    , rowToStats
    , combineRows
    , displayStats
    , calculateStats
    , printProductStats
    ) where

import Data.List.NonEmpty (NonEmpty (..), fromList)
import Data.Semigroup (Max (..), Min (..), Semigroup (..), Sum (..))
import Text.Read (readMaybe)
import Data.Maybe (mapMaybe)
import System.Environment (getArgs)
import Data.Char (isSpace)
import Data.List (dropWhileEnd)

{- In this exercise, instead of writing the entire program from
scratch, you're offered to complete the missing parts.

Let's use this task as an opportunity to learn how to solve real-world
problems in a strongly-typed, functional, algebraic way.

First, let's define data types to represent a single row of our file.
-}

data TradeType
    = Buy
    | Sell
    deriving (Show, Eq, Read)

data Row = Row
    { rowProduct   :: String
    , rowTradeType :: TradeType
    , rowCost      :: Int
    } deriving (Show, Eq)

{-
Now you can implement a function that takes a String containing a single row and
parses it. The only catch here is that the input string might have format
errors. We will simply return an optional result here.

🕯 HINT: You may need to implement your own function to split 'String' by 'Char'.

🕯 HINT: Use the 'readMaybe' function from the 'Text.Read' module.
-}

parseRow :: String -> Maybe Row
parseRow r = case wordsBy ',' r of
   [p, t, c] -> do
      p' <- parseProduct p
      t' <- parseTradeType $ trim t
      c' <- parseCost $ trim c
      pure $ Row p' t' c'
   _ -> Nothing

wordsBy :: Char -> String -> [String]
wordsBy c s = case dropWhile (== c) s of
   "" -> []
   s' -> w : wordsBy c s''
      where (w, s'') = break (== c) s'

trim :: String -> String
trim = dropWhileEnd isSpace . dropWhile isSpace

parseProduct :: String -> Maybe String
parseProduct "" = Nothing
parseProduct s = Just s

parseTradeType :: String -> Maybe TradeType
parseTradeType "Buy" = Just Buy
parseTradeType "Sell" = Just Sell
parseTradeType _ = Nothing

parseCost :: String -> Maybe Int
parseCost s = case readMaybe s of
   Just n -> if n < 0 then Nothing else Just n
   _ -> Nothing


{-
We have almost all we need to calculate final stats in a simple and
elegant way.

To use algebraic abstractions for this problem, let's introduce a
custom data type for finding the longest product name.
-}

newtype MaxLen = MaxLen
    { unMaxLen :: String
    } deriving (Show, Eq)

{-
We can implement the 'Semigroup' instance for this data type that will
choose between two strings. The instance should return the longest
string.

If both strings have the same length, return the first one.
-}
instance Semigroup MaxLen where
   (<>) :: MaxLen -> MaxLen -> MaxLen
   MaxLen x <> MaxLen y
      | length x < length y = MaxLen y
      | otherwise = MaxLen x

{-
It's convenient to represent our stats as a data type that has
'Semigroup' instance so we can easily combine stats for multiple
lines.
-}

data Stats = Stats
    { statsTotalPositions :: Sum Int
    , statsTotalSum       :: Sum Int
    , statsAbsoluteMax    :: Max Int
    , statsAbsoluteMin    :: Min Int
    , statsSellMax        :: Maybe (Max Int)
    , statsSellMin        :: Maybe (Min Int)
    , statsBuyMax         :: Maybe (Max Int)
    , statsBuyMin         :: Maybe (Min Int)
    , statsLongest        :: MaxLen
    } deriving (Show, Eq)

{-
The 'Stats' data type has multiple fields. All these fields have
'Semigroup' instances. This means that we can implement a 'Semigroup'
instance for the 'Stats' type itself.
-}

instance Semigroup Stats where
   (<>) :: Stats -> Stats -> Stats
   (Stats tp1 ts1 amx1 amn1 smx1 smn1 bmx1 bmn1 l1) <> (Stats tp2 ts2 amx2 amn2 smx2 smn2 bmx2 bmn2 l2) =
      Stats (tp1 <> tp2) (ts1 <> ts2) (amx1 <> amx2) (amn1 <> amn2) (smx1 <> smx2) (smn1 <> smn2) (bmx1 <> bmx2) (bmn1 <> bmn2) (l1 <> l2)


{-
The reason for having the 'Stats' data type is to be able to convert
each row independently and then combine all stats into a single one.

Write a function to convert a single 'Row' to 'Stats'. To implement this
function, think about how final stats will look like if you have only a single
row in the file.

🕯 HINT: Since a single row can only be 'Buy' or 'Sell', you can't
   populate both sell max/min and buy max/min values. In that case,
   you can set the corresponding field to 'Nothing'.
-}

rowToStats :: Row -> Stats
rowToStats r = Stats
   { statsTotalPositions = Sum 1
   , statsTotalSum = Sum $ if rowTradeType r == Buy then negate $ rowCost r else rowCost r
   , statsAbsoluteMax = Max $ rowCost r
   , statsAbsoluteMin = Min $ rowCost r
   , statsSellMax = if rowTradeType r == Sell then Just $ Max $ rowCost r else Nothing
   , statsSellMin = if rowTradeType r == Sell then Just $ Min $ rowCost r else Nothing
   , statsBuyMax = if rowTradeType r == Buy then Just $ Max $ rowCost r else Nothing
   , statsBuyMin = if rowTradeType r == Buy then Just $ Min $ rowCost r else Nothing
   , statsLongest = MaxLen $ rowProduct r
   }

{-
Now, after we learned to convert a single row, we can convert a list of rows!

However, we have a minor problem. Our 'Stats' data type doesn't have a
'Monoid' instance and it couldn't have it! One reason for this is that
there's no sensible "empty" value for the longest product name. So we
simply don't implement the 'Monoid' instance for 'Stats'.

But the list of rows might be empty and we don't know what to return
on empty list!

The solution of this problem is to propagate handling of this
situation upstream. In our type signature we will require to accept a
non-empty list of rows.

We can use the 'NonEmpty' data type from the 'Data.List.NonEmpty'
module for this purpose. 'NonEmpty' is like 'List1' from Lecture 3
exercises (remember that type?) but with the different constructor.

Have a look at the 'sconcat' function from the 'Semigroup' typeclass to
implement the next task.
-}

combineRows :: NonEmpty Row -> Stats
combineRows = sconcat . fmap rowToStats

{-
After we've calculated stats for all rows, we can then pretty-print
our final result.

If there's no some value (for example, there were not "Buy" products),
you can return string "no value"
-}

displayStats :: Stats -> String
displayStats s = unlines
   [ "Total positions        : " ++ show (getSum $ statsTotalPositions s)
   , "Total final balance    : " ++ show (getSum $ statsTotalSum s)
   , "Biggest absolute cost  : " ++ show (getMax $ statsAbsoluteMax s)
   , "Smallest absolute cost : " ++ show (getMin $ statsAbsoluteMin s)
   , "Max earning            : " ++ displayMaybe getMax (statsSellMax s)
   , "Min earning            : " ++ displayMaybe getMin (statsSellMin s)
   , "Max spending           : " ++ displayMaybe getMax (statsBuyMax s)
   , "Min spending           : " ++ displayMaybe getMin (statsBuyMin s)
   , "Longest product name   : " ++ unMaxLen (statsLongest s)
   ]
   where
      displayMaybe :: Show b => (a -> b) -> Maybe a -> String
      displayMaybe f = maybe "no value" $ show . f


{-
Now, we definitely have all the pieces in places! We can write a
function that takes content of the file (the full content with multiple lines)
and converts it to pretty-printed stats.

The only problem here is that after parsing a file we might end with
an empty list of rows but our 'combineRows' function requires to have
a non-empty list. In that case, you can return a string saying that
the file doesn't have any products.

🕯 HINT: Ideally, the implementation of 'calculateStats' should be just a
   composition of several functions. Use already implemented functions, some
   additional standard functions and maybe introduce helper functions if you need.

🕯 HINT: Have a look at 'mapMaybe' function from 'Data.Maybe' (you may need to import it).
-}

calculateStats :: String -> String
calculateStats c = case mapMaybe parseRow $ lines c of
  [] -> "file has no products"
  (r:rs) -> displayStats . combineRows $ fromList (r:rs)

{- The only thing left is to write a function with side-effects that
takes a path to a file, reads its content, calculates stats and prints
the result.

Use functions 'readFile' and 'putStrLn' here.
-}

printProductStats :: FilePath -> IO ()
printProductStats p = readFile p >>= putStrLn . calculateStats

{-
Okay, I lied. This is not the last thing. Now, we need to wrap
everything together. In our 'main' function, we need to read
command-line arguments that contain a path to a file and then call
'printProductStats' if arguments contain a path. If they are invalid,
you can print an error message.

Use 'getArgs' function from the 'System.Environment' module to read
CLI args:

https://hackage.haskell.org/package/base-4.16.0.0/docs/System-Environment.html#v:getArgs
-}

main :: IO ()
main = do
   args <- getArgs
   case args of 
     [p] -> printProductStats p
     _   -> putStrLn "supply the path to the products file"


{-
And that's all!

You solved a difficult problem in functional style! 🥳
You should be proud of yourself 🤗

====================================================

For an extra challenge, you can make sure that your solution is optimally lazy
and streaming. The course contains an additional executable
"generate-many-products" that generates a 2GB file of products.

> NOTE: Make sure you have enough disk space before running the generator and
> make sure to delete the file afterwards to not to waste space

To run the executable that produces a huge file, use the following command:


cabal run generate-many-products


Laziness in Haskell is a double-edged sword. On one hand, it leads to
more composable code and automatic streaming in most cases. On the
other hand, it's easy to introduce space leaks if you're not being
careful.

The naive and straightforward implementation of this task most likely
contains space leaks. To implement the optimal streaming and laziness
solution, consider doing the following improvements:

  1. Enable the {-# LANGUAGE StrictData #-} pragma to this module.

     * Fields in Haskell data types are lazy by default. So, when
       combining 'Stats' with <>, fields on the new 'Stats' value are
       not fully-evaluated. Enabling 'StrictData' fixes this.

  2. Make sure you traverse the list of all products only once in each
     function. In that case, due to laziness, composition of such
     functions will traverse the list only once as well.

     * You can traverse each separate line multiple times because each
       individual line in the file is short and traversing it only
       once won't bring lots of performance improvements.

  3. Don't use 'length' to calculate the total number of rows.

  4. Replace 'sconcat' in 'combineRows' with foldl' or manual recursive
     function using {-# LANGUAGE BangPatterns #-} and strict
     accumulator of type 'Stats'.

     * 'sconcat' is a lazy function. So, even if you force every field
       of the 'Stats' data type with 'StrictData', it won't make a
       difference if you don't force the 'Stats' accumulator itself.

  5. Combine fields of type 'Maybe' in the 'Stats' data type with a
     stricter version of '<>'.

     * The 'Semigroup' instance for 'Maybe' (that you've probably used
       for implementing the 'Semigroup' instance for 'Stats') is lazy
       and doesn't force values inside 'Just' constructors. To fix
       this problem, you can use a custom function that combines two
       values of type 'Maybe' and pattern matches on @Just !x@ to
       ensure that values inside 'Just' are fully-evaluated on each
       step.


You can check memory usage of your program by running `htop` in a
separate terminal window. If you see that the memory usage doesn't
grow indefinitely by eating all your RAM, it means that the solution
requires constant-size memory.

Additionally, on Linux, you can run the following command to see the
actual size of required memory during your program execution:


/usr/bin/time -v cabal run lecture4 -- test/gen/big.csv


You can expect the optimal lazy solution to run in ~20 minutes and
consume ~200 MB of RAM. The numbers are not the best and there's lots
of room for optimization! But at least you've managed to implement a
streaming solution using only basic Haskell 🤗

-}
