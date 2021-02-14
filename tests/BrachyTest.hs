{-
Copyright (c) 2021 Lars Krueger

Permission is hereby granted, free of charge, to any person obtaining
a copy of this software and associated documentation files (the
"Software"), to deal in the Software without restriction, including
without limitation the rights to use, copy, modify, merge, publish,
distribute, sublicense, and/or sell copies of the Software, and to
permit persons to whom the Software is furnished to do so, subject to
the following conditions:

The above copyright notice and this permission notice shall be included
in all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.
IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY
CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT,
TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE
SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
-}
{-# LANGUAGE OverloadedStrings #-}
module Main
( main
) where

import Brachy
import Test.HUnit
import System.Exit(exitFailure)
import Control.Monad(when)
import Text.Pandoc.Definition
import Data.Text

incrementOdd :: Int -> IO (Maybe Int)
incrementOdd x = do
  if odd x
     then return $ Just (x+1)
     else return Nothing

alwaysNothing :: Int -> IO (Maybe Int)
alwaysNothing x = return Nothing

testFilterMap :: (Int -> IO (Maybe Int)) -> [Int] -> [Int] -> Assertion
testFilterMap fun input expected = do
  result <- filterMapM fun input
  result @?= expected

testFar :: ReplacementsTable -> Text -> Maybe [Inline] -> Assertion
testFar reps text expected = do
  let result = findAndReplace reps text
  result @?= expected

testReps :: ReplacementsTable
testReps =
  [ ( ">>>", [ Emph [ Str "fsqr" ] ] )
  , ( ">>",  [ Str "fqr" ] )
  ]

testReplace :: Int -> Text -> [Inline] -> [Int] -> [Inline] -> Assertion
testReplace len needle replaceWith idxs expected = do
  let result = replaceAll 0 len needle replaceWith idxs
  result @?= expected

tests :: Test
tests = TestList
  [ "filterMapM, empty input"   ~: TestCase $ testFilterMap incrementOdd [] []
  , "filterMapM, empty result"  ~: TestCase $ testFilterMap alwaysNothing [1,2,3] []
  , "filterMapM, normal list"  ~: TestCase $ testFilterMap incrementOdd [1,2,3,4,5,6,100,101] [2,4,6,102]
  , "replaceAll, one found, at start" ~: TestCase $ testReplace 2 "abcdef" [ Str "xy" ] [0] [ Str "xy", Str "cdef"]
  , "replaceAll, one found, middle" ~: TestCase $ testReplace 2 "abcdef" [ Str "xy" ] [3] [ Str "abc", Str "xy", Str "f"]
  , "replaceAll, one found, end" ~: TestCase $ testReplace 2 "abcdef" [ Str "xy" ] [4] [ Str "abcd", Str "xy"]
  , "replaceAll, one found, full string" ~: TestCase $ testReplace 2 "ab" [ Str "xy" ] [0] [ Str "xy"]
  , "replaceAll, multiple found" ~: TestCase $ testReplace 2 "ab..def..gh..kl" [ Str "xy" ] [2,7,11] [ Str "ab", Str "xy", Str "def", Str "xy", Str "gh", Str "xy", Str "kl"]
  , "findAndReplace, empty table" ~: TestCase $ testFar [] "test string" Nothing
  , "findAndReplace, general case" ~: TestCase $ testFar testReps "abc>>def" $ Just [Str "abc", Str "fqr", Str "def"]
  , "findAndReplace, find first in middle" ~: TestCase $ testFar testReps "abc>>>def" $ Just [Str "abc", Emph [ Str "fsqr" ], Str "def"]
  , "findAndReplace, find in first place" ~: TestCase $ testFar testReps ">>>def" $ Just [Emph [ Str "fsqr" ], Str "def"]
  , "findAndReplace, find in last place" ~: TestCase $ testFar testReps "abc>>>" $ Just [Str "abc", Emph [ Str "fsqr" ] ]
  ]

main :: IO ()
main = do
  (Counts _ _ _ f) <- runTestTT tests
  when (f /= 0) exitFailure
  return ()
