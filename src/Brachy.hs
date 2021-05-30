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
module Brachy
where

import Text.Pandoc.Definition

import Data.Text as T
import Data.Text.Internal.Search

-- Monadic version of filterMap, very inefficient
filterMapM :: Monad m => (x -> m (Maybe y)) -> [x] -> m [y]
filterMapM _ []  = return []
filterMapM fun (x:xs) = do
  maybeY <- fun x
  ys <- filterMapM fun xs
  case maybeY of
        Just y -> return (y:ys)
        Nothing -> return ys

-- A string to be found and its replacement
type Replacement = (Text, [Inline])

-- A list of replacements. The first one found is used
type ReplacementsTable = [ Replacement ]

-- Search the given Text for all string and replace them. The order of
-- processing matters: A pattern earlier in the table overrides an earlier
-- occurence of a later pattern.
-- In order not process the replacements, each part of the list needs to be
-- marked as processed (Right) or not (Left).
-- Thus, the processing consists of three steps: Marking the whole text as
-- unprocessed, processing the unprocessed parts one replacement pair at a
-- time, reconstructing the inlines from processed and unprocessed text.
findAndReplace :: ReplacementsTable -> Text -> [Inline]
findAndReplace reps text = reconstructInlines $ farInternal reps [Left text]

-- Reconstruct a list of inlines from an internal processing list. This is
-- a kind of concatMap
reconstructInlines :: [Either Text [Inline]] -> [Inline]
reconstructInlines [] = []
reconstructInlines ((Left t) : rest) = Str t : reconstructInlines rest
reconstructInlines ((Right []) : rest) = reconstructInlines rest
reconstructInlines ((Right (i1:r1)):rest) = i1 : reconstructInlines ((Right r1):rest)

-- Internal replacement function: Replace all occurences of the first pair in
-- all unprocessed parts of the list, returning a new list.
farInternal :: ReplacementsTable -> [Either Text [Inline]] -> [Either Text [Inline]]
farInternal [] work = work
farInternal ((find,replaceWith):fs) work = farInternal fs worked
  where
    worked = farOnePair find replaceWith work

-- Process one find/replace pair on all unprocessed occurences in work
farOnePair :: Text -> [Inline] -> [Either Text [Inline]] -> [Either Text [Inline]]
farOnePair _ _ [] = []
farOnePair find replaceWith ((Right r):rest) = (Right r) : farOnePair find replaceWith rest
farOnePair find replaceWith ((Left l):rest) = (farOneText find replaceWith l) ++ (farOnePair find replaceWith rest)

-- Process one find/replace pair on a single Text
farOneText :: Text -> [Inline] -> Text -> [Either Text [Inline]]
farOneText find replaceWith haystack =
  let found = indices find haystack
  in replaceAll 0 (T.length find) haystack replaceWith found

-- Replace all occurences of a text at a give list of positions with the
-- given replacement
replaceAll :: Int -> Int -> Text -> [Inline] -> [Int] -> [Either Text [Inline]]
-- No more indices -> return rest a single unprocessed Text
replaceAll begin len haystack replaceWith [] = 
  let rest = T.drop begin haystack
  in if T.null rest
        then []
        else [ Left rest ]
-- begin characters of haystack have been processed, next match of length len is at idx
-- 2 1 abcdefgh ... (5:...) -> xx cde r 
replaceAll begin len haystack replaceWith (idx:idxs) = 
  let
    -- drop begin characters
    currentHaytack = T.drop begin haystack
    -- take (idx - begin) characters -> prefix / needle+rest
    (prefix, needle_rest) = T.splitAt (idx - begin) currentHaytack
    rest = T.drop len needle_rest
  in if T.null prefix
        then (Right replaceWith) : (replaceAll (idx + len) len haystack replaceWith idxs)
        else (Left prefix) : (Right replaceWith) : (replaceAll (idx + len) len haystack replaceWith idxs)
