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

findAndReplace :: ReplacementsTable -> Text -> Maybe [Inline]
findAndReplace [] _ = Nothing
findAndReplace ((find,replaceWith):fs) text =
  if Prelude.null found
     then findAndReplace fs text
     else Just $ replaceAll 0 (T.length find) text replaceWith found
  where
    found = indices find text

replaceAll :: Int -> Int -> Text -> [Inline] -> [Int] -> [Inline]
-- No more indices -> return rest a single Str
replaceAll begin len haystack replaceWith [] = 
  let rest = T.drop begin haystack
  in if T.null rest
        then []
        else [ Str rest ]
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
        then replaceWith ++ (replaceAll (idx + len) len haystack replaceWith idxs)
        else ((Str prefix) : replaceWith) ++ replaceAll (idx + len) len haystack replaceWith idxs
