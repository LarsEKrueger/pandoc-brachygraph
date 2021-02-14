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
where

import Control.Monad
import Control.Monad.Extra
import Data.Aeson
import Data.String
import Data.Text as T
import Data.Text.Internal.Search
import Text.Pandoc.JSON as J
import Text.Pandoc.Definition
import Text.Pandoc.Walk
import qualified Data.ByteString.Lazy as BL
import System.IO (hPutStrLn, stderr)

import Brachy

-- Print a string to stderr
errorMsg :: String -> IO ()
errorMsg = hPutStrLn stderr

-- Build a replacement table from Meta data, giving error messages
replacementTable :: Meta -> IO ReplacementsTable
replacementTable meta = do
  case lookupMeta "brachygraph" meta of
       Just (MetaList a) -> replacementsFromMeta a
       _ -> do
         errorMsg "No brachygraph data found in meta block"
         return []
  where
    replacementsFromMeta :: [MetaValue] -> IO ReplacementsTable
    replacementsFromMeta vals = filterMapM maybeReplacement vals

    maybeReplacement :: MetaValue -> IO (Maybe Replacement )
    maybeReplacement val = do
      case val of
           MetaList [MetaInlines [Str find], MetaInlines replace] -> return $ Just (find, replace)
           _ -> do
             errorMsg ("Cannot interpret brachygraph meta data: " ++ show val)
             return Nothing

-- This might split a single Inline into multiple Inlines.
farSingleInline :: ReplacementsTable -> Inline -> IO [Inline]
farSingleInline reps (Str s)
  = case findAndReplace reps s of
         Just rep -> return rep
         _ -> return [Str s]
farSingleInline _ x = return [x]

-- Find and replace in lists of inlines.
--
-- Walk will handle the recursive processing
farInlines :: ReplacementsTable -> [Inline] -> IO [Inline]
farInlines reps inlines = concatMapM (farSingleInline reps) inlines

main :: IO ()
main = do
  txt <- BL.getContents
  let input@(Pandoc meta bs) = either error id $ eitherDecode' txt
  replacements <- replacementTable meta
  outputBs <- walkM (farInlines replacements) bs
  BL.putStr $ encode $ Pandoc meta outputBs

