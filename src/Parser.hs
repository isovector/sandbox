{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Parser where

import Debug.Trace
import Data.Aeson.Encode.Pretty
import Data.String.Conv (toS)
import BasePrelude hiding (Any (..), optional, many)
import Data.Aeson
import Text.HTML.Scalpel
import Text.ParserCombinators.Parsec hiding (Line (..))

type P a = GenParser Char () a

data Line = Words String
          | Action String
          deriving (Show, Generic, ToJSON)

ltActionParser :: P Line
ltActionParser = do
  x <- fmap Action . between (char '(') (char ')') . many $ noneOf ")"
  optional spaces
  return x

ltWordsParser :: P Line
ltWordsParser = do
  c <- noneOf "("
  x <- many (noneOf "(")
  optional spaces
  let z = c : x
  return . Words . reverse . dropWhile (== ' ') $ reverse z

actionParser :: P [Line]
actionParser = many1 $ choice [ltActionParser, ltWordsParser]

data Character = Ross
               | Phoebe
               | Rachel
               | Joey
               | Monica
               | Chandler
               | Other String
               deriving (Show, Eq, Ord, Generic, ToJSON)

readCharacter :: String -> Character
readCharacter "Ross"     = Ross
readCharacter "Phoebe"   = Phoebe
readCharacter "Rachel"   = Rachel
readCharacter "Joey"     = Joey
readCharacter "Monica"   = Monica
readCharacter "Chandler" = Chandler
readCharacter x = Other x

data Utterance = Utterance
  { character :: Character
  , line :: [Line]
  }
  deriving (Generic, Show, ToJSON)

type S a = Scraper String a

lineParser :: S Utterance
lineParser = do
  raw <- text Any
  let char = readCharacter $ takeWhile (/= ':') raw
  let line = map changeNs . dropWhile (liftM2 (||) (== ':') (== ' '))
                          $ dropWhile (/= ':') raw
  let Right lts = case line of
                    "" -> Right []
                    xs -> parse actionParser "" (xs)
  return $ Utterance char lts
 where
  changeNs '\n' = ' '
  changeNs x = x


main :: IO ()
main = do
  let ok = [0 .. 15] ++ [17 .. 20] ++ [22 .. 29] ++ [31..57] ++ [59..63] ++ [65..115] ++ [117..131] ++ [133..137] ++ [139..140] ++ [142..154] ++ [156..158] ++ [160..167] ++ [171..174] ++ [176..181] ++ [183..189] ++ [192..193] ++ [199..201] ++ [204..213]
  forM_ ok $ \ep -> do
    print ep
    x <- readFile $ "/home/bootstrap/friends/raw/" <> show ep <> ".txt"
    let Just y = scrapeStringLike x $ chroot "body" $ chroots "p" $ lineParser
    writeFile ("/home/bootstrap/friends/" <> show ep <> ".json") $ toS $ encodePretty y
    return ()

