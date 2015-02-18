{-# OPTIONS_GHC -Wall #-}

module Cis194.Hw2 where

import Log (LogMessage(..), MessageType(..), testParse)
import Text.ParserCombinators.Parsec
import Control.Applicative hiding ((<|>))

digits :: Parser Int
digits = read' <$> many1 digit
  where read' = read :: String -> Int

warn :: Parser LogMessage
warn = do
  _ <- char 'W'
  _ <- spaces
  t <- digits
  _ <- spaces
  msg <- many1 anyChar
  return $ LogMessage Warning t msg

info :: Parser LogMessage
info = do
  _ <- char 'I'
  _ <- spaces
  t <- digits
  _ <- spaces
  msg <- many1 anyChar
  return $ LogMessage Info t msg

err :: Parser LogMessage
err = do
  _ <- char 'E'
  _ <- spaces
  code <- digits
  _ <- spaces
  t <- digits
  _ <- spaces
  msg <- many1 anyChar
  return $ LogMessage (Error code) t msg

parseMessage :: String -> LogMessage
parseMessage m =
  case Text.ParserCombinators.Parsec.parse
       (    err
        <|> info
        <|> warn) "" m of
    Right l -> l
    Left _ -> Unknown m

parse :: String -> [LogMessage]
parse = map parseMessage . lines
