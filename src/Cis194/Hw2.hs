{-# OPTIONS_GHC -Wall #-}

module Cis194.Hw2 where

import Log (LogMessage(..), MessageType(..), testParse)
import qualified Text.ParserCombinators.Parsec as Parsec

parseMessage :: String -> LogMessage
parseMessage m =
  case Parsec.parse (Parsec.oneOf "IEW") "" m of
    Right 'I' -> LogMessage Info 0 "foo"
    Right 'W' -> LogMessage Warning 0 "foo"
    Right 'E' -> LogMessage (Error 1) 0 "foo"
    Right x -> Unknown ("what is " ++ show x)
    Left err -> Unknown (show err)
