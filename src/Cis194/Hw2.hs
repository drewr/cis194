{-# OPTIONS_GHC -Wall #-}

module Cis194.Hw2 where

import Log ( LogMessage(..)
           , MessageType(..)
           , MessageTree(..)
           , testParse
           , testWhatWentWrong
           )
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

insert :: LogMessage -> MessageTree -> MessageTree
insert (Unknown _) tree = tree
insert msg Leaf = Node Leaf msg Leaf
insert (log @ (LogMessage _ t1 _)) (tree@(Node Leaf (LogMessage _ t2 _) Leaf)) =
  if t1 <= t2
  then Node Leaf log tree
  else Node tree log Leaf
insert (log @ (LogMessage _ t1 _)) (Node left (node@(LogMessage _ t2 _)) right) =
  if t1 <= t2
  then Node (insert log left) node right
  else Node left node (insert log right)

build :: [LogMessage] -> MessageTree
build = foldr insert Leaf

inOrder :: MessageTree -> [LogMessage]
inOrder Leaf = []
inOrder (Node Leaf msg right) = [msg] ++ inOrder right
inOrder (Node left msg Leaf) = inOrder left ++ [msg]
inOrder (Node left msg right) = inOrder left ++ [msg] ++ inOrder right

bad :: LogMessage -> Bool
bad (LogMessage (Error severity) _ _) =
  if severity > 50
  then True
  else False
bad _ = False

getMsg :: LogMessage -> String
getMsg (LogMessage _ _ msg) = msg

whatWentWrong :: [LogMessage] -> [String]
whatWentWrong logs = map getMsg $ filter bad $ inOrder $ build logs
