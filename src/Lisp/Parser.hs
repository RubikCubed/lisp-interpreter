{-# LANGUAGE OverloadedStrings #-}

module Lisp.Parser (
  Expression(..),
) where

import qualified Text.Megaparsec.Char.Lexer as L
import Text.Megaparsec
import Data.Void ( Void )
import Data.Text (Text)
import Text.Megaparsec.Char ( space1 )
import Data.Char (isAlphaNum)

type Parser = Parsec Void Text

data Expression
  = Atom Text
  | List [Expression] deriving (Show, Eq)

expression :: Parser Expression
expression = atom <|> list

atom :: Parser Expression
atom = Atom <$> lexeme (takeWhile1P Nothing isAlphaNum)

list :: Parser Expression
list = List <$> parens (many expression)

program :: Parser Expression
program = hidden sc *> expression <* eof


-- helper
parens :: Parser a -> Parser a
parens = between (symbol "(") (symbol ")")

sc :: Parser ()
sc = L.space space1 lineComment empty

lineComment :: Parser ()
lineComment = L.skipLineComment ";"

lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

symbol :: Text -> Parser Text
symbol = L.symbol sc
