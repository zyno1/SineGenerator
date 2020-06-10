-- MIT License
-- 
-- Copyright (c) 2020 Olivier Zeyen
-- 
-- Permission is hereby granted, free of charge, to any person obtaining a copy
-- of this software and associated documentation files (the "Software"), to deal
-- in the Software without restriction, including without limitation the rights
-- to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
-- copies of the Software, and to permit persons to whom the Software is
-- furnished to do so, subject to the following conditions:
-- 
-- The above copyright notice and this permission notice shall be included in all
-- copies or substantial portions of the Software.
-- 
-- THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
-- IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
-- FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
-- AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
-- LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
-- OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
-- SOFTWARE.

module Parser where

import System.IO
import Control.Monad
import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Expr
import Text.ParserCombinators.Parsec.Language
import qualified Text.ParserCombinators.Parsec.Token as Token

data Stmt = BPM Double
    | Volume Double
    | Note String Double
    | Silence Double
    | SampleRate Int
    | Attack Double
    | Release Double
    | Seq [Stmt]
    deriving (Show, Eq)

langageDef = emptyDef
    { Token.commentStart = "/*"
    , Token.commentEnd = "*/"
    , Token.commentLine = "//"
    , Token.identStart = letter
    , Token.identLetter = alphaNum <|> char '#'
    , Token.reservedNames = [ "bpm"
                            , "vol"
                            , "n"
                            , "s"
                            , "sr"
                            , "att"
                            , "rel"
                            ]
    , Token.reservedOpNames = []
    , Token.caseSensitive = False
    }

lexer = Token.makeTokenParser langageDef

identifier = Token.identifier lexer
reserved = Token.reserved lexer
semi = Token.semi lexer
whiteSpace = Token.whiteSpace lexer
integer = Token.decimal lexer
float = Token.float lexer

whileParser :: Parser Stmt
whileParser = whiteSpace >> statement

statement :: Parser Stmt
statement = sequeceOfStmt

sequeceOfStmt = do
    list <- (sepBy1 statement' whiteSpace)
    return $ if length list == 1
        then head list
        else Seq list

statement' :: Parser Stmt
statement' = bpmParser
    <|> volParser
    <|> noteParser
    <|> silenceParser
    <|> sampleRateParser
    <|> attackParser
    <|> releaseParser

bpmParser :: Parser Stmt
bpmParser = do
    reserved "bpm"
    v <- float
    return $ BPM v

volParser :: Parser Stmt
volParser = do
    reserved "vol"
    v <- float
    return $ Volume v

noteParser :: Parser Stmt
noteParser = do
    reserved "n"
    f <- identifier
    t <- float
    return $ Note f t

silenceParser :: Parser Stmt
silenceParser = do
    reserved "s"
    t <- float
    return $ Silence t

sampleRateParser :: Parser Stmt
sampleRateParser = do
    reserved "sr"
    v <- integer
    return $ SampleRate (fromIntegral v)

attackParser :: Parser Stmt
attackParser = do
    reserved "att"
    v <- float
    return $ Attack v

releaseParser :: Parser Stmt
releaseParser = do
    reserved "rel"
    v <- float
    return $ Release v

parseString :: String -> Stmt
parseString str = case parse whileParser "" str of
    Left e -> error $ show e
    Right r -> r

parseFile :: String -> IO Stmt
parseFile file = do
    program <- readFile file
    case parse whileParser "" program of
        Left e -> print e >> fail "parse error"
        Right r -> return r
