module Data.DSON.Parse(parseDson, DSON(..)) where

import Control.Applicative ((<$>), (<*>))
import Control.Monad (liftM)
import Text.Parsec hiding (Empty)
import Text.Parsec.String
import Text.Parsec.Combinator
import Text.Parsec.Language (javaStyle)
import qualified Text.Parsec.Token as P

data DSON = Str String
           | Object [(String, DSON)]
           | Array [DSON]
           | Number Double
           | No
           | Yes
           | Empty
  deriving (Show, Eq)

-- | Parse a DSON string, returning `Nothing` if no valid DSON is found
parseDson   :: String -> Maybe DSON
parseDson s = either (const Nothing) Just result
  where result = parse topLevel "" s

topLevel :: Parser DSON
topLevel = try objectP <|> arrayP

valueP :: Parser DSON
valueP =     try strP
         <|> try numberP
         <|> try objectP
         <|> try arrayP
         <|> try (symbol "yes" >> return Yes)
         <|> try (symbol "no" >> return No)
         <|> (symbol "empty" >> return Empty)

objectP :: Parser DSON
objectP = do symbol "such"
             tups <- optTuplesP
             symbol "wow"
             return $ Object tups
  where optTuplesP = option [] ((:) <$> tupleP <*> tuplesP)
        tuplesP = many (separatorP >> tupleP)
        tupleP = do str <- stringLiteral
                    symbol "is"
                    v <- valueP
                    return (str, v)
        separatorP =     try (symbol "next")
                     <|> try (symbol ",")
                     <|> try (symbol ".")
                     <|> try (symbol "!")
                     <|> symbol "?"

arrayP :: Parser DSON
arrayP = do symbol "so"
            vs <- valuesP
            symbol "many"
            return $ Array vs
  where valuesP = (:) <$> valueP <*> many (separatorP >> valueP)
        separatorP = try (symbol "and") <|> symbol "also"

strP :: Parser DSON
strP = fmap Str stringLiteral

numberP :: Parser DSON
numberP = do factor <- option 1 (char '-' >> return (-1))
             n <- natOrFloat
             ex <- option 1 (veryP >> integer)
             return $ Number ((factor * n) ** fromInteger ex)
  where veryP = try (symbol "very") <|> symbol "VERY"
        natOrFloat = liftM (either fromInteger id) naturalOrFloat

lexer = P.makeTokenParser javaStyle

symbol = P.symbol lexer
stringLiteral = P.stringLiteral lexer
float = P.float lexer
integer = P.integer lexer
naturalOrFloat = P.naturalOrFloat lexer
