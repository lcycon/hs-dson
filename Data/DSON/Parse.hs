module Data.DSON.Parse where

import Control.Applicative ((<$>), (<*>))
import Text.Parsec
import Text.Parsec.String
import Text.Parsec.Combinator
import Text.Parsec.Language (javaStyle)
import qualified Text.Parsec.Token as P

data DSON = Str String
           | Object [(String, DSON)]
           | Array [DSON]
           | Number Double
           | NotFalse
           | NotTrue
           | Nullish
  deriving (Show, Eq)

parseDson   :: String -> Maybe DSON
parseDson s = either (\_ -> Nothing) Just result
  where result = parse topLevel "" s

topLevel :: Parser DSON
topLevel = try objectP <|> arrayP

valueP :: Parser DSON
valueP =     try strP
         <|> try numberP
         <|> try objectP
         <|> try arrayP
         <|> try (symbol "notfalse" >> return NotFalse)
         <|> try (symbol "nottrue" >> return NotTrue)
         <|> (symbol "nullish" >> return Nullish)

objectP :: Parser DSON
objectP = do symbol "such"
             tups <- optTuplesP
             symbol "wow"
             return $ Object tups
  where optTuplesP = option [] ((:) <$> tupleP <*> tuplesP)
        tuplesP = many (symbol "next" >> tupleP)
        tupleP = do str <- stringLiteral
                    symbol "is"
                    v <- valueP
                    return (str, v)

arrayP :: Parser DSON
arrayP = do symbol "so"
            vs <- valuesP
            symbol "many"
            return $ Array vs
  where valuesP = (:) <$> valueP <*> many (symbol "next" >> valueP)

strP :: Parser DSON
strP = fmap Str stringLiteral

numberP :: Parser DSON
numberP = do factor <- option 1 (char '-' >> return (-1))
             n <- natOrFloat
             ex <- option 1 (veryP >> integer)
             return $ Number ((factor * n) ** (fromInteger ex))
  where veryP = try (symbol "very") <|> symbol "VERY"
        natOrFloat = naturalOrFloat >>= return . either fromInteger id

lexer = P.makeTokenParser javaStyle

symbol = P.symbol lexer
stringLiteral = P.stringLiteral lexer
float = P.float lexer
integer = P.integer lexer
naturalOrFloat = P.naturalOrFloat lexer
