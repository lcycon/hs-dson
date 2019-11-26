module Data.DSON.Parse
  ( parseDson
  , DSON(..)
  )
where

import           Control.Applicative            ( (<$>)
                                                , (<*>)
                                                )
import           Data.Char                      ( digitToInt )
import           Data.Functor.Identity
import           Text.Parsec             hiding ( Empty )
import qualified Text.Parsec.Char              as PC
import           Text.Parsec.Language           ( emptyDef )
import           Text.Parsec.String
import qualified Text.Parsec.Token             as P

data DSON = Str String
           | Object [(String, DSON)]
           | Array [DSON]
           | Number Double
           | No
           | Yes
           | Empty
  deriving (Show, Eq)

-- | Parse a DSON string, returning `Nothing` if no valid DSON is found
parseDson :: String -> Maybe DSON
parseDson s = either (const Nothing) Just result
  where result = parse topLevel "" s

topLevel :: Parser DSON
topLevel = try objectP <|> arrayP

valueP :: Parser DSON
valueP =
  try strP
    <|> try numberP
    <|> try objectP
    <|> try arrayP
    <|> try (symbol "yes" >> return Yes)
    <|> try (symbol "no" >> return No)
    <|> (symbol "empty" >> return Empty)

objectP :: Parser DSON
objectP = do
  _    <- symbol "such"
  tups <- optTuplesP
  _    <- symbol "wow"
  return $ Object tups
 where
  optTuplesP = option [] ((:) <$> tupleP <*> tuplesP)
  tuplesP    = many (separatorP >> tupleP)
  tupleP     = do
    str <- stringLiteral
    _   <- symbol "is"
    v   <- valueP
    return (str, v)
  separatorP =
    try (symbol "next")
      <|> try (symbol ",")
      <|> try (symbol ".")
      <|> try (symbol "!")
      <|> symbol "?"

arrayP :: Parser DSON
arrayP = do
  _  <- symbol "so"
  vs <- valuesP
  _  <- symbol "many"
  return $ Array vs
 where
  valuesP    = (:) <$> valueP <*> many (separatorP >> valueP)
  separatorP = try (symbol "and") <|> symbol "also"

strP :: Parser DSON
strP = fmap Str stringLiteral

numberP :: Parser DSON
numberP = do
  sign         <- signP
  number       <- octal
  exponentPart <- option 1 exponentP
  whiteSpace
  return $ Number (sign * number * exponentPart)
 where
  veryP = symbol "very" <|> symbol "VERY"
  signP = (char '-' >> return (-1)) <|> (char '+' >> return 1) <|> return 1
  parseOctal =
    sum
      . zipWith (*) heckinPositivePowersOfEight
      . fmap (fromIntegral . digitToInt)
      . reverse
  simpleOctalP = parseOctal <$> many1 PC.octDigit
  exponentP =
    (\s exponentPart -> 8 ** (s * exponentPart))
      <$> (veryP >> signP)
      <*> simpleOctalP

-- Lexer stuff

lexer :: P.GenTokenParser String u Identity
lexer = P.makeTokenParser emptyDef

symbol :: String -> ParsecT String u Identity String
symbol = P.symbol lexer

stringLiteral :: ParsecT String u Identity String
stringLiteral = P.stringLiteral lexer

whiteSpace :: ParsecT String u Identity ()
whiteSpace = P.whiteSpace lexer

heckinPositivePowersOfEight :: [Double]
heckinPositivePowersOfEight = map (8 **) [0 ..]

heckinNegativePowersOfEight :: [Double]
heckinNegativePowersOfEight = map (recip . (8 **)) [1 ..]

octal :: ParsecT String u Identity Double
octal = do
  bigDigitz <- many1 PC.octDigit
  let bigSum = convertMain bigDigitz
  littleSum <- option
    0.0
    (fmap convertFractional (symbol "." >> many1 PC.octDigit))
  return (bigSum + littleSum)
 where
  convertMain =
    sum
      . zipWith (*) heckinPositivePowersOfEight
      . fmap (fromIntegral . digitToInt)
      . reverse
  convertFractional = sum . zipWith (*) heckinNegativePowersOfEight . fmap
    (fromIntegral . digitToInt)
