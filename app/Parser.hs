
module Parser
    ( Parser(..)
    , satisfy
    , failure
    , orElse
    , string
    , lowerUpperString
    , many
    , some
    , whitespace
    , parserPure
    ) where

import qualified Control.Monad                 as M
import           Data.Char
import           Data.Maybe

newtype Parser a = Parser { runParser :: String -> Maybe (a, String) }

satisfy :: (Char -> Bool) -> Parser Char
satisfy pred = Parser p
  where
    p ""         = Nothing
    p (c : rest) = if pred c then Just (c, rest) else Nothing

failure :: Parser a
failure = Parser (const Nothing)

orElse :: Parser a -> Parser a -> Parser a
orElse l r = Parser (orElse' (runParser l) (runParser r))
orElse' l r s = res
  where
    lr  = l s
    res = if isJust lr then lr else r s

parserPure :: a -> Parser a
parserPure x = Parser (\s -> Just (x, s))


parserBind :: Parser a -> (a -> Parser b) -> Parser b
parserBind m f = Parser (parserBind' m f)

parserBind' m f s =
    runParser m s >>= (\(match, rest) -> runParser (f match) rest)

instance Functor Parser where
    fmap = M.liftM

instance Applicative Parser where
    pure  = parserPure
    (<*>) = M.ap

instance Monad Parser where
    return = pure
    (>>=)  = parserBind

string :: String -> Parser String
string str@(c : s) = satisfy (== c) >> string s >> pure str
string []          = parserPure []

lowerUpperString :: [Char] -> Parser [Char]
lowerUpperString str@(c : s) =
    satisfy (\a -> toLower a == toLower c) >> lowerUpperString s >> pure str
lowerUpperString [] = parserPure []

many :: Parser a -> Parser [a]
many p = many' p []
many' p soFarAcc = (p >>= (\match -> many' p (soFarAcc ++ [match])))
    `orElse` parserPure soFarAcc

some :: Parser a -> Parser [a]
some p = many p >>= (\match -> if null match then failure else pure match)

-- | includes whitespace characters, multiline and single-line comments
whitespace :: Parser ()
whitespace = many
            (        (satisfy isSpace >> parserPure ())
            `orElse` singleLineComment
            `orElse` multiLineComment
            )
        >> parserPure ()

spaces :: Parser [Char]
spaces = many $ satisfy isSpace

manyNot :: Char -> Parser [Char]
manyNot c = many $ satisfy (/= c)

consumeOrEndMLComment :: Parser () -- either end comment and return or consume a character and try to end again
consumeOrEndMLComment =
    (string "*/" >> parserPure ())
        `orElse` (satisfy (const True) >> consumeOrEndMLComment)

singleLineComment :: Parser () -- //        skip non-newlines, skipNewline & terminate the rule or skip & terminate because EOF
singleLineComment =
    spaces
        >> string "//"
        >> manyNot '\n'
        >> ((satisfy (== '\n') >> parserPure ()) `orElse` parserPure ())

multiLineComment :: Parser ()
multiLineComment =
    spaces >> string "/*" >> consumeOrEndMLComment >> parserPure ()
