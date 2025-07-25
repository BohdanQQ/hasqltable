
-- Credit: Vít Šefl
    -- https://github.com/vituscze
    -- https://github.com/vituscze/neproc/blob/master/Homework/hw6.hs

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
    , wspaced
    , parserPure
    ) where

import qualified Control.Monad                 as M
import           Data.Char
import           Data.Maybe

newtype Parser a = Parser { runParser :: String -> Maybe (a, String) }

satisfy :: (Char -> Bool) -> Parser Char
satisfy predicate = Parser p
  where
    p ""         = Nothing
    p (c : rest) = if predicate c then Just (c, rest) else Nothing

failure :: Parser a
failure = Parser (const Nothing)

orElse :: Parser a -> Parser a -> Parser a
orElse ifP elseP = Parser (orElse' (runParser ifP) (runParser elseP))
  where
    orElse' runPl runPr s = res
      where
        lr  = runPl s
        res = if isJust lr then lr else runPr s

parserPure :: a -> Parser a
parserPure x = Parser (\s -> Just (x, s))


parserBind :: Parser a -> (a -> Parser b) -> Parser b
parserBind p newPFn = Parser (parserBind' p newPFn)
  where
    parserBind' m f s =
        runParser m s >>= (\(match, rest) -> runParser (f match) rest)

instance Functor Parser where
    fmap = M.liftM

instance Applicative Parser where
    pure  = parserPure
    (<*>) = M.ap

instance Monad Parser where
    (>>=)  = parserBind

string :: String -> Parser String
string str@(c : s) = satisfy (== c) >> string s >> pure str
string []          = parserPure []

lowerUpperString :: [Char] -> Parser [Char]
lowerUpperString str@(c : s) =
    satisfy (\a -> toLower a == toLower c) >> lowerUpperString s >> pure str
lowerUpperString [] = parserPure []

many :: Parser a -> Parser [a]
many psr = many' psr []
  where
    many' p soFarAcc = (p >>= (\match -> many' p (soFarAcc ++ [match])))
        `orElse` parserPure soFarAcc

some :: Parser a -> Parser [a]
some p = many p >>= (\match -> if null match then failure else pure match)

-- | includes whitespace characters, multiline and single-line comments
whitespace :: Parser ()
whitespace =
    many
            ((satisfy isSpace >> parserPure ())
            `orElse` singleLineComment
            `orElse` multiLineComment
            )
        >> parserPure ()

wspaced :: Parser a -> Parser a
wspaced x = whitespace *> x <* whitespace

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
