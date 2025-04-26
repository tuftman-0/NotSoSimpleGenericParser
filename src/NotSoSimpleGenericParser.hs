{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE InstanceSigs #-}


module NotSoSimpleGenericParser (
    -- Types
    Parser (..),
    -- ParserResult (..),
    -- Stream typeclass
    Stream (..),
    -- Running parsers
    parse,
    -- parseFile,
    -- Basic parsers
    anyToken,
    satisfy,
    token,
    notToken,
    tokens,
    oneOf,
    noneOf,
    -- Combinators
    try,
    optional,
    choice,
    between,
    sepBy,
    sepBy1,
    many,
    some,
    modifyError,
    lookAhead,
    -- Character parsers for String
    char,
    string,
    spaces,
    whitespace,
    digit,
    letter,
    alphaNum,
    -- Type aliases
    StreamOf,
) where

import Control.Applicative (Alternative (..))
import Data.Monoid (Monoid, mappend, mempty)
import Data.Char (isAlpha, isAlphaNum, isDigit, isSpace)
import Data.Foldable (asum, Foldable (toList))
import Data.Kind (Type)
import qualified Data.List as List
import Control.Monad (ap)

type ParseError = String

pattern Success :: (a, st) -> Either (ParseError, st) (a, st)
pattern Success result = Right result

pattern Failure :: (String, st) -> Either (ParseError, st) (a, st)
pattern Failure err = Left err


data ParserState s = ParserState
  { input :: s         -- The remaining input stream
  , pos   :: Int       -- The current position in the input
  } deriving (Show, Eq)


-- Create an initial state from an input stream.
mkInitialState :: s -> ParserState s
mkInitialState s = ParserState { input = s, pos = 0 }

-- a (Parser s a) is a parser that operates on an input/stream of type `s` and has a result type of `a`
-- so a (Parser String Int) would be a parser that parses a string and gives an Int in the result
-- newtype Parser s a = Parser {runParser :: s -> Either (ParseError, s) (a, s)}
newtype Parser s a = Parser {runParser :: ParserState s -> Either (ParseError, ParserState s) (a, ParserState s)}


parse :: Parser s a -> s -> Either (ParseError, ParserState s) (a, ParserState s)
parse p s = runParser p (mkInitialState s)
-- parse :: Parser s a -> s -> Either (ParseError, s) (a, s)
-- parse p s =
--     case runParser p (mkInitialState s) of
--       Success (a,   st) -> Success (a, input st)
--       Failure (err, st) -> Failure (err, input st)

-- generic Stream class so you can Implement your own Instances for whatever type e.g. Text/ByteString
class (Eq (Elem s), Show (Elem s)) => Stream s where
    type Elem s :: Type
    -- Get the next item and the rest
    uncons :: s -> Maybe (Elem s, s)

    -- For efficiency
    lengthS :: s -> Int
    takeS :: Int -> s -> s
    dropS :: Int -> s -> s
    splitAtS :: Int -> s -> (s, s)

    -- Test for prefix
    isPrefixOfS :: [Elem s] -> s -> Bool

    -- Convert to string for error messages
    showInput :: s -> String

-- Constraint for Arbitrary Stream s with element type e (requires ConstraintKinds, TypeOperators)
type StreamOf e s = (Stream s, Elem s ~ e)

-- Stream instance for lists of tokens
instance (Eq a, Show a) => Stream [a] where
    type Elem [a] = a
    uncons [] = Nothing
    uncons (x : xs) = Just (x, xs)

    lengthS = length
    takeS = take
    dropS = drop
    splitAtS = splitAt

    isPrefixOfS = List.isPrefixOf
    showInput = show

instance Functor (Parser s) where
    fmap f parser = Parser $ \input ->
        case runParser parser input of
            Success (v, rest) -> Success (f v, rest)
            Failure err -> Failure err

instance Applicative (Parser s) where
    pure :: a -> Parser s a
    pure x = Parser $ \input -> Success (x, input)

    (<*>) :: Parser s (a -> b) -> Parser s a -> Parser s b
    pf <*> px = Parser $ \input ->
        case runParser pf input of
            Failure err -> Failure err
            Success (f, rest) ->
                case runParser px rest of
                    Failure err -> Failure err
                    Success (x, rest') -> Success (f x, rest')

instance Monad (Parser s) where
    (>>=) :: Parser s a -> (a -> Parser s b) -> Parser s b
    parser >>= f = Parser $ \input ->
        case runParser parser input of
            Failure err -> Failure err
            Success (v, rest) -> runParser (f v) rest

instance MonadFail (Parser s) where
    fail msg = Parser $ \input -> Failure (msg, input)

-- slightly annoying Stream conditions because we use lengthS to improve errors
instance Alternative (Parser s) where
    empty = Parser $ \input ->
        Failure ("Empty parser", input)

    (<|>) :: Parser s a -> Parser s a -> Parser s a
    p1 <|> p2 = Parser $ \st ->
        case runParser p1 st of
            Success result -> Success result
            Failure (err1, st1) ->
                case runParser p2 st of
                    Success result -> Success result
                    -- if both parsers fail take the error from the parser that consumed more
                    Failure (err2, st2) ->
                        case compare (pos st1) (pos st2) of
                            GT -> Failure (err1, st1)
                            EQ -> Failure (err1 ++ " or " ++ err2, st1)
                            LT -> Failure (err2, st2)


instance Monoid a => Semigroup (Parser s a) where
    p1 <> p2 = liftA2 mappend p1 p2

instance Monoid a => Monoid (Parser s a) where
    mempty = pure mempty
    mappend = (<>)

newtype Committed s a = Committed {unCommitted :: Parser s a}
newtype Cut s a = Cut {unCut :: Parser s a}

try' :: Either (Committed s a) (Parser s a) -> Parser s a
try' (Right p) = try p  -- The usual backtracking try.
try' (Left (Committed p)) = p  -- Strip the commit wrapper and don’t reset on failure.


-- Get any token
anyToken :: (Stream s) => Parser s (Elem s)
anyToken = Parser $ \st ->
    case uncons (input st) of
        Nothing -> Failure ("End Of Input", st)
        Just (t, rest) -> Success (t, st')
          where st' = st {input = rest, pos = pos st + 1 }

-- Match a token that satisfies a predicate
satisfy :: (Stream s) => (Elem s -> Bool) -> String -> Parser s (Elem s)
satisfy pred expected = try $ do
    t <- anyToken `modifyError` \msg -> msg ++ ", Expected " ++ expected
    if pred t
        then return t
        else fail $ "Expected " ++ expected ++ ", found " ++ show t

-- Parse a specific token
token :: (Stream s) => Elem s -> Parser s (Elem s)
token t = satisfy (== t) (show t)

-- Parse anything that's not a particular token
notToken :: (Stream s) => Elem s -> Parser s (Elem s)
notToken t = satisfy (/= t) ("not " ++ show t)



-- -- Parse a sequence of tokens
-- tokens :: (Stream s) => [Elem s] -> Parser s [Elem s]
-- tokens ts = Parser $ \st ->
--     let inp = input st
--         st' =
--     in if ts `isPrefixOfS` inp
--         then Success (ts, dropS (length ts) inp)
--         else Failure ("Expected " ++ show ts, inp)

-- Parse a sequence of tokens
tokens :: (Stream s) => [Elem s] -> Parser s [Elem s]
tokens ts = Parser $ \st ->
  let inp = input st
      n   = lengthS ts
  in if ts `isPrefixOfS` inp
        then let rest   = dropS n inp
                 newPos = pos st + n
                 newSt  = st { input = rest, pos = newPos }
             in Success (ts, newSt)
        else Failure ("Expected " ++ show ts, st)


tokens' :: (Stream s, Traversable t) => t (Elem s) -> Parser s (t (Elem s))
tokens' = traverse token

-- showElems :: t a -> [Char]
-- showElems :: (Foldable t, Show a) => t a -> [Char]
-- showElems =
-- showElems ts = "\'" ++ concatMap show ts ++ "'"

tokens'' :: (Stream s, Traversable t, Show (t (Elem s))) => t (Elem s) -> Parser s (t (Elem s))
-- tokens'' :: (Stream s, Traversable t) => t (Elem s) -> Parser s (t (Elem s))
tokens'' ts = traverse token ts `modifyError` \msg -> "in tokens (" ++ show ts ++ "): " ++ msg

halve = splitAt =<< (`div` 2) . length
-- tokens''' = (modifyError . traverse token) `ap` (const . ("expected " ++) . show)

tokens''' :: (Stream s, Traversable t, Show (t (Elem s))) => t (Elem s) -> Parser s (t (Elem s))
tokens''' = ap (modifyError . traverse token) ((("in tokens: " ++) .) . (++) . show)
-- tokens''' = (modifyError . traverse token) `ap` ((("expected " ++) .) . (++) . show)

-- foo :: a -> b -> [Char]
-- foo = const . ("expected " ++) . show

-- bar = ((("expected " ++) .) . (++) . show)

-- Parse one of the tokens in the list
-- oneOf :: (Stream s) => [Elem s] -> Parser s (Elem s)
oneOf :: (Stream s, Foldable t, Show (t (Elem s))) => t (Elem s) -> Parser s (Elem s)
oneOf ts = satisfy (`elem` ts) ("one of " ++ show ts)

-- Parse none of the tokens in the list
-- noneOf :: (Stream s) => [Elem s] -> Parser s (Elem s)
noneOf :: (Stream s, Foldable t, Show (t (Elem s))) => t (Elem s) -> Parser s (Elem s)
noneOf ts = satisfy (`notElem` ts) ("none of " ++ show ts)

-- tries a parser but on failure doesn't consume input
try :: Parser s a -> Parser s a
try p = Parser $ \st ->
    case runParser p st of
        Failure (msg, _) -> Failure (msg, st)
        success -> success

-- tries a parser but on success doesn't consume input
lookAhead :: Parser s a -> Parser s a
lookAhead p = Parser $ \st ->
    case runParser p st of
        Success (x, _) -> Success (x, st)
        -- Failure (e, _) -> Failure (e, st)
        failure -> falure


-- modifies the error of a parser on failure using a function
modifyError :: Parser s a -> (ParseError -> ParseError) -> Parser s a
modifyError parser modify = Parser $ \input ->
    case runParser parser input of
        Failure (msg, remaining) -> Failure (modify msg, remaining)
        success -> success

wConsumed :: (Stream s) => Parser s a -> Parser s (a, s)
wConsumed p = Parser $ \st ->
    case runParser p st of
        Success (x, st') -> Success ( (x, takeS (pos st' - pos st) (input st)), st' )
        Failure (e, st') -> Failure (e, st')

-- getTokens :: (Stream s) => s -> Either (ParseError, ParserState s) ([Elem s], ParserState s)
-- getTokens s 



-- wCapture :: (Stream s) => Parser s a -> Parser s (a, Parser s a)
wCapture p = do
    (result, consumed) <- wConsumed p
    return (result, tokens' (toList consumed) *> pure result)

concatParsers :: (Foldable t, Monoid a) => t (Parser s a) -> Parser s a
concatParsers = foldr (liftA2 mappend) (pure mempty)
-- concatParsers = foldr (\x y -> (<>) <$> x <*> y) (pure mempty)

-- Parse optional value
optional :: Parser s a -> Parser s (Maybe a)
optional p = (Just <$> p) <|> pure Nothing

-- Parse one of a list of parsers
choice :: [Parser s a] -> Parser s a
choice = asum
-- choice = foldr (<|>) empty

-- | Parse between matching pairs of tokens, handling nested pairs correctly
-- matchingPair :: Parser s [Elem s] -> Parser s [Elem s] -> Parser s [Elem s]
-- matchingPair openP closeP = do
--     _ <- openP
--     content <- collectWithin 0 []
--     return (reverse content)
--   where
--     collectWithin :: Int -> [Elem s] -> Parser s [Elem s]
--     collectWithin nestLevel acc = do
--         -- Try to match a closing token
--         (do
--             close <- closeP
--             if nestLevel == 0
--                 then return acc  -- Found the matching close, we're done
--                 else collectWithin (nestLevel - 1) (close ++ acc)  -- Continue with decreased nesting
--           ) <|>
--         -- Try to match an opening token (increases nesting)
--           (do
--             open <- openP
--             collectWithin (nestLevel + 1) (open ++ acc)
--           ) <|>
--         -- Any other token is part of the content
--           (do
--             tok <- anyToken
--             collectWithin nestLevel (tok : acc)
--           )
--         <|> fail "Unclosed matching pair"

-- Parse something between delimiters
between :: Parser s open -> Parser s close -> Parser s a -> Parser s a
between open close p = open *> p <* close

-- Parse zero or more occurrences separated by delimiter
sepBy :: Parser s a -> Parser s sep -> Parser s [a]
sepBy p sep = sepBy1 p sep <|> pure []

-- Parse one or more occurrences separated by delimiter
sepBy1 :: Parser s a -> Parser s sep -> Parser s [a]
sepBy1 p sep = do
    x <- p
    xs <- many (sep *> p)
    return (x : xs)

-- Character-specific parsers (for Char streams)
-- char :: Char -> Parser String Char
char :: (StreamOf Char s) => Char -> Parser s Char
char = token

-- string :: String -> Parser String String
string :: (StreamOf Char s) => String -> Parser s String
string = tokens

-- spaces :: Parser String String
spaces :: (StreamOf Char s) => Parser s String
spaces = many (char ' ')

-- whitespace :: Parser String String
whitespace :: (StreamOf Char s) => Parser s String
whitespace = many (satisfy isSpace "whitespace")

-- digit :: Parser String Char
digit :: (StreamOf Char s) => Parser s Char
digit = satisfy isDigit "digit"

-- letter :: Parser String Char
letter :: (StreamOf Char s) => Parser s Char
letter = satisfy isAlpha "letter"

-- alphaNum :: Parser String Char
alphaNum :: (StreamOf Char s) => Parser s Char
alphaNum = satisfy isAlphaNum "alphanumeric character"
