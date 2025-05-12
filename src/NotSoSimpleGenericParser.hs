{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use $>" #-}
{-# LANGUAGE ExistentialQuantification #-}

module NotSoSimpleGenericParser (
    -- Types
    Parser (..),
    ParserState (..),
    ParseError,

    -- Stream typeclasses
    Stream (..),
    CharStream (..),

    -- Running parsers
    parse,

    -- Basic parsers
    anyToken,
    satisfy,
    token,
    notToken,
    tokens,
    tokens',
    tokens'',
    oneOf,
    noneOf,
    endOfInput,

    -- Combinators
    try,
    optional,
    ifP,
    choice,
    between,
    sepBy,
    sepBy1,
    many,
    some,
    modifyError,
    wErrorMod,
    wError,
    lookAhead,
    peekNot,
    wConsumed,
    wCapture,
    revive,
    concatParsers,
    matchPairs,
    -- matchPairsFun,

    -- Character parsers
    char,
    string,
    spaces,
    whitespace,
    digit,
    letter,
    alphaNum,

    -- Pattern synonyms
    pattern Success,
    pattern Failure,

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
import Data.String (IsString)
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as BSC
import Data.Text (Text)
import qualified Data.Text as T

type ParseError = String

pattern Success :: (a, st) -> Either (ParseError, st) (a, st)
pattern Success result = Right result

pattern Failure :: (ParseError, st) -> Either (ParseError, st) (a, st)
pattern Failure err = Left err

{-# COMPLETE Success, Failure #-}

data ParserState s = ParserState
  { inputS :: s         -- The remaining input stream
  , pos   :: Int       -- The current position in the input
  -- , isCut :: Bool
  } deriving (Show, Eq)


-- Create an initial state from an input stream.
mkInitialState :: s -> ParserState s
mkInitialState s = ParserState { inputS = s, pos = 0 }

{-
a (Parser s a) is a parser that operates on an input/stream of type `s` and has result type `a`
so a (Parser String Int) would be a parser that parses a string and gives an Int in the result

I've added position tracking to the parser state but originally the parser type looked like this:
newtype Parser s a = Parser {runParser :: s -> Either (ParseError, s) (a, s)}
-}

newtype Parser s a = Parser
    { runParser ::
        ParserState s ->
        Either
            (ParseError, ParserState s)
            (a, ParserState s)
    }

instance Show (Parser s a) where
    show _ = "<Parser>"

parse :: Parser s a -> s -> Either (ParseError, ParserState s) (a, ParserState s)
parse p s = runParser p (mkInitialState s)

-- parse :: Parser s a -> s -> Either (ParseError, s) (a, s)
-- parse p s =
--     case runParser p (mkInitialState s) of
--       Success (a,   st) -> Success (a, input st)
--       Failure (err, st) -> Failure (err, input st)

-- generic Stream class so you can Implement your own Instances for whatever type
-- e.g. Text/ByteString
class (Eq (Elem s), Show (Elem s), Show s) => Stream s where
    type Elem s :: Type
    -- Get the next item and the rest
    uncons :: s -> Maybe (Elem s, s)
    emptyS :: s
    -- check if is empty maybe
    -- nullS :: s -> Bool

    -- For efficiency
    lengthS :: s -> Int
    takeS :: Int -> s -> s
    dropS :: Int -> s -> s
    splitAtS :: Int -> s -> (s, s)

    -- Test for prefix
    isPrefixOfS :: s -> s -> Bool

-- Constraint for Arbitrary Stream s with element type e
-- (requires ConstraintKinds, TypeOperators)
type StreamOf e s = (Stream s, Elem s ~ e)

-- *TODO* decide whether we're going to use IsString
class (Stream s, Elem s ~ Char, IsString s) => CharStream s where
    fromString :: String -> s
    toString :: s -> String

    -- fromString = fromList
    toString s = case uncons s of
        Nothing -> ""
        Just (c, rest) -> c : toString rest


-- Stream instance for lists of tokens
instance (Eq a, Show a) => Stream [a] where
    type Elem [a] = a
    uncons []     = Nothing
    uncons (x:xs) = Just (x, xs)
    emptyS      = []
    lengthS     = List.length
    takeS       = List.take
    dropS       = List.drop
    splitAtS    = List.splitAt
    isPrefixOfS = List.isPrefixOf

instance CharStream String where
    fromString = id
    toString   = id

-- Stream instance for Text
instance Stream Text where
    type Elem Text = Char
    uncons      = T.uncons
    emptyS      = T.empty
    lengthS     = T.length
    takeS       = T.take
    dropS       = T.drop
    splitAtS    = T.splitAt
    isPrefixOfS = T.isPrefixOf

instance CharStream Text where
    fromString = T.pack
    toString   = T.unpack

-- Stream instance for ByteString
instance Stream ByteString where
    type Elem ByteString = Char
    uncons      = BSC.uncons
    emptyS      = BSC.empty
    lengthS     = BSC.length
    takeS       = BSC.take
    dropS       = BSC.drop
    splitAtS    = BSC.splitAt
    isPrefixOfS = BSC.isPrefixOf

instance CharStream ByteString where
    fromString = BSC.pack
    toString   = BSC.unpack

instance Functor (Parser s) where
    fmap :: (a -> b) -> Parser s a -> Parser s b
    fmap f parser = Parser $ \st ->
        case runParser parser st of
            Success (v, rest) -> Success (f v, rest)
            Failure err -> Failure err

instance Applicative (Parser s) where
    pure :: a -> Parser s a
    pure x = Parser $ \st -> Success (x, st)

    (<*>) :: Parser s (a -> b) -> Parser s a -> Parser s b
    pf <*> px = Parser $ \st ->
        case runParser pf st of
            Failure err -> Failure err
            Success (f, rest) ->
                case runParser px rest of
                    Failure err -> Failure err
                    Success (x, rest') -> Success (f x, rest')

instance Monad (Parser s) where
    (>>=) :: Parser s a -> (a -> Parser s b) -> Parser s b
    parser >>= f = Parser $ \st ->
        case runParser parser st of
            Failure err -> Failure err
            Success (v, rest) -> runParser (f v) rest

instance MonadFail (Parser s) where
    fail :: String -> Parser s a
    fail msg = Parser $ \st -> Failure (msg, st)

instance Alternative (Parser s) where
    empty = Parser $ \st ->
        Failure ("Empty parser", st)

    (<|>) :: Parser s a -> Parser s a -> Parser s a
    p1 <|> p2 = Parser $ \st ->
        case runParser p1 st of
            Success result -> Success result
            -- if first parser fails try the second one on the original state
            Failure (err1, st1) ->
                case runParser p2 st of
                    Success result -> Success result
                    -- if both parsers fail take the error from the parser that consumed more
                    Failure (err2, st2) ->
                        case compare (pos st1) (pos st2) of
                            GT -> Failure (err1, st1)
                            EQ -> Failure (err1 ++ " or " ++ err2, st1)
                            LT -> Failure (err2, st2)

-- Conditional branch parsing
-----------------------------
-- tries a parser, if it's successful return parser thenP otherwise return parser elseP
ifP :: Parser s a -> Parser s b -> Parser s b -> Parser s b
ifP condP thenP elseP = do
    result <- optional condP
    case result of
        Just _  -> thenP
        Nothing -> elseP

-- *TODO* do something like ultra debug mode or something
ifPdebug :: Parser s a -> Parser s b -> Parser s b -> Parser s b
ifPdebug p thenP elseP = do
    result <- optional p
    case result of
        Just _  -> thenP `wErrorMod` ("ifP (then):" ++)
        Nothing -> elseP `wErrorMod` ("ifP (else):" ++)

-- construct for chaining parser ifP: Cond (p1) (p2) is like if (p1) succeeds then parser (p2)
data Branch s a
    = forall c.
        Cond (Parser s c) (Parser s a)
    | Otherwise (Parser s a)

-- basically like haskell's guards except we have predicate cond parsers and action parsers
branches :: [Branch s a] -> Parser s a
branches [] = empty
branches (Cond cond action : rest) = do
    result <- optional cond
    case result of
        Just _  -> action
        Nothing -> branches rest
branches (Otherwise action : _) = action


instance Monoid a => Semigroup (Parser s a) where
    p1 <> p2 = liftA2 mappend p1 p2

instance Monoid a => Monoid (Parser s a) where
    mempty = pure mempty
    mappend = (<>)


-- funny stupid function that converts a stream to a list of elements in a very overcomplicated way
toTokens :: Stream s => s -> [Elem s]
toTokens stream = case runParser (many anyToken) (mkInitialState stream) of
    Success (result, _) -> result
    _ -> []

-- toTokens' :: Stream s => s -> [Elem s]
-- toTokens' s = case uncons s of
--     Just (x, rest) -> x : toTokens' rest
--     _ -> []

-- Get any token
anyToken :: (Stream s) => Parser s (Elem s)
anyToken = Parser $ \st ->
    case uncons (inputS st) of
        Nothing -> Failure ("End Of Input", st)
        Just (t, rest) -> Success (t, st')
          where st' = st {inputS = rest, pos = pos st + 1 }

-- parses if input is empty
endOfInput :: (Stream s) => Parser s ()
endOfInput = peekNot anyToken `wError` "Expected end of input"

-- Match a token that satisfies a predicate, also takes a string representing what was expected
satisfy :: (Stream s) => (Elem s -> Bool) -> String -> Parser s (Elem s)
satisfy pred expected = try $ do
    t <- anyToken `wErrorMod` \msg -> msg ++ ", Expected " ++ expected
    if pred t
        then return t
        else fail $ "Expected " ++ expected ++ ", found " ++ show t

-- Parse a specific token
token :: (Stream s) => Elem s -> Parser s (Elem s)
token t = satisfy (== t) (show t)

-- Parse anything that's not a particular token
notToken :: (Stream s) => Elem s -> Parser s (Elem s)
notToken t = satisfy (/= t) ("not " ++ show t)

-- Parse a sequence of tokens
tokens :: (Stream s) => s -> Parser s s
tokens ts = Parser $ \st ->
  let inp = inputS st
      n   = lengthS ts
  in if ts `isPrefixOfS` inp
        then let rest   = dropS n inp
                 newPos = pos st + n
                 newSt  = st { inputS = rest, pos = newPos }
             in Success (ts, newSt)
        else Failure ("Expected " ++ show ts, st)


tokens' :: (Stream s, Traversable t) => t (Elem s) -> Parser s (t (Elem s))
tokens' = traverse token

tokens'' :: (Stream s, Traversable t, Show (t (Elem s))) => t (Elem s) -> Parser s (t (Elem s))
tokens'' ts = traverse token ts `wErrorMod` \msg -> "in tokens " ++ show ts ++ ": found" ++ msg

-- Parse one of the tokens in the list
-- oneOf :: (Stream s) => [Elem s] -> Parser s (Elem s)
oneOf :: (Stream s, Foldable t, Show (t (Elem s))) => t (Elem s) -> Parser s (Elem s)
oneOf ts = satisfy (`elem` ts) ("one of " ++ show ts)

-- Parse none of the tokens in the list
-- noneOf :: (Stream s) => [Elem s] -> Parser s (Elem s)
noneOf :: (Stream s, Foldable t, Show (t (Elem s))) => t (Elem s) -> Parser s (Elem s)
noneOf ts = satisfy (`notElem` ts) ("none of " ++ show ts)


concatParsers :: (Foldable t, Monoid a) => t (Parser s a) -> Parser s a
concatParsers = foldr (liftA2 mappend) (pure mempty)
-- concatParsers = foldr (\x y -> (<>) <$> x <*> y) (pure mempty)

-- Parse optional value
optional :: Parser s a -> Parser s (Maybe a)
optional p = (Just <$> p) <|> pure Nothing

-- Parse one of a list of parsers (same as `choice = foldr (<|>) empty`)
choice :: [Parser s a] -> Parser s a
choice = asum

-- exactly n repetitions of p
count :: Int -> Parser s a -> Parser s [a]
count 0 _ = pure []
count n p = (:) <$> p <*> count (n-1) p

bounded' :: Int -> Int -> Parser s a -> Parser s [a]
bounded' lo hi p = choice [ count n p | n <- [hi, hi-1 .. lo] ]

-- Grab the current state
getState :: Parser s (ParserState s)
getState = Parser $ \st -> Success (st, st)

-- Unconditionally restore a saved state
putState :: Parser s () -> ParserState s -> Parser s ()
putState _ st' = Parser $ \_ -> Success ((), st')


-- collectUpTo n p = do
--     st0 <- getState

-- boundedThen :: Int -> Int -> Parser s a -> Parser s b -> Parser s ([a], b)
-- boundedThen lo hi p suffix = trySuffix desc where
--     collect 0 st acc rems = (reverse acc, (0, st) : rems)
--     collect n st acc rems =
--         case runParser p st of
--             Success (a, st') ->
--                 collect (n-1) st'
--                         (a:acc)
--                         ((hi - n, st) : rems)
--             Failure _ -> (reverse acc, (hi - n, st) : rems)

--     trySuffix


-- atLeast :: Int -> Parser s a -> Parser s [a]
-- atLeast n p = do
--     first <- count n p
--     rest <- many p
--     return $ first ++ rest

atLeast :: Int -> Parser s a -> Parser s [a]
atLeast n p = (++) <$> count n p <*> many p


search :: Stream s => Parser s a -> Parser s a
search p = p <|> (anyToken *> search p)

manyTill :: Parser s a -> Parser s end -> Parser s [a]
manyTill p end = (end *> pure []) <|> ((:) <$> p <*> manyTill p end)

-- tries a parser but on failure doesn't consume input
try :: Parser s a -> Parser s a
try p = Parser $ \st ->
    case runParser p st of
        Failure (err, _) -> Failure (err, st)
        success -> success

-- tries a parser but doesn't consume input *TODO* maybe rename to peek
lookAhead :: Parser s a -> Parser s a
lookAhead p = Parser $ \st ->
    case runParser p st of
        Success (x, _) -> Success (x, st)
        Failure (e, _) -> Failure (e, st)
        -- failure -> failure

-- Succeeds if parser fails, doesn't consume input (negative lookAhead)
peekNot :: Parser s a -> Parser s ()
peekNot p = Parser $ \st ->
    case runParser p st of
        Success _ -> Failure ("peekNot: parser matched", st)
        Failure _ -> Success ((), st)


-- negates success and failure retaining consumption behaviour
negateP :: Parser s a -> Parser s ()
negateP p = Parser $ \st ->
    case runParser p st of
        Success (_, st') -> Failure ("notP: parser matched", st')
        Failure (_, st') -> Success ((), st')

revive :: a -> Parser s a -> Parser s a
revive defaultVal p = Parser $ \st ->
    case runParser p st of
        Failure (_, st') -> Success (defaultVal, st)
        success -> success




-- modifies the error of a parser on failure using a function
modifyError :: (ParseError -> ParseError) -> Parser s a -> Parser s a
modifyError modify p = Parser $ \input ->
    case runParser p input of
        Failure (msg, remaining) -> Failure (modify msg, remaining)
        success -> success


setError :: ParseError -> Parser s a -> Parser s a
setError = modifyError . const


wErrorMod :: Parser s a -> (ParseError -> ParseError) -> Parser s a
wErrorMod = flip modifyError

-- replaces the error of a parser
wError :: Parser s a -> ParseError -> Parser s a
-- wError p error = p `wErrorMod` const error
wError = flip setError

forceFail :: Parser s a -> ParseError -> Parser s b
forceFail p msg = p `wError` msg *> fail msg

-- takes a parser and gives you the result and the amount consumed
wConsumed :: (Stream s) => Parser s a -> Parser s (a, s)
wConsumed p = Parser $ \st ->
    case runParser p st of
        Success (res, st') -> Success ( (res, consumed), st' )
            where consumed = takeS (pos st' - pos st) (inputS st)
        Failure (err, st') -> Failure (err, st')


-- takes a parser gives a parser whose result is what the first consumes
getConsumed :: Stream s => Parser s a -> Parser s s
getConsumed = (snd <$>) . wConsumed

-- run a parser
wCapture :: (Stream s) => Parser s a -> Parser s (a, Parser s a)
wCapture p = do
    (result, consumed) <- wConsumed p
    let replay = tokens consumed *> pure result
    return (result, replay)


matchPairs :: (Stream s) => Elem s -> Elem s -> Parser s s
matchPairs open close = getConsumed (token open *> go 1)
  where
    go 0 = pure ()
    go n = do
        t <- anyToken `wErrorMod` (\msg -> "matchPairs: " ++ msg ++ ", " ++ show n ++ " unmatched delimiters")
        case t of
            _ | t == open  -> go (n + 1)
              | t == close -> go (n - 1)
              | otherwise  -> go n




matchPairsP :: (Stream s) => Parser s a -> Parser s b -> Parser s s
matchPairsP openP closeP = getConsumed (openP *> inner 1)
  where
    errf n msg = "matchPairsP: " ++ msg ++ ", " ++ show n ++ " unmatched delimiters"
    inner 0 = return ()
    inner n =
        branches
            [ Cond openP (inner (n + 1))
            , Cond closeP (inner (n - 1))
            , Otherwise (anyToken `wErrorMod` errf n *> inner n)
            ]

parseInt :: (CharStream s) => Parser s Int
parseInt = read <$> some digit

-- -- gets stream contained within a pair of matching open/close patterns
-- matchPairsFun :: Stream s => Parser s a -> Parser s b -> Parser s s
-- matchPairsFun openP closeP = getConsumed (openP *> go <* closeP) `wErrorMod` \msg -> msg
--   where
--     errf msg = "matchPairsFun: " ++ msg ++ ", " ++ "unmatched delimiters"
--     go =
--         branches
--             [ Cond openP (go *> closeP *> go)
--             , Cond closeP (pure ())
--             , Otherwise (anyToken `wErrorMod` errf *> go)
--             ]

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
char :: (CharStream s) => Char -> Parser s Char
char = token

-- string :: String -> Parser String String
string :: (CharStream s) => s -> Parser s s
string = tokens

-- spaces :: Parser String String
spaces :: (CharStream s) => Parser s String
spaces = many (char ' ')

-- whitespace :: Parser String String
whitespace :: (CharStream s) => Parser s String
whitespace = many (satisfy isSpace "whitespace")

-- digit :: Parser String Char
digit :: (CharStream s) => Parser s Char
digit = satisfy isDigit "digit"

-- letter :: Parser String Char
letter :: (CharStream s) => Parser s Char
letter = satisfy isAlpha "letter"

-- alphaNum :: Parser String Char
alphaNum :: (CharStream s) => Parser s Char
alphaNum = satisfy isAlphaNum "alphanumeric character"
