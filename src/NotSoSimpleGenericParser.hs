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
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE MultiWayIf #-}
-- {-# LANGUAGE NoMonomorphismRestriction #-}

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
    succeeds,
    ifP,
    branches,
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
    count,
    atLeast,
    search,
    manyTill,
    negateP,
    matchPairsP,
    getState,
    toTokens,
    putState,
    checkpoint,
    rollback,
    collectUpTo,
    boundedThen,
    bounded,
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
import Data.Foldable (asum)
import Data.Kind (Type)
import qualified Data.List as List
import Data.String (IsString)
import qualified Data.ByteString.Char8 as BSC
import qualified Data.Text as T
import Data.ByteString (ByteString)
import Data.Text (Text)
import Data.Bool (bool)


type ParseError = String

pattern Success :: (a, st) -> Either (ParseError, st) (a, st)
pattern Success result = Right result

pattern Failure :: (ParseError, st) -> Either (ParseError, st) (a, st)
pattern Failure err = Left err


{-# COMPLETE Success, Failure #-}


data ParserState s = ParserState
  { inputS :: s         -- The remaining input stream
  , posS    :: Int       -- The current position in the input
  , isCut    :: Bool
  } deriving (Show, Eq)


-- Create an initial state from an input stream.
mkInitialState :: s -> ParserState s
mkInitialState s = ParserState { inputS = s, posS = 0, isCut = False}

{-
a (Parser s a) is a parser that operates on an input/stream of type `s` and has result type `a`
so a (Parser String Int) would be a parser that parses a string and gives an Int in the result

I've added position tracking to the parser state but originally the parser type looked like this:
newtype Parser s a = Parser {runParser :: s -> Either (ParseError, s) (a, s)}

we keep state on both success and failure so that we can provide rich error messages
e.g.

parseABC :: Parser String String
parseABC = do
    char 'a'
    char 'b'
    char 'c'
    return "abc"

if we give the input "abd" it will fail but it will advance the state upto the point of failure
Left ("Expected 'c', found 'd'",ParserState {inputS = "d", ... }
notice that the inputS is advanced to the point of failure and a and b have been consumed
this allows us to find the exact spot where an error happened on failure
this doesn't affect backtracking since we always backtrack when trying the alternative
the behaviour should be as follows:
    - simple parsers should never consume on failure
    - compound parsers (made from combining multiple other parsers) can consume on failure
-}

newtype Parser s a = Parser
    { runParser ::
        ParserState s ->
        Either
            (ParseError, ParserState s)
            (a,          ParserState s)
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

    fmap f p = do
        result <- p
        return (f result)

    -- fmap f p = Parser $ \st ->
    --     case runParser p st of
    --         Success (v, rest) -> Success (f v, rest)
    --         Failure err -> Failure err

instance Applicative (Parser s) where
    pure :: a -> Parser s a
    pure x = Parser $ \st -> Success (x, st)

    (<*>) :: Parser s (a -> b) -> Parser s a -> Parser s b
    pf <*> px = do
        f <- pf
        x <- px
        return (f x)

    -- (<*>) :: Parser s (a -> b) -> Parser s a -> Parser s b
    -- pf <*> px = Parser $ \st ->
    --     case runParser pf st of
    --         Failure err -> Failure err
    --         Success (f, st') ->
    --             case runParser px st' of
    --                 Failure err -> Failure err
    --                 Success (x, st'') -> Success (f x, st'')

instance Monad (Parser s) where
    (>>=) :: Parser s a -> (a -> Parser s b) -> Parser s b
    p >>= f = Parser $ \st ->
        case runParser p st of
            Success (v, st') -> runParser (f v) st'
            Failure err -> Failure err

    -- p >>= f = p `onSuccess` \(v, st) -> Parser $ \_ -> runParser (f v) st

instance MonadFail (Parser s) where
    fail :: String -> Parser s a
    fail msg = Parser $ \st -> Failure (msg, st)

-- basically >>= but flips Success/Failure, passes successes through and focuses on errors
onFailure :: Parser s a -> ((ParseError, ParserState s) -> Parser s a) -> Parser s a
onFailure p f = do
    st <- getState
    case runParser p st of
        Success res -> succeedWith res
        Failure err -> f err

-- basically >>= but affects the whole (result, state) tuple
onSuccess :: Parser s a -> ((a, ParserState s) -> Parser s b) -> Parser s b
onSuccess p f = do
    st <- getState
    case runParser p st of
        Success res -> f res
        Failure err -> failWith err

failWith :: (ParseError, ParserState s) -> Parser s a
failWith err = Parser $ \_ -> Failure err

succeedWith :: (a, ParserState s) -> Parser s a
succeedWith (res, st) = Parser $ \_ -> Success (res, st)

-- modifies the error of a parser on failure using a function
modifyError :: (ParseError -> ParseError) -> Parser s a -> Parser s a
-- modifyError modify p = Parser $ \st ->
--     case runParser p st of
--         Failure (msg, st') -> Failure (modify msg, st')
--         success -> success

modifyError modify p = p `onFailure` \(err, st) -> failWith (modify err, st)
-- modifyError modify p = p `onFailure` \(err, st) -> failWith (modify err) st
-- modifyError modify = (`onFailure` uncurry (failWith . modify))

setError :: ParseError -> Parser s a -> Parser s a
setError = modifyError . const


wErrorMod :: Parser s a -> (ParseError -> ParseError) -> Parser s a
wErrorMod = flip modifyError

-- replaces the error of a parser
wError :: Parser s a -> ParseError -> Parser s a
-- wError p error = p `wErrorMod` const error
wError = flip setError

-- forceFail :: Parser s a -> ParseError -> Parser s b
-- forceFail p msg = p `wError` msg *> fail msg



instance Alternative (Parser s) where
    empty = Parser $ \st ->
        Failure ("Empty parser", st)

    -- (<|>) :: Parser s a -> Parser s a -> Parser s a
    -- p1 <|> p2 = Parser $ \st ->
    --     case runParser p1 st of
    --         Success (res1, st1) -> Success (res1, st1 {isCut = False})
    --         -- if first parser fails try the second one on the original state
    --         Failure (err1, st1) ->
    --             case runParser p2 st of
    --                 Success result -> Success result
    --                 -- if both parsers fail take the error from the parser that consumed more
    --                 Failure (err2, st2) ->
    --                     case compare (posS st1) (posS st2) of
    --                         GT -> Failure (err1, st1)
    --                         EQ -> Failure (err1 ++ " or " ++ err2, st1)
    --                         LT -> Failure (err2, st2)

    (<|>) :: Parser s a -> Parser s a -> Parser s a
    p1 <|> p2 = unCut $ do
        st0 <- getState -- save initial state
        p1 `onFailure` \e@(err1, st1) ->
            if isCut st1
                then failWith e
                else do
                    putState st0 -- backtrack to initial state
                    p2 `onFailure` \(err2, st2) -> Parser $ \_ ->
                        case compare (posS st1) (posS st2) of
                            GT -> Failure (err1, st1)
                            EQ -> Failure (err1 ++ " or " ++ err2, st1)
                            LT -> Failure (err2, st2)

cut :: Parser s ()
cut = Parser $ \st -> Success ((), st {isCut = True})


unCut :: Parser s b -> Parser s b
unCut p = do
    r <- p
    st <- getState
    putState st {isCut = False}
    return r

-- Parse optional value
optional :: Parser s a -> Parser s (Maybe a)
optional p = (Just <$> p) <|> pure Nothing

-- Parse one of a list of parsers (same as `choice = foldr (<|>) empty`)
choice :: [Parser s a] -> Parser s a
choice = asum

succeeds :: Alternative f => f a -> f Bool
succeeds p = (p *> pure True) <|> pure False

-- Conditional branch parsing
-----------------------------
-- tries a parser, if it's successful return parser thenP otherwise return parser elseP
ifP :: Parser s a -> Parser s b -> Parser s b -> Parser s b
ifP condP thenP elseP = do
    result <- succeeds condP
    if result
        then thenP
        else elseP


ifP' :: Parser s a -> Parser s b -> Parser s b -> Parser s b
ifP' condP thenP elseP = succeeds condP >>= bool thenP elseP

-- *TODO* do something like ultra debug mode or something
ifPdebug :: Parser s a -> Parser s b -> Parser s b -> Parser s b
ifPdebug p thenP elseP = do
    result <- succeeds p
    if result
        then thenP `wErrorMod` ("ifP (then):" ++)
        else elseP `wErrorMod` ("ifP (else):" ++)

-- aaaaaab
-- construct for chaining parser ifP: Cond (p1) (p2) is like if (p1) succeeds then parser (p2)
data Branch s a
    = forall c.
        Cond (Parser s c) (Parser s a)
    | Otherwise (Parser s a)

-- basically like haskell's guards except we have predicate cond parsers and action parsers
branches :: [Branch s a] -> Parser s a
branches [] = empty
branches (Cond cond action : rest) = do
    result <- succeeds cond
    if result
        then action
        else branches rest
branches (Otherwise action : _) = action


instance Semigroup a => Semigroup (Parser s a) where
    p1 <> p2 = liftA2 (<>) p1 p2

instance Monoid a => Monoid (Parser s a) where
    mempty = pure mempty
    mappend = (<>)


-- funny stupid function that converts a stream to a list of elements in a very overcomplicated way
toTokens :: Stream s => s -> [Elem s]
toTokens stream = case runParser (many anyToken) (mkInitialState stream) of
    Success (result, _) -> result
    _ -> []

-- Get any token
anyToken :: (Stream s) => Parser s (Elem s)
anyToken = Parser $ \st ->
    case uncons (inputS st) of
        Nothing -> Failure ("End Of Input", st)
        Just (t, rest) -> Success (t, st')
          where st' = st {inputS = rest, posS = posS st + 1 }

-- parses if input is empty
endOfInput :: (Stream s) => Parser s ()
endOfInput = peekNot anyToken `wError` "Expected end of input"

-- Match a token that satisfies a predicate, also takes a string representing what was expected
satisfy :: (Stream s) => (Elem s -> Bool) -> ParseError -> Parser s (Elem s)
satisfy pred expected = try $ do
    t <- anyToken `wErrorMod` \msg -> msg ++ ", Expected " ++ expected
    -- st <- getState
    -- t <- anyToken `onFailure` \(msg,_) -> failWith (msg ++ ", Expected " ++ expected, st)
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
                 newPos = posS st + n
                 newSt  = st { inputS = rest, posS = newPos }
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

-- exactly n repetitions of p
count :: Int -> Parser s a -> Parser s [a]
count 0 _ = pure []
count n p = (:) <$> p <*> count (n-1) p

-- atLeast :: Int -> Parser s a -> Parser s [a]
-- atLeast n p = do
--     first <- count n p
--     rest <- many p
--     return $ first ++ rest


atLeast :: Int -> Parser s a -> Parser s [a]
atLeast n p = (++) <$> count n p <*> many p


manyTill :: Parser s a -> Parser s end -> Parser s [a]
manyTill p end = (lookAhead end *> pure []) <|> ((:) <$> p <*> manyTill p end)

search :: Stream s => Parser s a -> Parser s a
search p = p <|> (anyToken *> search p)


collectUpTo :: Int -> Parser s a -> Parser s [(a, ParserState s)]
collectUpTo n p = go n []
  where
    go 0 acc = pure (reverse acc)
    go k acc =
        ( do
            (x, st) <- checkpoint p
            go (k - 1) ((x, st) : acc)
        )
            <|> pure (reverse acc)

boundedThen :: Int -> Int -> Parser s a -> Parser s b -> Parser s ([a], b)
boundedThen lo hi p suffix = do
    st0 <- getState
    resultsWithStates <- collectUpTo hi p

    let results = map fst resultsWithStates
        cuts = (0, st0) : zip [1 ..] (map snd resultsWithStates)
        valid = drop lo cuts

        -- tryAt [] = fail "suffix never matched"
        -- tryAt ((i,st) : rest) = do
        --     rollback st
        --     b <- suffix
        --     return (take i results, b)
        --     <|> tryAt rest
        tryAt [] = fail "suffix never matched"
        tryAt ((i, st) : rest) =
            rollback st
                *> ( (take i results,) <$> suffix <|> tryAt rest
                   )
    tryAt (reverse valid)


bounded :: Int -> Int -> Parser s a -> Parser s [a]
bounded lo hi p = fst <$> boundedThen lo hi p (pure ())

-- Grab the current state
getState :: Parser s (ParserState s)
getState = Parser $ \st -> Success (st, st)

-- Unconditionally restore a saved state
putState :: ParserState s -> Parser s ()
putState st' = Parser $ \_ -> Success ((), st')


checkpoint :: Parser s a -> Parser s (a, ParserState s)
checkpoint p = (,) <$> p <*> getState

rollback :: ParserState s -> Parser s ()
rollback = putState

-- failW

-- tries a parser but on failure doesn't consume input (mostly used for manipulating errors and stuff)
try :: Parser s a -> Parser s a
try p = Parser $ \st ->
    case runParser p st of
        Failure (err, _) -> Failure (err, st)
        success -> success
-- try p = do
--     st <- getState
--     p `onFailure` \(err,_) -> putState st *> fail err


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
-- peekNot p = lookAhead $ ifP p
--     (fail "peekNot: parser matched")
--     (pure ())


-- negates success and failure retaining consumption behaviour
negateP :: Parser s a -> Parser s ()
negateP p = Parser $ \st ->
    case runParser p st of
        Success (_, st') -> Failure ("negateP: parser matched", st')
        Failure (_, st') -> Success ((), st')

revive :: a -> Parser s a -> Parser s a
revive defaultVal p = Parser $ \st ->
    case runParser p st of
        Failure _ -> Success (defaultVal, st)
        success -> success


-- takes a parser and gives you the result and the amount consumed
wConsumed :: (Stream s) => Parser s a -> Parser s (a, s)
-- wConsumed p = Parser $ \st ->
--     case runParser p st of
--         Success (res, st') -> Success ( (res, consumed), st' )
--             where consumed = takeS (posS st' - posS st) (inputS st)
--         Failure (err, st') -> Failure (err, st')
wConsumed p = do
    st0 <- getState
    result <- p
    st1 <- getState
    let consumed = takeS (posS st1 - posS st0) (inputS st0)
    return (result, consumed)


-- takes a parser gives a parser whose result is what the first consumes
getConsumed :: Stream s => Parser s a -> Parser s s
getConsumed = (snd <$>) . wConsumed

-- run a parser
wCapture :: (Stream s) => Parser s a -> Parser s (a, Parser s a)
wCapture p = do
    (result, consumed) <- wConsumed p
    let replay = tokens consumed *> pure result
    return (result, replay)


-- parses from a starting opening delimiter to its matching closing delimiter
matchPairs :: (Stream s) => Elem s -> Elem s -> Parser s s
matchPairs open close = getConsumed (token open *> inner 1)
  where
    inner 0 = pure ()
    inner n = do
        t <- anyToken `wErrorMod` (\msg -> "matchPairs: " ++ msg ++ ", " ++ show n ++ " unmatched delimiters")
        if | t == open  -> inner (n + 1)
           | t == close -> inner (n - 1)
           | otherwise  -> inner n




{-
you probably shouldn't use this! it has to re-parse the delimiters each time
you should probably lex first and just use matchPairs
this is mostly here as a proof of concept
This is the same as matchPairs but accepts arbitrary parsers for the opening and closing delimiters
care must be taken when using this parser because it
-}
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

matchPairsP2 :: (Stream s) => Parser s a -> Parser s c -> Parser s s
matchPairsP2 openP closeP = getConsumed (openP *> inner 1)
  where
    errf n msg = "matchPairsP: " ++ msg ++ ", " ++ show n ++ " unmatched delimiters"
    inner 0 = return ()
    inner n =
        ( (openP *> cut *> inner (n + 1))
            <|> (closeP *> cut *> inner (n - 1))
        )
            <|> (anyToken `wErrorMod` errf n *> inner n)

-- (DONT USE! just for fun) gets stream contained within a pair of matching open/close patterns
matchPairsFun :: Stream s => Parser s a -> Parser s b -> Parser s s
matchPairsFun openP closeP = getConsumed (openP *> go <* closeP)
  where
    errf msg = "matchPairsFun: " ++ msg ++ ", " ++ "unmatched delimiters"
    go =
        branches
            [ Cond openP (go *> closeP *> go)
            , Cond closeP (pure ())
            , Otherwise (anyToken `wErrorMod` errf *> go)
            ]


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
