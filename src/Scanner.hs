module Scanner where

import Control.Applicative
import Data.Foldable
import Token
import TokenType

type Line = Int

newtype Scanner a = Scanner
    { runScanner :: (Line, String) -> Maybe (a, Line, String)
    }

instance Functor Scanner where
    fmap f (Scanner scan) = Scanner $ \inputState -> do
        (a, nextLine, xs) <- scan inputState
        Just (f a, nextLine, xs)

instance Applicative Scanner where
    pure x = Scanner (\(line, xs) -> Just (x, line, xs))
    (Scanner f1) <*> (Scanner f2) = Scanner $ \input -> do
        (f, line, xs) <- f1 input
        (a, line', xs') <- f2 (line, xs)
        Just (f a, line', xs')

instance Alternative Scanner where
    empty = Scanner $ const Nothing
    (Scanner f1) <|> (Scanner f2) = Scanner $ \input -> f1 input <|> f2 input

instance Monad Scanner where
    return x = Scanner (\(line, xs) -> Just (x, line, xs))
    (Scanner f1) >>= f = Scanner $ \input -> do
        (a, line, xs) <- f1 input
        let Scanner f2 = f a
        f2 (line, xs)

scanChar :: Char -> Scanner Char
scanChar c = Scanner f
  where
    f (line, x : xs)
        | x == c = Just (x, if x == '\n' then line + 1 else line, xs)
        | otherwise = Nothing
    f (line, []) = Nothing

scanWord :: String -> Scanner String
scanWord = traverse scanChar

charToTokens :: [(Char, TokenType)]
charToTokens =
    [ ('(', LEFT_PAREN)
    , (')', RIGHT_PAREN)
    , ('{', LEFT_BRACE)
    , ('}', RIGHT_BRACE)
    , (',', COMMA)
    , ('.', DOT)
    , ('-', MINUS)
    , ('+', PLUS)
    , (';', SEMICOLON)
    , ('*', STAR)
    , ('!', BANG)
    , ('=', EQUAL)
    , ('<', LESS)
    , ('>', GREATER)
    ]

wordToTokens :: [(String, TokenType)]
wordToTokens =
    [ ("!=", BANG_EQUAL)
    , ("==", EQUAL_EQUAL)
    , ("<=", LESS_EQUAL)
    , (">=", GREATER_EQUAL)
    ]

scanTokens :: Scanner TokenType
scanTokens = foldl (<|>) empty (wordTokenScanners ++ charTokenScanners)
  where
    f g (x, tokenType) = tokenType <$ g x
    wordTokenScanners = map (f scanWord) wordToTokens
    charTokenScanners = map (f scanChar) charToTokens

scannerStartState :: String -> (String, Line)
scannerStartState x = (x, 0)