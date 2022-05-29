module Token where

import TokenType (TokenType)

data Token = Token
    { getType :: TokenType
    , getLexeme :: String
    , getLine :: Int
    }