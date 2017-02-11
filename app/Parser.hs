module Parser
    (
      parsePage
    ) where

import Text.Parsec as P

expr = do
    x  <- number
    xs <- many $ do           -- 繰り返し
        char '-'
        number
    return $ x:xs             -- 連結

number = do
    x <- many1 digit
    return (read x :: Int)

parsePage :: String -> (Int, Int)
parsePage page = case (parse expr "" page) of
  Left _ -> (-1, -1)
  Right xs -> if 2 /= length xs
                  then (-1, -1)
                  else (xs !! 0, xs !! 1)
                  
