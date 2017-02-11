{-# LANGUAGE OverloadedStrings #-}

module Helper where


import NumberPlace
import DB
import Parser
import qualified BoardModel as BM
import qualified Data.ByteString.Lazy.Internal as BLI
import qualified Data.ByteString.Internal as BI

{--
****問題解答関連****
-}
succes_solve        = 0
parse_error         = 1
failed_solve        = 2
too_many_solved     = 3
not_available_board = 4

succesSolveRes :: [[[Int]]] -> Int -> BM.NPResponse
succesSolveRes boards id = BM.NPResponse boards succes_solve id

parseErrorRes :: BM.NPResponse
parseErrorRes = BM.NPResponse [failedBoard] parse_error (-1)

solveErrorRes :: BM.NPResponse
solveErrorRes = BM.NPResponse [failedBoard] failed_solve (-1)

tooManySolvedRes :: [[[Int]]] -> Int -> BM.NPResponse
tooManySolvedRes boards id = BM.NPResponse boards too_many_solved id

notAvailableBoardErrorRes :: BM.NPResponse
notAvailableBoardErrorRes = BM.NPResponse [failedBoard] not_available_board (-1)

handleNP :: BI.ByteString -> BLI.ByteString -> IO(BM.NPResponse)
handleNP biBody bliBody = do
  putStrLn $ "BIBody  is " ++ show biBody
  putStrLn $ "BLIBody is " ++ show bliBody
  let np = getNp biBody bliBody
  case np of
    Nothing -> do
      return parseErrorRes
    Just req@(BM.NP b) -> do
      putStrLn $ "check"
      if availableBoard b
        then do
          putStrLn $ "availableBoard"
          board <- solveBoard b
          return board
        else do
          putStrLn $ "not availableBoard"
          return notAvailableBoardErrorRes

-- JSON形式に変換
getNp :: BI.ByteString -> BLI.ByteString -> Maybe BM.NP
getNp biBody bliBody = if bliBody == BLI.Empty then BM.decodeBiNp biBody else BM.decodeBliNp bliBody

-- ↓↓↓↓ 問題が不正でないかのチェック関連 ↓↓↓↓
availableBoard :: [[Int]] -> Bool
availableBoard board = let bRowList = map isNotOverlapList $ map (filter (0/=)) board
                           bColumnList = map isNotOverlapList $ map (filter (0/=)) $ rotateBoard board
                           bGroupList = map isNotOverlapList $ map (filter (0/=)) $ getGroups board
                       in    isOverlapList bRowList
                          && isOverlapList bColumnList
                          && isOverlapList bGroupList
                          && isNineList board

-- 問題が9*9であるかどうかのチェック
isNineList :: [[Int]] -> Bool
isNineList board = 9 * 9 == length (concat board)

-- 90度回転させる
rotateBoard :: [[Int]] -> [[Int]]
rotateBoard board = [ getColumn board x | x <- [0..boardLength]]
  where
    getColumn board n = map (!! n) board
    boardLength = (length board) - 1

-- 各グループ
getGroups :: [[Int]] -> [[Int]]
getGroups board = [ getGroup board x y | x <- [0, 1, 2], y <- [0, 1, 2]]

getGroup :: [[Int]] -> Int -> Int -> [Int]
getGroup board x y = concatMap (f x') $ f y' board
  where x' = x * 3
        y' = y * 3
        f n xs = take 3 $ drop n xs

-- リストが全て同じものであるかのチェック
isOverlapList :: Eq a => [a] -> Bool
isOverlapList []     = True
isOverlapList [x]    = True
isOverlapList (x:xs) = (x == (head xs)) && isOverlapList xs

-- リストの中身が全て違うものであることのチェック
isNotOverlapList :: Eq a => [a] -> Bool
isNotOverlapList []     = True
isNotOverlapList [x]    = True
isNotOverlapList (x:xs) = (x /= (head xs)) && isNotOverlapList xs

-- ↑↑↑↑ 問題が不正でないかのチェック関連 ↑↑↑↑

-- 問題を解く
solveBoard :: [[Int]] -> IO(BM.NPResponse)
solveBoard board = do
  let solvedBoard = solver' board
  case length solvedBoard of
    0 -> return solveErrorRes
    5 -> return $ tooManySolvedRes solvedBoard 1
    _ -> do
      insertNP board solvedBoard
      return $ succesSolveRes solvedBoard 1

{--
****全取得関連****
-}

success_all_get = 0
failed_all_get  = 1

getAllNp :: IO BM.NumberPlaces
getAllNp = do
  npList <- selectAllNP
  if length npList == 0
    then return $ BM.NumberPlaces npList failed_all_get
    else return $ BM.NumberPlaces npList success_all_get

{--
****id指定取得関連****
-}
success_id_get      = 0
not_exist           = 1
not_available_page  = 2

getSelectNp :: String -> IO BM.NumberPlaces
getSelectNp page = do
  let (from, to) = parsePage page
  if availableSelectNum from to
    then do
      npList <- selectNP from to
      if length npList == 0
        then return $ BM.numberPlacesError not_exist
        else return $ BM.NumberPlaces npList success_id_get
    else return $ BM.numberPlacesError not_available_page

availableSelectNum :: Int -> Int -> Bool
availableSelectNum from to = isFromMoreThanTo && isParseSuccess && isPlus
  where
    isFromMoreThanTo = from <= to
    isParseSuccess = from /= -1 && to /= -1
    isPlus = from >= 0 && to >= 0
