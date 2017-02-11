module  NumberPlace
    (solver, solver', solveMyBoard, Board, failedBoard
    ) where

import Data.List
import Data.Bits

myBoard :: Board
myBoard = [
          [0,0,0,7,0,0,6,0,0],
          [2,0,3,9,0,0,0,8,0],
          [7,0,8,0,0,0,3,0,0],
          [0,0,0,8,3,0,0,0,0],
          [0,0,0,0,0,6,0,0,9],
          [3,0,0,0,0,0,7,0,0],
          [9,7,0,0,0,0,4,0,0],
          [5,0,0,1,0,7,0,0,0],
          [0,4,0,0,6,0,0,0,1]
          ]

failedBoard :: Board
failedBoard = [[0]]

-- [[0,0,0,7,0,0,6,0,0],[2,0,3,9,0,0,0,8,0],[7,0,8,0,0,0,3,0,0],[0,0,0,8,3,0,0,0,0],[0,0,0,0,0,6,0,0,9],[3,0,0,0,0,0,7,0,0],[9,7,0,0,0,0,4,0,0],[5,0,0,1,0,7,0,0,0],[0,4,0,0,6,0,0,0,1]]
{-
[[1,1,1,1,1,1,1,1,1],[2,2,2,2,2,2,2,2,2],[3,3,3,3,3,3,3,3,3],[4,4,4,4,4,4,4,4,4],[5,5,5,5,5,5,5,5,5],[6,6,6,6,6,6,6,6,6],[7,7,7,7,7,7,7,7,7],[8,8,8,8,8,8,8,8,8],[9,9,9,9,9,9,9,9,9]]
[[
[4,1,9,7,8,3,6,5,2],
[2,6,3,9,5,4,1,8,7],
[7,5,8,6,1,2,3,9,4],
[6,9,7,8,3,1,2,4,5],
[1,2,5,4,7,6,8,3,9],
[3,8,4,2,9,5,7,1,6],
[9,7,1,5,2,8,4,6,3],
[5,3,6,1,4,7,9,2,8],
[8,4,2,3,6,9,5,7,1]
]]
-}

-- 盤面
type Board = [[Int]]

-- フラグ
data Flag = Flag [Int] [Int] [Int] deriving (Show)

-- 実際はIntだがBitとして扱う場合にはBitIntを使う
type BitInt = Int

-- n 番目の要素を m に置き換える
substNth :: [a] -> Int -> a -> [a]
substNth []     _ _ = []
substNth (x:xs) 0 m = m : xs
substNth (x:xs) n m = x : substNth xs (n - 1) m

-- x, y から g を求める
groupNum :: Int -> Int -> Int
groupNum x y = (y `div` 3) * 3 + x `div` 3

-- フラグを反転する
invFlag :: Flag -> Int -> Int -> BitInt -> Flag
invFlag (Flag xs ys gs) x y n =
  Flag (invFlag' xs x n) (invFlag' ys y n) (invFlag' gs g n)
  where
    g = groupNum x y
    invFlag' xs x n = substNth xs x $ (xs !! x) `xor` n

-- 数字を書き込む
putNum :: Board -> Int -> Int -> BitInt -> Board
putNum board x y n =
  substNth board y $ substNth (board !! y) x n

-- 数字を取り出す
getNum :: Board -> Int -> Int -> BitInt
getNum board x y = (board !! y) !! x

-- ビットを分離
splitBit :: BitInt -> [Int]
splitBit 0 = []
splitBit n = m : splitBit (n `xor` m)
  where m = (- n) .&. n

-- 可能性のある数字を取り出す
placeNum :: Flag -> Int -> Int -> [BitInt]
placeNum (Flag xs ys gs) x y =
  splitBit $ xf .&. yf .&. gf
  where xf = xs !! x
        yf = ys !! y
        gf = gs !! (groupNum x y)

placeBitNum :: Flag -> Int -> Int -> Int
placeBitNum (Flag xs ys gs) x y =
  xf .&. yf .&. gf
  where xf = xs !! x
        yf = ys !! y
        gf = gs !! (groupNum x y)

-- フラグの初期化
initFlag :: Board -> Flag
initFlag board = foldl (\a (x,y) -> let n = getNum board x y
                                    in if n /= 0 then invFlag a x y n else a)
                 flag
                 [(x,y)| y <- [0..8], x <- [0..8]]
  where flag = Flag (replicate 9 0x3fe)
                    (replicate 9 0x3fe)
                    (replicate 9 0x3fe)

-- 数字をビットに変換
numToBit :: Int -> BitInt
numToBit 0 = 0
numToBit n = shiftL 1 n

-- ビットを数字に変換
bitToNum :: BitInt -> Int
bitToNum 0 = 0
bitToNum n = popCount (n - 1)

-- 盤面をビットボードに変換
toBitBoard :: Board -> Board
toBitBoard board =
  map (\xs -> map (\x -> numToBit x) xs) board

-- ビットボードを元に戻る
fromBitBoard :: Board -> Board
fromBitBoard board =
  map (\xs -> map (\x -> bitToNum x) xs) board

-- 解法
solver :: Board -> [Board]
solver board = iter board' mkidx (initFlag board')
  where board' = toBitBoard board
        mkidx = [(x,y) | y <- [0..8], x <- [0..8], getNum board x y == 0]
        iter board [] _ = return (fromBitBoard board)
        iter board ((x, y):idx) flag = do
          n <- placeNum flag x y
          iter (putNum board x y n) idx (invFlag flag x y n)

--
-- 確定サーチ
--

-- マスに候補が一つしかない
decideCell :: Board -> Flag -> [(Int, Int)] -> (Board, Flag, Int)
decideCell board flag ss=
  foldl (\(b, f, a) (x, y) -> let ns = placeNum f x y
                                  n  = head ns
                              in if length ns == 1
                                 then (putNum b x y n, invFlag f x y n, a + 1)
                                 else (b, f, a))
       (board, flag, 0)
       ss

-- x, y, g のチェック
decideFrame' :: Board -> Flag -> Int -> [(Int,Int)] -> (Board, Flag, Int)
decideFrame' board flag n ss =
  case xs of
    [(x,y)] -> (putNum board x y n, invFlag flag x y n, 1)
    _       -> (board, flag, 0)
  where xs = foldl (\a (x, y) -> let ns = placeBitNum flag x y
                                 in if ns .&. n /= 0
                                    then (x,y):a else a)
                   []
                   ss

decideFrame :: Board -> Flag -> [Int] -> [(Int,Int)] -> (Board, Flag, Int)
decideFrame board flag ns ss =
  foldl (\(b, f, a) n ->
           let (b', f', a') = decideFrame' b f n ss
           in (b', f', a + a'))
        (board, flag, 0)
        ns

-- 縦のチェック
decideX :: Board -> Flag -> (Board, Flag, Int)
decideX board flag@(Flag xs _ _) =
  foldl (\(b, f, a) x ->
            let ss = [(x, y) | y <- [0..8], getNum b x y == 0]
                ns = splitBit $ xs !! x
                (b', f', a') = decideFrame b f ns ss
            in (b', f', a + a'))
        (board, flag, 0)
        [0..8]

-- 横のチェック
decideY :: Board -> Flag -> (Board, Flag, Int)
decideY board flag@(Flag _ ys _) =
  foldl (\(b, f, a) y ->
            let ss = [(x, y) | x <- [0..8], getNum b x y == 0]
                ns = splitBit $ ys !! y
                (b', f', a') = decideFrame b f ns ss
            in (b', f', a + a'))
        (board, flag, 0)
        [0..8]

-- 枠のチェック
decideG :: Board -> Flag -> (Board, Flag, Int)
decideG board flag@(Flag _ _ gs) =
  foldl (\(b, f, a) g ->
            let x0 = (g `mod` 3) * 3
                y0 = (g `div` 3) * 3
                ss = [(x, y) | y <- [y0,y0+1,y0+2], x <- [x0,x0+1,x0+2], getNum b x y == 0]
                ns = splitBit $ gs !! g
                (b', f', a') = decideFrame b f ns ss
            in (b', f', a + a'))
        (board, flag, 0)
        [0..8]

decideNum :: Board -> Flag -> (Board, Flag, Bool)
decideNum board flag =
  if null ss
  then (board, flag, True)
  else if a1 + a2 + a3 + a4 > 0
  then decideNum b4 f4
  else (b4, f4, False)
  where ss = [(x, y) | y <- [0..8], x <- [0..8], getNum board x y == 0]
        (b1, f1, a1) = decideCell board flag ss
        (b2, f2, a2) = decideX b1 f1
        (b3, f3, a3) = decideY b2 f2
        (b4, f4, a4) = decideG b3 f3

-- 確定サーチ + バックトラック
solver' :: Board -> [Board]
solver' board =
  if r
  then [fromBitBoard b]
  else take 5 $ iter b mkidx f
  where board' = toBitBoard board
        (b, f, r) = decideNum board' (initFlag board')
        mkidx = [(x,y) | y <- [0..8], x <- [0..8], getNum b x y == 0]
        iter board [] _ = return (fromBitBoard board)
        iter board ((x, y):idx) flag = do
          n <- placeNum flag x y
          iter (putNum board x y n) idx (invFlag flag x y n)

solveMyBoard :: [Board]
solveMyBoard = solver myBoard
