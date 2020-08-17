{-# OPTIONS_GHC -O2 #-}

import qualified Data.Vector.Unboxed.Mutable as M
import Data.Char (isSpace)
import Debug.Trace
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BSC

sum' :: M.IOVector Int -> Int -> Int -> IO ()
sum' = s 0 2
  where s acc j v i md | j - 1 > i = do
                          x <- M.unsafeRead v (i-1)
                          let y = (acc + x) `mod` md
                            in y `seq` M.unsafeWrite v i y
                       | j - 1 < i = do
                          x <- M.unsafeRead v (i-j+1)
                          z <- M.unsafeRead v (i-j)
                          let y = (acc + x - z) `mod` md
                            in y `seq` s y (2*j) v i md
                       | j - 1 == i = do
                          x <- M.unsafeRead v (i-j+1)
                          let y = (acc + x) `mod` md
                            in y `seq` s y (2*j) v i md

make_vector :: Int -> Int -> M.IOVector Int -> IO ()
make_vector = make_vector' 2
  where make_vector' i mx md v | i > mx = return ()
                               | i <= mx = do
                                 sum' v i md
                                 make_vector' (i+1) mx md v

iter :: Int -> Int -> Int -> M.IOVector Int -> IO (Int)
iter = iter' 0
  where iter' sm start end md v | start == 0 = iter' 1 1 end md v
                                | start /= 0 = do
                                  x <- M.unsafeRead v (start - 1)
                                  y <- M.unsafeRead v end
                                  return $ (y - x + sm) `mod` md

iter_calc :: [(Int, Int)] -> Int -> M.IOVector Int -> IO ()
iter_calc ((i, end) : []) md v = do
  x <- iter i end md v
  putStrLn (show x)
iter_calc ((i, end) : xs) md v = do
  x <- iter i end md v
  putStrLn (show x)
  iter_calc xs md v

readMany = readMany' [] 0
readMany' acc mx s = case readInt s of
                        Just (a, r) -> let Just (b, r1) = readInt r
                                       in if b > mx then readMany' ((a, b) : acc) b r1 else readMany' ((a, b) : acc) mx r1
                        Nothing -> (reverse acc, mx)

readInt s = BSC.readInt (BSC.dropWhile isSpace  s)

main :: IO ()
main = do
  all <- BS.getContents
  let Just (n, r1) = readInt all
  let Just (m, r2) = readInt r1
  let (points, mx) = readMany r2
  vector <- M.new (mx+1)
  M.unsafeWrite vector 0 2
  M.unsafeWrite vector 1 4
  make_vector mx m vector
  iter_calc points m vector
