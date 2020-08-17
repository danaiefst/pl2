{-# OPTIONS_GHC -O2 #-}

import Data.Char (isSpace)
import Data.Bits (shiftR)
import Data.List (sort)
import Debug.Trace (trace)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BSC

partition1 :: Int -> [((Int, Int), Int)] -> ([((Int, Int), Int)], [((Int, Int), Int)])
partition1 = partition' [] []
  where partition' lt ge _ [] = (lt, ge)
        partition' lt ge n (((a, b), i) : xs) | a < n = partition' (((a, b), i) : lt) ge n xs
                                              | a >= n = partition' lt (((a, b), i) : ge) n xs

partition2 :: Int -> [(Int, Int)] -> ([(Int, Int)], [(Int, Int)])
partition2 = partition' [] []
  where partition' lt ge _ [] = (lt, ge)
        partition' lt ge n ((a, b) : xs) | b < n = partition' ((a, b) : lt) ge n xs
                                         | b >= n = partition' lt ((a, b) : ge) n xs

qsort1 :: [((Int, Int), Int)] -> [((Int, Int), Int)]
qsort1 [] = []
qsort1 (((a, b), i) : xs) = qsort1 lt ++ [((a, b), i)] ++ qsort1 ge
  where (lt,ge) = partition1 a xs

qsort2 :: [(Int, Int)] -> [(Int, Int)]
qsort2 [] = []
qsort2 ((a, b) : xs) = qsort2 lt ++ [(a, b)] ++ qsort2 ge
  where (lt,ge) = partition2 b xs

previous :: Int -> [Int] -> [[Int]]
previous mx = previous' mx (2^mx) (2^mx)
  where previous' i mx pow_i l | i == 0 = []
                               | i /= 0 = let x = drop (mx - pow_i) l
                                            in x `seq` x : previous' (i-1) mx (shiftR pow_i 1) l

sum_heads :: [[Int]] -> Int -> (Int, [[Int]])
sum_heads = sum_heads' 0 []
  where sum_heads' acc_sum acc_tails [] md = (acc_sum, acc_tails)
        sum_heads' acc_sum acc_tails ((x:xs):ys) md = let y = (x + acc_sum) `mod` md
                                                        in  y `seq` sum_heads' y (xs : acc_tails) ys md

sum_more_lists :: [[Int]] -> Int -> [Int]
sum_more_lists [] _ = []
sum_more_lists l md = let (sm, tails) = sum_heads l md
                        in sm : sum_more_lists tails md

make_points :: Int -> Int -> [Int]
make_points mx md = let y = 2^(floor(logBase 2 (fromIntegral mx)))
                        x = (take (y - 3) (repeat 0)) ++ [2, 2] ++ (sum_more_lists (previous (floor(logBase 2 (fromIntegral mx))) x) md)
                      in take (mx + 1) $ drop (y - 2) x

print_combs :: [(Int, Int)] -> IO ()
print_combs [] = putStr ""
print_combs ((sum, _):xs) = do
  print sum
  print_combs xs

readMany = readMany' [] 0 0
readMany' acc mx i s = case readInt s of
                        Just (a, r) -> let Just (b, r1) = readInt r
                                       in if b > mx then readMany' (((a, b), i) : acc) b (i+1) r1 else readMany' (((a, b), i) : acc) mx (i+1) r1
                        Nothing -> (acc, mx)

readInt s = BSC.readInt (BSC.dropWhile isSpace s)

rem_copies :: [Int] -> [Int]
rem_copies acc = let (x:xs) = sort acc in rem_copies' xs x [x]
  where rem_copies' (y:ys) x acc = if x == y then rem_copies' ys y acc else rem_copies' ys y (y : acc)
        rem_copies' [] _ acc = reverse acc

unwrap :: [((Int, Int), Int)] -> [Int]
unwrap = unwrap' []
  where unwrap' acc [] = rem_copies (0 : acc)
        unwrap' acc (((a,b), i) : xs) = unwrap' (a : b : acc) xs

make_helper :: [Int] -> [Int] -> Int -> ([Int], [Int])
make_helper = make_helper' [] [] 0
  where make_helper' acc_h acc_i throws (a : []) all_points _ = (reverse acc_h, reverse (head (drop (a - 1 - throws) all_points) : acc_i))
        make_helper' acc_h acc_i throws (a : b : xs) all_points m = let all_points' = drop (a - throws - 1) all_points
                                                                        z = if a - 1 > 0 then a - 1 else 0
                                                                        sum_p = find_sum a b all_points' m
                                                                      in sum_p `seq` (all_points' `seq` make_helper' (sum_p : acc_h) (head all_points' : acc_i) z (b : xs) all_points' m)

find_sum :: Int -> Int -> [Int] -> Int -> Int
find_sum start end l m | start == 0 = 1 + find_sum' (0:l) 0 end m
                        | start /= 0 = find_sum' l 0 (end - start) m
  where find_sum' (x:xs) sm 0 m = (sm + x) `mod` m
        find_sum' (x:xs) sm i m = let y = (x + sm) `mod` m
                                    in y `seq` find_sum' xs y (i - 1) m

find_points :: [((Int, Int), Int)] -> [Int] -> [Int] -> [Int] -> Int -> [(Int, Int)]
find_points = find_points' []
  where find_points' acc [] _ _ _ _ = acc
        find_points' acc (((start, end), id) : xs) unwrapped helper (ind : individuals) m = let (throws, points) = sum_helper start end unwrapped helper (ind : individuals) m
                                                                                                helper' = drop throws helper
                                                                                                ind' = drop throws (ind : individuals)
                                                                                                unwrapped' = drop throws unwrapped
                                                                                              in helper' `seq` (ind' `seq` (unwrapped' `seq` find_points' ((points, id) : acc) xs unwrapped' helper' ind' m))

sum_helper :: Int -> Int -> [Int] -> [Int] -> [Int] -> Int -> (Int, Int)
sum_helper = sum_helper' 0
  where sum_helper' throws start end (u_x : u_xs) (h_x : h_xs) (i_x : i_xs) m | start /= u_x = if h_xs == [] then (throws, head i_xs) else sum_helper' (throws+1) start end u_xs h_xs i_xs m
                                                                              | start == u_x = if u_xs == [] || end == u_x then (throws, i_x) else (throws, sum_helper'' 0 end (u_xs) (h_x : h_xs) (i_xs) m)
                                                                                  where sum_helper'' sm end (u_x : u_xs) (h_x : h_xs) (i_x : i_xs) m | end == u_x = (sm + h_x) `mod` m
                                                                                                                                                     | end /= u_x = let y = (sm + h_x - i_x) `mod` m
                                                                                                                                                                     in y `seq` sum_helper'' y end u_xs h_xs i_xs m

main :: IO ()
main = do
  all <- BS.getContents
  let Just (n, r1) = readInt all
  let Just (m, r2) = readInt r1
  let (l, mx) = readMany r2
  let all_points = take (mx+1) $ make_points mx m
  let unwrapped = unwrap l
  let (helper, (ind : individuals)) = make_helper unwrapped all_points m
  let final_points_unsorted = find_points (qsort1 l) unwrapped helper (1 : individuals) m
  let fpu = qsort2 final_points_unsorted
  print_combs (fpu)
