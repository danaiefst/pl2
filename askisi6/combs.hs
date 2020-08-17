import Data.Char (isSpace)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BSC

factorial :: Int -> Int -> Int -> Int
factorial start end m = factorial' start start end m
  where factorial' acc start end m | start == end = acc
                                   | start > end = 1
                                   | otherwise = let res = mod (acc * end) m
                                                 in res `seq` factorial' res start (end - 1) m

find_inv :: Int -> Int -> Int
find_inv a p = let inv = find_inv' 0 1 p a
                in if inv < 0 then (p + inv) else inv
  where find_inv' s old_s r old_r | r == 0 = old_s
                                  | otherwise = let quotient = div old_r r
                                                  in find_inv' (old_s - quotient * s) s (old_r - quotient * r) r

comb_mod :: (Int, Int, Int) -> Int
comb_mod (n, k, p) = let (mn, mx) = (min k (n - k), max k (n - k))
                         ret = mod ((factorial (mx + 1) n p) * (find_inv (factorial 2 mn p) p)) p
                      in if ret < 0 then ret + p else ret

read_input = read_input' []
  where read_input' acc t r | t == 0 = acc
                            | otherwise = let Just (n, r1) = readInt r
                                              Just (k, r2) = readInt r1
                                              Just (p, r3) = readInt r2
                                            in read_input' ((n, k, p) : acc) (t-1) r3

readInt s = BSC.readInt (BSC.dropWhile isSpace s)

main = do
  all <- BS.getContents
  let Just (t, r1) = readInt all
  mapM_ print (map comb_mod (reverse $ read_input t r1))
