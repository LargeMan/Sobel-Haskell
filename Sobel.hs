{-
Sobel implementation in Haskell.
Meant for 256 by 256 PGM images;
however, can probably be adjusted for other sizes.
-}

--module Sobel where

import Data.Word
import Data.List
import Data.Binary.Get
import Data.Binary.Put
import qualified Data.ByteString as BL
import Control.Monad
import System.IO
import System.Environment


maskX :: [[Int]]
maskX = [[-1, 0, 1], [-2, 0, 2], [-1, 0, 1]]

maskY :: [[Int]]
maskY = [[ 1, 2, 1], [ 0, 0, 0], [-1,-2,-1]]

header :: [Word8]
header = [80, 53, 10, 50, 53, 54, 32, 50, 53, 54, 10, 50, 51, 53, 10]

{- Input and Output via command line args -}
main = do
    args <- getArgs
    fp1 <- openFile (args !! 0) ReadMode
    fo1 <- openFile (args !! 1) WriteMode
    --fo2 <- openFile (args !! 2) WriteMode
    --fo3 <- openFile (args !! 3) WriteMode
    --let lo  = (args !! 4)
    --let hi  = (args !! 5)
    fpbl <- BL.hGetContents fp1
    let (_, pic) = splitAt 15 (BL.unpack fpbl)
    let (a, b, c) = toList 256 pic
    let top = (replicate 256 0) ++ a
    let bot = c ++ (replicate 256 0)
    let xs = zip9   ((tail top) ++ [0]) (top) (0:top)
                    ((tail b)   ++ [0]) (b)   (0:b)
                    ((tail bot) ++ [0]) (bot) (0:bot)
    let mags = sums 256 xs
    let out1 = header ++ (norm ((maximum mags)*1.15) 256 mags)
    BL.hPut fo1 (BL.pack out1)
    

{- Create three lists (-n, base, +n) -}
toList :: Int -> [Word8] -> ([Float], [Float], [Float])
toList n xs = (a, b, c) where
    b = [(fromIntegral(x) :: Float) | x <- xs]
    (_,c) = splitAt n b       -- +n
    (a,_) = splitAt (n^2-n) b -- -n


{- Zips up 9 lists -}
zip9 :: [Float] -> [Float] -> [Float] -> 
        [Float] -> [Float] -> [Float] -> 
        [Float] -> [Float] -> [Float] ->  [(Float, Float, Float,
                                            Float, Float, Float,
                                            Float, Float, Float)]
zip9 (a:as) (b:bs) (c:cs)
     (d:ds) (e:es) (f:fs)
     (g:gs) (h:hs) (i:is)
        = (a,b,c,d,e,f,g,h,i) : zip9 as bs cs ds es fs gs hs is
zip9 _ _ _ _ _ _ _ _ _ = []


{- Calculate the weighted sums for each mask,
    then convolutes them to find magnitude.
    Automatically sets sums at borders to 0 -}

sums :: Int -> [(Float, Float, Float,
                 Float, Float, Float,
                 Float, Float, Float)] -> [Float]

sums n xs
    = [z | 
           (i, (tl,t,tr,l,c,r,bl,b,br)) <- (zip [0..(n^2 - 1)] xs),
           let (x,y) = if (i `mod` n == 0
                          || (i+1) `mod` n == 0)
                          || i   <   n 
                          || i > ((n * (n-1))-1) then (0, 0)
                       else
                        (
                          (
                            (tl * (-1)) +
                            (tr)        +
                            (l  * (-2)) +
                            (r  * 2)    +
                            (bl * (-1)) +
                            (br)
                          ),
                          (
                            (tl)        +
                            (t  * 2)    +
                            (tr)        +
                            (bl * (-1)) +
                            (b  * (-2)) +
                            (br * (-1))
                          )
                        ),

           let z = sqrt (x**2 + y**2)]


-- Normalizes magnitude values and converts to Word8
norm :: Float -> Float -> [Float] -> [Word8]
norm max n xs = [r | x <- xs,
                     let r = (fromIntegral(
                                floor ((x/max) * (n-1))) :: Word8)]