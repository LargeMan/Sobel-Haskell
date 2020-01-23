{-
Sobel implementation in Haskell.
Meant for 256 by 256 PGM images;
however, can probably be adjusted for other sizes.

Needs some improvement, particularly in runtime.
-}



module Sobel where

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

-- Input and Output via command line args
main = do
    args <- getArgs
    fp1 <- openFile (args !! 0) ReadMode
    fo1 <- openFile (args !! 1) WriteMode
    --fo2 <- openFile (args !! 2) WriteMode
    --fo3 <- openFile (args !! 3) WriteMode
    --let lo  = (args !! 4)
    --let hi  = (args !! 5)
    fpbl <- BL.hGetContents fp1
    let pic = removeHeader $ BL.unpack fpbl
    let mags = sums 256 (toList pic)
    let out1 = header ++ (norm ((maximum mags)*1.25) 256 mags)
    BL.hPut fo1 (BL.pack out1)
    

removeHeader (_:_:_:_:_:_:_:_:_:_:_:_:_:_:_:xs) = xs

toList :: [Word8] -> [Int]
toList xs = [(fromIntegral(x) :: Int) | x <- xs]

{- Calculate the weighted sums for each mask,
    then convolutes them to find magnitude.
    Automatically sets sums at borders to 0 -}
sums :: Int -> [Int] -> [Float]
sums n xs =
    [z | i <- [0..(n^2 - 1)],
        let (a,b) = if (i `mod` n == 0     ||
                        (i+1) `mod` n == 0)||
                        i   <   n          ||
                        i > ((n * (n-1))-1)  then (0, 0)
                    else
                      ((
                        (xs !! (i-n-1)) * ((maskX !! 0) !! 0) +
                        (xs !! (i-n) )  * ((maskX !! 0) !! 1) +
                        (xs !! (i-n+1)) * ((maskX !! 0) !! 2) +
                        (xs !! (i-1))   * ((maskX !! 1) !! 0) +
                        (xs !!   i  )   * ((maskX !! 1) !! 1) +
                        (xs !! (i+1))   * ((maskX !! 1) !! 2) +
                        (xs !! (i+n-1)) * ((maskX !! 2) !! 0) +
                        (xs !! (i+n))   * ((maskX !! 2) !! 1) +
                        (xs !! (i+n+1)) * ((maskX !! 2) !! 2)
                        ),
                        (
                        (xs !! (i-n-1)) * ((maskY !! 0) !! 0) +
                        (xs !! (i-n) )  * ((maskY !! 0) !! 1) +
                        (xs !! (i-n+1)) * ((maskY !! 0) !! 2) +
                        (xs !! (i-1))   * ((maskY !! 1) !! 0) +
                        (xs !!   i  )   * ((maskY !! 1) !! 1) +
                        (xs !! (i+1))   * ((maskY !! 1) !! 2) +
                        (xs !! (i+n-1)) * ((maskY !! 2) !! 0) +
                        (xs !! (i+n))   * ((maskY !! 2) !! 1) +
                        (xs !! (i+n+1)) * ((maskY !! 2) !! 2)
                        )
                      ),
          let z = sqrt (fromIntegral (a^2 + b^2) :: Float)]

-- Normalizes magnitude values and converts to Word8
norm :: Float -> Float -> [Float] -> [Word8]
norm max n xs = [r | x <- xs,
                     let r = (fromIntegral(
                                floor ((x/max) * (n-1))) :: Word8)]