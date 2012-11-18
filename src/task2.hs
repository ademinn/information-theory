import System.IO
import Text.Printf
import qualified Data.Map as Map

calcFreq :: Int -> String -> Map.Map String Int
calcFreq n s = if length s' >= n
    then Map.insertWith (+) s' 1 $ calcFreq n $ tail s
    else Map.empty
    where s' = take n s

calcEntropy :: Int -> Double -> (Int -> Double) -> Map.Map String Int -> Double
calcEntropy k acc calcProb m = (foldr f acc $ Map.elems m) / fromIntegral k
    where f val acc' = acc' - prob * logBase 2.0 prob where prob = calcProb val

printfLn :: (PrintfType r) => String -> r
printfLn format = printf $ format ++ if nativeNewline == LF then "\n" else "\r\n"

main :: IO ()
main = do
    s <- hGetContents =<< openFile "BIB" ReadMode
    let len = length s
        calcProb k val = (fromIntegral val) / (fromIntegral (len - k) + 1.0)
        calcAns k = (calcEntropy k 0.0 (calcProb k) m, calcEntropy k acc ((*) norm . calcProb k) m)
            where
                m = calcFreq k s
                cnt = Map.size m
                a = 256 ^ k
                prob = 1.0 / fromIntegral (len ^ k)
                norm = 1.0 - (fromIntegral $ a - cnt) * prob
                acc = (fromIntegral $ cnt - a) * prob * logBase 2.0 prob
        printAns k = uncurry (printfLn "%d %10.4f %10.4f" k) (calcAns k)
    printfLn "%s %10s %10s" "n" "0" "1/(N^n)"
    sequence $ map printAns [1, 2, 3, 4]
    return ()