{-# LANGUAGE RecordWildCards #-}

import Control.Monad
import Data.Foldable
import Data.List
import Debug.Trace
import Text.Printf

paragraphLengths=[9,4,6,2,2,2,2,4,1, --remove 6 for unsolvable
                  3,2,5,2,6,4,2,1,5,1,3,
                  6,3,2,3,1,3,7,5,
                  2,5,4,2,1,3,2,2,1,3,1,6,
                  1,3,2,6,4,4,2,3,5,2,
                  7,4,5,3,6,1,4,3,
                  4,1,2,2,5,2,5,2,3,1,6, --change last 6 to 3 for backtrack
                  6,4,1,1,1,3,5,3,3,6,
                  9,2,2,3,1,1,2,2,1,2,1,1,
                  2,2,2,2,2,3,3,1,1,5,6,8,
                  7,9,9,7,3,3,9,5,5,8,
                  4,3,7,3,4,2,4,3,4,3,8,5,4]
targetHeight = 32

data Comp = Comp Nature Int deriving Show
data Nature = Entire | Begin | Mid | End deriving (Show, Eq)

main = do
    putStrLn "~.~.~  A L B A T R O S S  ~.~.~"
    putStrLn ""
    let shapes = pageShapesNoWidows paragraphLengths [] 0
    case shapes of
        Nothing -> putStrLn "No solution found"
        Just x -> do
            putStrLn "Found solution:"
            printSolution x


printSolution :: [[Comp]] -> IO ()
printSolution shapes = do
    putStrLn "page  lines  paragraphs"
    putStrLn "----  -----  ----------"
    for_ (zip [1..] shapes) $ \(p,x) -> do
        putStr $ printf "%4d  %5d " (p :: Int) $ sum [l | Comp _ l <- x]
        for_ x $ \(Comp nat len) -> do
            putStr $ if nat == End || nat == Mid then "-" else " "
            putStr $ show len
            when (nat == Begin || nat == Mid) $ putStr "-"
        putStrLn ""


-- Page numbers are 1-based, for consistency with the real world.
findFirstWidow :: [[Comp]] -> Maybe Int
findFirstWidow = fmap succ . findIndex (any (\(Comp n i) -> n == End && i == 1))


spread p = [v,v+1] where v = 2 * (p `div` 2)


pageShapesNoWidows :: [Int] -> [Int] -> Int -> Maybe [[Comp]]
pageShapesNoWidows paras shortPages paradoxLevel =
    traceShow shortPages $
    let shapes = pageShapes paras shortPages
    in case findFirstWidow shapes of
        Nothing -> Just shapes
        Just p -> if p-1 `elem` shortPages
                  then pageShapesNoWidows paras (applyParadox (paradoxLevel+1)) (paradoxLevel + 1)
                  else trace ("Adding page " ++ (show (p-1)) ++ " since it's not in " ++ (show shortPages)) $ pageShapesNoWidows paras (shortPages ++ spread (p-1)) paradoxLevel
        
applyParadox :: Int -> [Int]
applyParadox paradox =
    filter (/= 0) $ concatMap (\x -> if (paradox `div` (2^x)) `mod` 2 == 1 then [2*x,2*x+1] else [0])
                    [0..floor $ logBase 2 $ fromIntegral paradox]

data State = State {
    paragraphRemaining :: Int,
    paras :: [Int],
    pageNum :: Int,
    currentShape :: [Comp],
    pageRemaining :: Int,
    isPartial :: Bool,
    shapes :: [[Comp]]
} deriving (Show)


pageShapes :: [Int] -> [Int] -> [[Comp]]
pageShapes (para:paras) shortPages = iter $ State para paras 1 [] (pageLen 1) False []
      where pageLen p = targetHeight - if p `elem` shortPages then 1 else 0
            iter s@State{..}
              | paragraphRemaining == 0 = case paras of
                  [] -> shapes ++ [currentShape]
                  p:ps -> iter s{paragraphRemaining=p, paras=ps}
              | pageRemaining == 0 =
                  iter s{pageNum=(pageNum+1),
                         currentShape=[],
                         pageRemaining=(pageLen (pageNum+1)),
                         shapes=(shapes ++ [currentShape])}
              | paragraphRemaining <= pageRemaining =
                  iter s{paragraphRemaining=0,
                         currentShape=(currentShape ++ [Comp (if isPartial then End else Entire) paragraphRemaining]),
                         pageRemaining=(pageRemaining - paragraphRemaining),
                         isPartial=False}
              | otherwise =
                  iter s{paragraphRemaining=(paragraphRemaining - pageRemaining),
                         pageNum=(pageNum+1),
                         currentShape=[],
                         pageRemaining=(pageLen (pageNum+1)),
                         isPartial=True,
                         shapes=(shapes ++ [(currentShape ++ [Comp (if isPartial then Mid else Begin) pageRemaining])])}
