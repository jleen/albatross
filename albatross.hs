{-# LANGUAGE RecordWildCards #-}

import Data.List
import Debug.Trace

paragraphLengths = [ 4, 5, 6, 5, 3, 8, 10, 3, 5, 3, 9, 3, 2, 2, 6 ]
targetHeight = 15

data Comp = Comp Nature Int deriving Show
data Nature = Entire | Begin | Mid | End deriving (Show, Eq)

main = do
    putStrLn "~.~.~  A L B A T R O S S  ~.~.~"
    putStrLn ""
    print $ pageShapesNoWidows paragraphLengths []


findFirstWidow :: [[Comp]] -> Maybe Int
findFirstWidow = fmap succ . findIndex (any (\(Comp n i) -> n == End && i == 1))


spread p = [v,v+1] where v = 2 * (p `div` 2)


pageShapesNoWidows :: [Int] -> [Int] -> Maybe [[Comp]]
pageShapesNoWidows paras shortPages =
    traceShow shortPages $
    let shapes = pageShapes paras shortPages
    in case findFirstWidow shapes of
        Nothing -> Just shapes
        Just p -> if p `elem` shortPages then Nothing else pageShapesNoWidows paras (shortPages ++ spread p)
        
    
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
      where pageLen p = targetHeight - if (p+1) `elem` shortPages then 1 else 0
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
