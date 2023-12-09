import Data.List

paragraphLengths = [ 4, 5, 6, 5, 3, 8, 10, 3, 5, 3, 9, 3, 2, 2, 6 ]
targetHeight = 15

data Comp = Comp Nature Int deriving Show
data Nature = Entire | Begin | Mid | End deriving (Show, Eq)

main = do
    putStrLn "~.~.~  A L B A T R O S S  ~.~.~"
    putStrLn ""
    let shapes = pageShapes paragraphLengths [2]
    putStr "Page shapes "
    print shapes
    case findFirstWidow shapes of
        Just x -> putStr "First widow on page " >> print x
        Nothing -> putStr "No widows"


findFirstWidow :: [[Comp]] -> Maybe Int
findFirstWidow = findIndex (any (\(Comp n i) -> n == End && i == 1))


pageShapes :: [Int] -> [Int] -> [[Comp]]
pageShapes (para:paras) shortPages = iter para paras 0 [] (pageLen 0) False []
      where pageLen p = targetHeight - if (p+1) `elem` shortPages then 1 else 0
            iter paragraphRemaining paras pageNum currentShape pageRemaining isPartial shapes
              | paragraphRemaining == 0 = case paras of
                  [] -> shapes ++ [currentShape]
                  p:ps -> iter p ps pageNum currentShape pageRemaining isPartial shapes
              | pageRemaining == 0 =
                  iter paragraphRemaining paras (pageNum+1) [] (pageLen (pageNum+1)) isPartial
                       (shapes ++ [currentShape])
              | paragraphRemaining <= pageRemaining =
                  iter 0 paras pageNum
                       (currentShape ++ [Comp (if isPartial then End else Entire)
                                              paragraphRemaining])
                       (pageRemaining - paragraphRemaining) False shapes
              | otherwise =
                  iter (paragraphRemaining - pageRemaining) paras (pageNum+1) []
                       (pageLen (pageNum+1)) True
                       (shapes ++ [(currentShape ++ [Comp (if isPartial then Mid else Begin)
                                                          pageRemaining])])
