import Data.List

paragraphLengths = [ 4, 5, 6, 5, 3, 8, 10, 3, 5, 3, 9, 3, 2, 2, 6 ]
targetHeight = 15

data Comp = Comp Nature Int deriving Show
data Nature = Entire | Begin | Mid | End deriving (Show, Eq)

main = do
    putStrLn "~.~.~  A L B A T R O S S  ~.~.~"
    putStrLn ""
    let shapes = pageShapes paragraphLengths
    putStr "Page shapes "
    print shapes
    case findIndex (any (\(Comp n i) -> n == End && i == 1)) shapes of
        Just x -> putStr "First widow on page " >> print x
        Nothing -> putStr "No widows"


pageShapes :: [Int] -> [[Comp]]
pageShapes (para:paras) =
    iter para paras [] targetHeight False []
      where iter paragraphRemaining paras currentShape pageRemaining isPartial shapes
              | paragraphRemaining == 0 = case paras of
                  [] -> shapes ++ [currentShape]
                  p:ps -> iter p ps currentShape pageRemaining isPartial shapes
              | pageRemaining == 0 =
                  iter paragraphRemaining paras [] targetHeight isPartial
                       (shapes ++ [currentShape])
              | paragraphRemaining <= pageRemaining =
                  iter 0 paras
                       (currentShape ++ [Comp (if isPartial then End else Entire)
                                              paragraphRemaining])
                       (pageRemaining - paragraphRemaining) False shapes
              | otherwise =
                  iter (paragraphRemaining - pageRemaining) paras []
                       targetHeight True
                       (shapes ++ [(currentShape ++ [Comp (if isPartial then Mid else Begin)
                                                          pageRemaining])])
