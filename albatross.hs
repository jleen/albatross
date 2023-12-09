import Debug.Trace (traceShow)

paragraphLengths = [ 4, 5, 6, 5, 3, 8, 10, 3, 5, 3, 9, 3, 2, 2, 6 ]
targetHeight = 15

data Comp = Comp Nature Int deriving Show
data Nature = Entire | Begin | Mid | End deriving Show

main = do
    putStrLn "~.~.~  A L B A T R O S S  ~.~.~"
    print $ pageShape paragraphLengths

pageShape :: [Int] -> [[Comp]]
pageShape (para:paras) =
    iter para paras [] targetHeight False []
      where iter paragraphRemaining paras currentShape pageRemaining isPartial shapes
              | paragraphRemaining == 0 = case paras of
                  [] -> shapes
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
