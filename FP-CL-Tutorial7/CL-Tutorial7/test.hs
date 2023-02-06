{-# LANGUAGE ParallelListComp #-}
checkSquare = [ [ (i, j, n) | i <- [3*k+1..3*k+3], j <- [3*l+1..3*l+3] ] 
                   | k <- [0..2], l <- [0..2], n <- [1..9] ]

checkColumnsNoRepetition = [[(i, j, n), (i', j, n)] | j <- [1..9], n <- [1..9],
                                                    i <- [1..9], i' <- [1..(i-1)]]

--checkSquaresNoRepetition = [[(i, j, n), (i', j', n) | i <- [1..9], i' <- [1..(i-1)], j <- [1..9], j' <- [1..(j-1)]] 
--                                                    | n <- [1..9] ]

--foo :: [[(Int, Int, Int)]]
--foo = [[(i, j, n), (i', j', n)] | i <- [3*a+1..3*a+3], j <- [3*b+1..3*b+3], i' <- [3*a+1..3*a+3], j' <- [3*b+1..3*b+3]
--                                | a <- [0..2], b <- [0..2], n <- [1..9]]

testSquaresNoRepetition = [[[(i, j, n), (i', j', n)]
                            | i <- [(3 * a + 1)..(3 * a + 3)],
                              j <- [(3 * b + 1)..(3 * b + 3)],
                              i' <- [(3 * a + 1)..(3 * b + 3)],
                              j' <- [(3 * b + 1)..(3 * b + 3)],
                              (i, j) < (i', j')]
                          | a <- [0..2], b <- [0..2], n <- [1..9]]


testIneq = [[(i, j), (i', j')] | i <- [1..3], j <- [1..3], i' <- [1..3], j' <- [1..3] ]