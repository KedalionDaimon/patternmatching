doubleMe :: Int -> Int
doubleMe x = x + x

--         first arg  next arg  result
protoExtList :: Int -> [Int] -> [Int]
protoExtList n l
  | n == 0    = l
  | otherwise = protoExtList (n - 1) (n:l)
-- protoExtList 4 [] --> [1,2,3,4]

extList :: Int -> [Int]
extList n = protoExtList n []
-- extList 4 --> [1,2,3,4]

-- the below is the same as length:
protoCntList :: Int -> [Int] -> Int
protoCntList n l
  | l == []   = n
  | otherwise = protoCntList (n + 1) (tail l)

cntList :: [Int] -> Int
cntList l = protoCntList 0 l
-- cntList [1,2,3,4] --> 4

protoSpliceList :: Int -> Int -> [Int] -> [[Int]] -> [[Int]]
protoSpliceList n m l r
  | m == n    = ((take n l):r)
  | otherwise = protoSpliceList n (m - 1) (tail l) ((take n l):r)
-- protoSpliceList 3 7 [1,2,3,4,5,6,7] []
-- --> [[5,6,7],[4,5,6],[3,4,5],[2,3,4],[1,2,3]]

protoAllSplice :: Int -> Int -> Int -> [Int] -> [[Int]] -> [[Int]]
protoAllSplice x y m l r
  | y < x     = r
  | otherwise = protoAllSplice x (y - 1) m l ((protoSpliceList y m l []) ++ r)
-- protoAllSplice 3 4 8 [1,2,3,4,5,6,7,8] []
-- --> [[6,7,8],[5,6,7],[4,5,6],[3,4,5],[2,3,4],[1,2,3],[5,6,7,8],[4,5,6,7],[3,4,5,6],[2,3,4,5],[1,2,3,4]]

allSplice :: Int -> Int -> [Int] -> [[Int]]
allSplice x y l = protoAllSplice x y (length l) l []
-- allSplice 2 4 [1,2,3,4,5,6,7,8]
-- --> [[7,8],[6,7],[5,6],[4,5],[3,4],[2,3],[1,2],[6,7,8],[5,6,7],[4,5,6],[3,4,5],[2,3,4],[1,2,3],[5,6,7,8],[4,5,6,7],[3,4,5,6],[2,3,4,5],[1,2,3,4]]

fullSplice :: [Int] -> [[Int]]
fullSplice l = protoAllSplice 1 llen llen l []
                 where llen = (length l)
-- fullSplice [1,2,3,4,5,6,7,8]
-- --> [[8],[7],[6],[5],[4],[3],[2],[1],
--     [7,8],[6,7],[5,6],[4,5],[3,4],[2,3],[1,2],
--     [6,7,8],[5,6,7],[4,5,6],[3,4,5],[2,3,4],[1,2,3],
--     [5,6,7,8],[4,5,6,7],[3,4,5,6],[2,3,4,5],[1,2,3,4],
--     [4,5,6,7,8],[3,4,5,6,7],[2,3,4,5,6],[1,2,3,4,5],
--     [3,4,5,6,7,8],[2,3,4,5,6,7],[1,2,3,4,5,6],
--     [2,3,4,5,6,7,8],[1,2,3,4,5,6,7],
--     [1,2,3,4,5,6,7,8]]

-- TO HELL WITH THIS - SUDDENLY THIS STARTS TO FAIL!
-- OK, NOW CLEAR WHY: test with a list of lists, not just with a list of numbers:
-- [[Int]] is not the same as [Int]!!!
protoCmpLists :: [[Int]] -> [[Int]] -> Int -> Int
protoCmpLists l1 l2 cnt
  | l1 == []          = cnt
  | l2 == []          = 0
  | elem (head l1) l2 = protoCmpLists (tail l1) l2 (cnt + 1)
  | otherwise         = protoCmpLists (tail l1) l2 cnt
-- protoCmpLists [[1],[2],[9],[4]] [[1],[2],[3],[5],[4]] 0 --> 3

-- allright, straight idiotic, this does not work:
-- divi :: Int -> Int -> Float
-- divi a b = (a / b)
-- now try:
-- divi :: Int -> Int -> Float
-- divi a b = ((fromIntegral a) / (fromIntegral b))
-- that works: 3 `divi` 4 --> 0.75

-- valLists :: [[Int]] -> [[Int]] -> Float
-- works with floats:
-- valLists l1 l2 = (fromIntegral (protoCmpLists l1 l2 0)) / (fromIntegral (1 + abs ((length l1) - (length l2))))
-- valLists [[1],[2],[3],[4]] [[2],[6],[5],[3]] --> 2.0
-- now with ints:
valLists :: [[Int]] -> [[Int]] -> Int
valLists l1 l2 = 10 * (protoCmpLists l1 l2 0) `div` (1 + abs ((length l1) - (length l2)))
-- valLists [[1],[2],[3],[4]] [[2],[6],[5],[3]] --> 20

-- AT THIS STAGE, I AM ABLE TO COMPARE HOW SIMILAR TWO INTEGER LISTS ARE --

similarity :: [Int] -> [Int] -> Int
similarity l1 l2 = valLists (fullSplice l1) (fullSplice l2)

