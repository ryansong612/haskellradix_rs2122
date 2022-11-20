module Tests where
import IC.TestSuite
import Radix


-- No autotests for and, or, size, but you can try the following manually:
-- *Main> and One Zero
-- Zero
-- *Main> and One One
-- One
-- *Main> or One Zero
-- One
-- *Main> sizeITt1
-- 40
-- *Main> sizeRTt2
-- 28
-- *Main> sizeRTfigure
-- 37


-- *Main> binary 0
-- [0]
-- *Main> binary 1
-- [1]
-- *Main> binary 13
-- [1,1,0,1]
binaryTestCases
  = [ 0 ==> [0],
      1 ==> [1],
      13 ==> [1,1,0,1]
    ]


insertTestCases
  = [ ([0],(Leaf Zero)) ==> Node Zero (Leaf One) (Leaf Zero),
      ([1],(Leaf Zero)) ==> Node Zero (Leaf Zero) (Leaf One),
      ([0],(insert [1] (Leaf Zero))) ==> Node Zero (Leaf One) (Leaf One),
      ([0,1],(insert [1] (Leaf Zero))) ==> Node Zero (Node Zero (Leaf Zero)
      (Leaf One)) (Leaf One)
    ]


-- *Main> buildRadixTree []
-- Leaf Zero
-- *Main> buildRadixTree [5,3,2]
-- Node Zero (Leaf Zero) (Node Zero (Node One (Leaf Zero) (Leaf One))
-- (Leaf One))
-- *Main> buildRadixTree [0,1,3,7,12] == figure
-- True
-- *Main> buildRadixTree [12,0,3,1,7] == figure
-- True
buildRadixTreeTestCases
  = [ [] ==> Leaf Zero,
      [5,3,2] ==> Node Zero (Leaf Zero) (Node Zero (Node One (Leaf Zero)
      (Leaf One)) (Leaf One)),
      ([0,1,3,7,12]) ==> figure,
      ([12,0,3,1,7]) ==> figure
    ]


-- *Main> member 4 (buildRadixTree [1,3,7])
-- False
-- *Main> member 7 (buildRadixTree [1,3,7])
-- True
-- *Main> member 2 (buildRadixTree [])
-- False
-- *Main> member 0 (buildRadixTree [0])
-- True
-- *Main> member 6 (insert [1,1,0] figure)
-- True
memberTestCases
  = [ (4,(buildRadixTree [1,3,7])) ==> False,
      (7,(buildRadixTree [1,3,7])) ==> True,
      (2,(buildRadixTree [])) ==> False,
      (0,(buildRadixTree [0])) ==> True,
      (6,(insert [1,1,0] figure)) ==> True
    ]


-- *Main> union (buildRadixTree [1,3,7]) (buildRadixTree [2,3,4,7])
-- Node Zero (Leaf Zero) (Node One (Node One (Leaf One) (Leaf Zero))
-- (Node One (Leaf Zero) (Leaf One)))
-- *Main> union figure (buildRadixTree []) == figure
-- True
unionTestCases
  = [ ((buildRadixTree [1,3,7]),(buildRadixTree [2,3,4,7])) ==> Node Zero
      (Leaf Zero) (Node One (Node One (Leaf One) (Leaf Zero)) (Node One
      (Leaf Zero) (Leaf One))),
      (figure,(buildRadixTree [])) ==> figure
    ]


-- *Main> intersection (buildRadixTree [1,3,7]) (buildRadixTree [2,3,4,7])
-- Node Zero (Leaf Zero) (Node Zero (Leaf Zero) (Node One (Leaf Zero)
-- (Leaf One)))
-- *Main> intersection figure (buildRadixTree [])
-- Leaf Zero
intersectionTestCases
  = [ ((buildRadixTree [1,3,7]),(buildRadixTree [2,3,4,7])) ==> Node Zero
     (Leaf Zero) (Node Zero (Leaf Zero) (Node One (Leaf Zero) (Leaf One)))
     ,(figure,(buildRadixTree [])) ==> Leaf Zero
    ]


-- You can add your own test cases above

allTestCases
  = [
      TestCase "binary" (binary)
              binaryTestCases
    , TestCase "insert" (uncurry insert)
              insertTestCases
    , TestCase "buildRadixTree" (buildRadixTree)
              buildRadixTreeTestCases
    , TestCase "member" (uncurry member)
              memberTestCases
    , TestCase "union" (uncurry union)
              unionTestCases
    , TestCase "intersection" (uncurry intersection)
              intersectionTestCases
    ]


runTests = mapM_ goTest allTestCases

main = runTests