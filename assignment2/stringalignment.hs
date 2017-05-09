
import Data.List


scoreMatch = 0
scoreMismatch = -1
scoreSpace = -1
string1 = "writer"
string2 = "vintner"
string3 = "apa "
string4 = "apa"
string5 = "aka"

--test for attachTails
h1 = "o"
h2 = "a"

aList1 = "le"
aList2 = "pa"
aList = [(stringList,stringList)]

stringList = ["cs", "efd", "lth", "it"]


-- 2a.)
--returns the score of the optimal alignment of the two strings 
similarityScore :: String -> String -> Int
similarityScore string1 string2 = simScore (length string1) (length string2)
  where
    simScore i j = simTable!!i!!j
    simTable = [[ simEntry i j | j<-[0..]] | i<-[0..] ]
       
    simEntry :: Int -> Int -> Int
    simEntry i 0 = i * scoreSpace
    simEntry 0 j = j * scoreSpace
    simEntry i j = maximum [(simScore (i-1) (j-1)) + (score x y), (simScore i (j-1)) + (score x '-'), (simScore (i-1) j) + (score '-' y)]
      where
         x = string1!!(i-1)
         y = string2!!(j-1)

score :: Char -> Char -> Int
score  _ '-' = scoreSpace
score  '-' _ = scoreSpace
score x y 
    |x == y = scoreMatch
    |otherwise  = scoreMismatch

--2b.)
--Appends h1 and h2 to start of each list in aList
attachHeads :: a -> a -> [([a],[a])] -> [([a],[a])] 
attachHeads h1 h2 aList = [(h1:xs,h2:ys) | (xs,ys) <- aList]

--Appends h1 and h2 to the end of each list in aList
attachTails :: a -> a -> [([a],[a])] -> [([a],[a])]
attachTails t1 t2 aList = [(xs++[t1],ys++[t2]) | (xs,ys) <- aList]


--generalizes the maximum function :
--1. The "value" of an element is defined by a function supplied as a parameter.
--2. Instead of just one element, the result is a list of all maximum elements.
-- For example, maximaBy length ["cs", "efd", "lth", "it"] should return ["efd", "lth"].
maximaBy :: Ord b => (a -> b) -> [a] -> [a] 
maximaBy valueFcn xs = helper (elemIndices (maximum valueList) valueList) xs 
  where
    helper _ [] = []
    helper [] _ = []
    helper (i:indexList) xs = (xs !! i) : helper indexList xs
    valueList = (map valueFcn xs)


type AlignmentType = (String,String)

optScore :: Char -> Char -> Int
optScore  _ '-' = scoreSpace
optScore  '-' _ = scoreSpace
optScore x y 
    |x == y = scoreMatch
    |otherwise  = scoreMismatch

--returns a list of all optimal alignments between string1 and string2
optAlignments :: String -> String -> [AlignmentType]
optAlignments string1 string2 = snd $ optAl (length string1) (length string2)
  where
    optAl i j = optTable!!i!!j
    optTable = [[ optEntry i j | j <-[0..]] | i<-[0..] ]

    add :: Char -> Char -> (Int, [AlignmentType]) -> (Int, [AlignmentType])
    add t1 t2 (i, a) = ((score t1 t2) + i, attachTails t1 t2 a)

    sx, sy :: Int -> Char
    sx x = string1!!(x-1)
    sy y = string2!!(y-1)

    optEntry :: Int -> Int -> (Int, [AlignmentType])
    optEntry 0 0 = (0, [([],[])])
    optEntry i 0 = add (sx i) '-' $ optAl (i-1) 0
    optEntry 0 j = add '-' (sy j) $ optAl 0 (j-1)
    optEntry i j = (fst.head $ result, concat.map snd $ result)
      where
        result = maximaBy fst [add (sx i) (sy j) $ optAl (i-1) (j-1),add (sx i) '-' $ optAl (i-1) j, add '-' (sy j) $ optAl i (j-1)]

showAl :: AlignmentType -> String
showAl alignment = (fst alignment) ++ "\n" ++ (snd alignment) ++ "\n\n\n" 

outputOptAlignments :: String -> String -> IO ()
outputOptAlignments string1 string2 = do

  let alignments = optAlignments string1 string2
  putStrLn $ "There are " ++ (show.length $ alignments) ++ " optimal alignments:\n\n"
  mapM_ putStr $ map showAl $ alignments 
  putStrLn $ "There were " ++ (show.length $ alignments) ++ " optimal alignments!"  
