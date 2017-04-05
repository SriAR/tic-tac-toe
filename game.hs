import Data.List
import Data.Char
import Data.Array
import Data.Function (on)

getInt :: IO Int
getInt = readLn

data Choice = X | O | Null
    deriving (Eq)

instance Show Choice where
    show k = if k == X then "X" else if k == O then "O" else " "

data Graph = G [(Int,Choice)]
    deriving (Eq)

type GII = (Graph,[[Int]],[[Int]])

instance Show Graph where
    show (G [a,b,c,d,e,f,i,j,k]) = (sh f3) ++ "---|---|---\n" ++ (sh s3) ++ "---|---|---\n" ++ (sh t3)
        where   (f3,s3,t3) = (map snd [a,b,c],map snd [d,e,f],map snd [i,j,k])
                sh [a,b,c] = " " ++ (show a) ++ " | " ++ (show b) ++ " | " ++ (show c) ++ "\n"

main = do
    putStrLn "          TIC TAC TOE bot v0.1          \n"
    putStrLn "The index of the squares is the following:\n"
    print (P [1..9])
    getLine
    putStrLn "You will begin the match as 'X'"
    getLine
    let (g,ours,theirs) = (empty,winCombi,winCombi)
    print g
    putStrLn "Enter your index:"
    (a,b,c) <- firstRound (g,ours,theirs)
    m <- startRounds (a,b,c)
    putStrLn (if m == O then "You have lost the match :(" else if m == X then "You have won the match!" else "The match was a tie.")

firstRound :: GII -> IO GII
firstRound (a,b,c) = do
    k <- getInt
    let new = oneMoveX (a,b,c) k
    print (f new)
    m <- if k `elem` [1,3,9,7] then return (oneMovespecial new 5) else return (oneMoveO new)
    print (f m)
    return m

startRounds :: GII -> IO Choice
startRounds t@(g,ours,theirs) = do
    putStrLn "Enter your index:"
    k <- getInt
    let new = oneMoveX t k
    print (f new)
    if (((th new)==[])&&((s new)==[])) then return Null
    else do
        if (snd (gameOver new)) then return (fst (gameOver new))
        else do
            let next = oneMoveO new
            print (f next)
            if (snd (gameOver next)) then return (fst (gameOver next))
            else startRounds next

oneMoveX :: GII -> Int -> GII
oneMoveX (g,ours,theirs) x = ((insertX g x),(planFoiled ours x),(closer2victory theirs x))

oneMovespecial :: GII -> Int -> GII
oneMovespecial (g,ours,theirs) x = (( insertO g x),(closer2victory ours x),(planFoiled theirs x))

oneMoveO :: GII -> GII
oneMoveO (g,ours,theirs)
    | length (hea ours) == 1 = ((insertO g k),(closer2victory ours k),(planFoiled theirs k))
    | length(hea theirs) == 1 = ((insertO g x),(closer2victory ours x),(planFoiled theirs x))
    | otherwise = ((insertO g n),(closer2victory ours n),(planFoiled theirs n))
        where   k = head (head ours)
                x = head (head theirs)
                n = if length (hea ours) /= 5 then head (head ours) else head (head theirs)

gameOver :: GII -> (Choice,Bool)
gameOver (g,ours,theirs) = if hea ours == [] then (O,True) else if hea theirs == [] then (X,True) else (Null,False)

hea [] = [2,3,4,6,5]
hea (x:xs) = x

planFoiled :: [[Int]] -> Int -> [[Int]]
planFoiled ks k =foldr (\xs acc -> if elem k xs then acc else xs:acc) [] ks

closer2victory :: [[Int]] -> Int -> [[Int]]
closer2victory ks k = sortBy (compare `on` length) $ map (\xs -> if elem k xs then (delete k xs) else xs) ks

winCombi :: [[Int]]
winCombi = [[1,2,3],[4,5,6],[7,8,9],[1,4,7],[2,5,8],[3,6,9],[1,5,9],[3,5,7]]

insertO :: Graph -> Int -> Graph
insertO (G graph) x = G (foldr (\(a,b) acc -> if x == a then (x,O):acc else (a,b):acc) [] graph)

insertX :: Graph -> Int -> Graph
insertX (G graph) x = if (elem (x,Null) graph) then G (foldr (\(a,b) acc -> if x == a then (x,X):acc else (a,b):acc) [] graph) else error "There is already an element at that place"

empty :: Graph
empty = G (zip [1..] (replicate 9 Null))

f :: (a,b,c) -> a
f (a,b,c) = a

s :: (a,b,c) -> b
s (a,b,c) = b

th :: (a,b,c) -> c
th (a,b,c) = c

data Print = P [Int]

instance Show Print where
    show (P [a,b,c,d,e,f,i,j,k]) = (sh f3) ++ "---|---|---\n" ++ (sh s3) ++ "---|---|---\n" ++ (sh t3)
        where   (f3,s3,t3) = ([a,b,c],[d,e,f],[i,j,k])
                sh [a,b,c] = " " ++ (show a) ++ " | " ++ (show b) ++ " | " ++ (show c) ++ "\n"
