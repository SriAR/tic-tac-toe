------ Start from the bottom, and read the CE ones. They are the Explanations for the code I've written,
------ and will naturally make no sense when read from the top.

----CE Your ride has ended :D

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
                
----CE Printing,printing and more printing. Do the first round, and send it to the rest.

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

----CE Explained above I guess :P

firstRound :: GII -> IO GII
firstRound (a,b,c) = do
    k <- getInt
    let new = oneMoveX (a,b,c) k
    print (f new)
    m <- if k `elem` [1,3,9,7] then return (oneMovespecial new 5) else return (oneMoveO new)
    print (f m)
    return m

----CE Prompts to get index, gets index,creates new (g,ours,theirs) with that index entered,
----CE prints the current board. If the board is full, return Null. Else, if player has won the game,
----CE return X. If not, allow the bot to make a move, print the new board. Check if the bot has won
----CE the game. If yes, return O. Else, do this again.

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

----CE It takes the current state of the board, the place where the user wants to input his move,
----CE and gives the new board.

oneMoveX :: GII -> Int -> GII
oneMoveX (g,ours,theirs) x = ((insertX g x),(planFoiled ours x),(closer2victory theirs x))

----CE This is for the "firstRound" function. I have to force the bot to move to the index 5, and
----CE not do "bot things" by itself, not obeying it's master.

oneMovespecial :: GII -> Int -> GII
oneMovespecial (g,ours,theirs) x = (( insertO g x),(closer2victory ours x),(planFoiled theirs x))

----CE (g,ours,theirs) == (Current Graph, Our winning combinations, Their winning combinations)
----CE Since the combinations are sorted by length (i.e. number of moves left to win the game),
----CE I defined the next move for O as follows (O is the bot always)
----CE  -- If I have a winning move which requires just one move, i.e. (length (head (our winning move))) == 1, then take it and win the game.
----CE  -- If they have a winning move which requires one move, block it.
----CE  -- Otherwise, do your most optimal move, which is the first move in the first winning combination. i.e.(head (head ours)) (which will be a single move).

oneMoveO :: GII -> GII
oneMoveO (g,ours,theirs)
    | length (hea ours) == 1 = ((insertO g k),(closer2victory ours k),(planFoiled theirs k))
    | length(hea theirs) == 1 = ((insertO g x),(closer2victory ours x),(planFoiled theirs x))
    | otherwise = ((insertO g n),(closer2victory ours n),(planFoiled theirs n))
        where   k = head (head ours)
                x = head (head theirs)
                n = if length (hea ours) /= 5 then head (head ours) else head (head theirs)

----CE If I have a winning move where I have no elements left, it means that I have won! (else I haven't :( and the game goes on)

gameOver :: GII -> (Choice,Bool)
gameOver (g,ours,theirs) = if hea ours == [] then (O,True) else if hea theirs == [] then (X,True) else (Null,False)

----CE This is my master hack (lol). I got tired of the "head of empty list" errors
----CE which I got when a person had no winning move, and so the program couldn't find out
----CE what move the person should make. Since each winning move has only 3 or less elements, so
----CE I define the "hea" function as the normal head when it has an element, but set up a flag
----CE to raise an error when it contains 5 elements (since it normally shouldn't).

hea [] = [2,3,4,6,5]
hea (x:xs) = x

----CE planFoiled takes a set of combinations which can make a player win,
----CE takes the move that the OTHER player has made, and removes it from your
----CE winning combinations, as that means you can't finish the game using those combinations.
----CE This is also updated separately for both players. 

planFoiled :: [[Int]] -> Int -> [[Int]]
planFoiled ks k =foldr (\xs acc -> if elem k xs then acc else xs:acc) [] ks

----CE closer2victory takes a set of combinations which can make a player win,and
----CE takes the current index where the SAME player has inserted an element, and redefines how
----CE many more steps he would need to make to win. This is maintained separately for X and
----CE for O. For example, assume that this is the first move, and so X has the possible
----CE winning combinations as [[1,2,3],[4,5,6],[7,8,9],[1,4,7],[2,5,8],[3,6,9],[1,5,9],[3,5,7]]
----CE Now, assume he inserts an element at the index '1'. Now his winning combinations
----CE are [[2,3],[4,7],[5,9],[4,5,6],[7,8,9],[2,5,8],[3,6,9],[3,5,7]], (sorted by length
----CE since those require the minimum number of moves for him to win.

closer2victory :: [[Int]] -> Int -> [[Int]]
closer2victory ks k = sortBy (compare `on` length) $ map (\xs -> if elem k xs then (delete k xs) else xs) ks

----CE winCombi is basically all the combinations of indices which ends the game.

winCombi :: [[Int]]
winCombi = [[1,2,3],[4,5,6],[7,8,9],[1,4,7],[2,5,8],[3,6,9],[1,5,9],[3,5,7]]

----CE insertO is the same, except no checking for Null, since the bot won't insert data
----CE at places already occupied. For reason, see above.

insertO :: Graph -> Int -> Graph
insertO (G graph) x = G (foldr (\(a,b) acc -> if x == a then (x,O):acc else (a,b):acc) [] graph)

----CE insertX inserts an X at the specified index if it is Null, else returns an error.

insertX :: Graph -> Int -> Graph
insertX (G graph) x = if (elem (x,Null) graph) then G (foldr (\(a,b) acc -> if x == a then (x,X):acc else (a,b):acc) [] graph) else error "There is already an element at that place"

----CE empty creates and empty Graph filled with Nulls

empty :: Graph
empty = G (zip [1..] (replicate 9 Null))

----CE First,second and third of a 3-tuple.

f :: (a,b,c) -> a
f (a,b,c) = a

s :: (a,b,c) -> b
s (a,b,c) = b

th :: (a,b,c) -> c
th (a,b,c) = c

----CE This prints the grid the first time to show the index of each position so that
----CE further inputs can be obtained.

data Print = P [Int]

instance Show Print where
    show (P [a,b,c,d,e,f,i,j,k]) = (sh f3) ++ "---|---|---\n" ++ (sh s3) ++ "---|---|---\n" ++ (sh t3)
        where   (f3,s3,t3) = ([a,b,c],[d,e,f],[i,j,k])
                sh [a,b,c] = " " ++ (show a) ++ " | " ++ (show b) ++ " | " ++ (show c) ++ "\n"
