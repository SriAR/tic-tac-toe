------ Start from the first, read my comments to your comments, ignore the "----CE " ones on the way down.
------ Then start from the bottom, and read the CE ones. They are the Explanations for the code I've written,
------ and will naturally make no sense when read from the top.

{--

Overall it's a pretty cool game, hopefully you'll find my criticism helpful!

You should try to decouple game logic from input/output
If you're going to keep it as a simple stdin stdout terminal game have a look
at the "interact" function, it's the only IO function you need and then do the rest
as String -> String (the trick is that you can lazily separate the input lines with "lines"
and update your game without having to wait for all the inputs)
--}

-- I have just read about the interact function a few days back. Will try to implement it in the functions I write from now on!

{-
You should try to make the game not crash, if the input is not a valid number (or an invalid move)
simply discard it and keep the game state the same
-}

-- Yeah I guess using Maybe everywhere is a solution to this problem?
-- I just wanted to try my hand at the bot, so I assumed that the user will give valid inputs :P

{-
Nice bot logic, I'm not too sure exactly how it works, maybe you can refactor the logic to make it
simpler to follow? That would be good for you to spot potential mistakes or improvements that could be made.
-}

-- This was my first try at something that wasn't competitive coding, or something I had to do for class,
-- so I kinda typed out whatever I had in mind, and fixed bugs as I came across them.
-- And I guess competitive programming doesn't really promote beautiful codes over efficient ones :P
-- I'll try to organize my code from now on!

{-
Game suggestions:
- make the bot play against itself to see if it always ends up in a draw,
  I'm guessing the bots play is completely deterministic (since there is no random input) so it might not be fun :/
- try to make ultimate tic tac toe (http://bejofo.net/ttt)
  here's a haskell ncurses implementation (https://github.com/alvare/tateti-tateti)
-}

-- I have just coded the bot to play as 'O', so I can't make it play against itself.
-- But it's easy to make it play as 'X' using the same code, just need to update some things. I'll try it out later!
-- I have no idea what ncurses is, will check it out man.

----CE Your ride has ended :D

import Data.List
import Data.Char
import Data.Array
import Data.Function (on)

{-
Make it fail safe?
something along the lines of:

getInt :: IO (Maybe Int)
getInt = readMaybe <$> getLine
-}

-- Maybe everywhere :D I guess I really need to get aboard the Maybe train :P

getInt :: IO Int
getInt = readLn

data Choice = X | O | Null
    deriving (Eq)

instance Show Choice where
    show k = if k == X then "X" else if k == O then "O" else " "

{-
Is the Int actually carrying any meaningful information?
It looks like it's just the current index (plus 1) so it might be better to remove it
otherwise you're making unexpected states possible i.e [(2,_), (1,_), (3,_), ...]

Use newtype for better perfomance instead of data when there's just one constructor wrapping another type
-}

-- If I have a TTT with X at top left and O at the top right, the board will be of the form
-- G [(1,X),(2,Null),(3,O),(4,Null)...(9,Null)]
-- It just stores the index and the element at that index. This makes it easier for me to
-- pattern match the numbers and check if I have reached a winning combination.

-- Does newtype allow me to define my own show?

data Graph = G [(Int,Choice)]
    deriving (Eq)

type GII = (Graph,[[Int]],[[Int]])

{-
You might want to try to use something like ncurses to display the game,
this way it doesn't keep scrolling and you can just update it in place
-}

-- I have no idea what ncurses is, I will check it out!

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
{-
    I'm personally not a fan of if then else and in this particular case a case statement would be a better fit:

    putStrLn $
        case m of
            O -> "You have lost the match :("
            X ->"You have won the match!"
            Null -> "The match was a tie."

    This also has the benefit that the compiler will give you a warning if you forget a case
-}

    -- Will use cases from now on!
{-
 I'm not too sure what makes the first round different from the other ones
-}

-- From my extensive knowledge acquired by playing TTT during my middle school, I think that the
-- only way the 'X' player wins is if he starts from one of the corners, and the 'O' player doesn't
-- react by choosing the middle square. I wanted to make sure that the bot never loses. Ping me if
-- you find a case where it does :P My middle school knowledge is a bit rusty.

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

{-
 Pretty obscure logic, explain it in a comment?
-}

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

{-
Make this not throw an error?
Don't make the move if it's not a valid move instead of crashing
-}

-- Maybe Nothing :P Maybe (Will do) . Maybe (Maybe) !

----CE insertX inserts an X at the specified index if it is Null, else returns an error.

insertX :: Graph -> Int -> Graph
insertX (G graph) x = if (elem (x,Null) graph) then G (foldr (\(a,b) acc -> if x == a then (x,X):acc else (a,b):acc) [] graph) else error "There is already an element at that place"

----CE empty creates and empty Graph filled with Nulls

empty :: Graph
empty = G (zip [1..] (replicate 9 Null))

{-
G (zip [1..9] $ repeat Null)
but not necessary if you update Graph as I suggested earlier
-}

-- Aren't both essentially the same thing? And if I update Graph without the Int,
-- wouldn't it make it harder for me to pattern match to see if the bot has won?

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

{-
If you parameterise Graph over the content you only have to write one Show instance
and it would print correctly for both Graph Int and Graph Choice
-}

-- Wouldn't having [Choice] for the first graph instead of [(Int,Choice)] mean that
-- it would be harder for me to check if the game has ended?

instance Show Print where
    show (P [a,b,c,d,e,f,i,j,k]) = (sh f3) ++ "---|---|---\n" ++ (sh s3) ++ "---|---|---\n" ++ (sh t3)
        where   (f3,s3,t3) = ([a,b,c],[d,e,f],[i,j,k])
                sh [a,b,c] = " " ++ (show a) ++ " | " ++ (show b) ++ " | " ++ (show c) ++ "\n"
