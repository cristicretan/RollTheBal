{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE EmptyDataDecls, MultiParamTypeClasses,
             TypeSynonymInstances, FlexibleInstances,
             InstanceSigs #-}
module RollTheBall where
import Pipes
import ProblemState
import Data.Array as A

{-
    Direcțiile în care se poate mișca o piesa pe tablă
-}

data Directions = North | South | West | East
    deriving (Show, Eq, Ord)

{-
    Sinonim tip de date pentru reprezetarea unei perechi (Int, Int)
    care va reține coordonatele celulelor de pe tabla de joc
-}

type Position = (Int, Int)

{-
    Tip de date pentru reprezentarea celulelor tablei de joc
-}
data Cell = Cell {
ch :: Char,
p :: Position
} deriving Show

{-
    Tip de date pentru reprezentarea nivelului curent
-}

type Size = (Position, Position)

data Level = Level {
                mat :: (Array Position Char)
                }
    deriving (Eq, Ord)
{-
    *** Optional *** 
  
    Dacă aveți nevoie de o funcționalitate particulară,
    instantiați explicit clasele Eq și Ord pentru Level.
    În cazul acesta, eliminați deriving (Eq, Ord) din Level.
-}

{-
    *** TODO ***

    Instanțiati Level pe Show. 
    Atenție! Fiecare linie este urmată de \n (endl in Pipes).
-}

elemAt :: Level -> Position -> Char
elemAt lvl poz = (mat lvl) A.! poz

instance Show Level 
    where show lvl = (++) "\n" $ concat $ foldr ((:) . (\x ->
                        if (snd x) == (snd $ snd $ A.bounds (mat lvl))
                        then
                            [(elemAt lvl x)] ++ "\n"
                        else
                            [(elemAt lvl x)])) [] $ A.indices (mat lvl)

{-
    *** TODO ***
    Primește coordonatele colțului din dreapta jos a hărții.
    Intoarce un obiect de tip Level în care tabla este populată
    cu EmptySpace. Implicit, colțul din stânga sus este (0,0)
-}


emptyLevel :: Position -> Level
emptyLevel poz = Level (A.array ((0, 0), poz) [((x,y), emptySpace) | x<-[0..(fst poz)], y<-[0..(snd poz)]])

{-
    *** TODO ***

    Adaugă o celulă de tip Pipe în nivelul curent.
    Parametrul char descrie tipul de tile adăugat: 
        verPipe -> pipe vertical
        horPipe -> pipe orizontal
        topLeft, botLeft, topRight, botRight -> pipe de tip colt
        startUp, startDown, startLeft, startRight -> pipe de tip initial
        winUp, winDown, winLeft, winRight -> pipe de tip final
    Parametrul Position reprezintă poziția de pe hartă la care va fi adaugată
    celula, dacă aceasta este liberă (emptySpace).
-}

addCell :: (Char, Position) -> Level -> Level
addCell (c, poz) lvl = Level ((mat lvl) A.// [if (i, j) == poz
                                then
                                    ((i, j), c)
                                else
                                    ((i, j), (elemAt lvl (i, j))) | (i, j) <- A.indices (mat lvl)])

{-
    *** TODO *** 

    Primește coordonatele colțului din dreapta jos al hărții și o listă de 
    perechi de tipul (caracter_celulă, poziția_celulei).
    Întoarce un obiect de tip Level cu toate celeule din listă agăugate pe
    hartă.
    Observatie: Lista primită ca parametru trebuie parcursă de la dreapta 
    la stanga.
-}
 
createLevel :: Position -> [(Char, Position)] -> Level
createLevel end cells = foldr (addCell) (emptyLevel end) cells

{-
    *** TODO ***

    Mișcarea unei celule în una din cele 4 direcții 
    Schimbul se poate face doar dacă celula vecină e goală (emptySpace).
    Celulele de tip start și win sunt imutabile.

    Hint: Dacă nu se poate face mutarea puteți lăsa nivelul neschimbat.
-}

-- (startX, startY) - (endX, endY) 
--

isMutableCell :: Position -> Level -> Bool
isMutableCell (x, y) lvl = length (filter (== (elemAt lvl (x, y))) (winningCells ++ startCells ++ [emptySpace])) == 0

validMove :: Position -> Directions -> Level -> Bool
validMove (x, y) North lvl = isMutableCell (x, y) lvl && (x - 1 >= (fst $ fst $ A.bounds (mat lvl)))
validMove (x, y) East lvl  = isMutableCell (x, y) lvl && (y + 1 <= (snd $ snd $ A.bounds (mat lvl)))
validMove (x, y) South lvl = isMutableCell (x, y) lvl && (x + 1 <= (fst $ snd $ A.bounds (mat lvl)))
validMove (x, y) West lvl  = isMutableCell (x, y) lvl && (y - 1 >= (snd $ fst $ A.bounds (mat lvl)))

moveCell :: Position -> Directions -> Level -> Level
moveCell (x, y) direction lvl
    | direction == North = if validMove (x, y) North lvl && (elemAt lvl (x - 1, y)) == emptySpace
                            then
                                addCell (emptySpace, (x, y)) $ addCell ((elemAt lvl (x, y)), (x - 1, y)) lvl
                            else
                                lvl
    | direction == East  = if validMove (x, y) East lvl && (elemAt lvl (x, y + 1)) == emptySpace
                            then
                                addCell (emptySpace, (x, y)) $ addCell ((elemAt lvl (x, y)), (x, y + 1)) lvl
                            else
                                lvl
    | direction == South  = if validMove (x, y) South lvl && (elemAt lvl (x + 1, y)) == emptySpace
                            then
                                addCell (emptySpace, (x, y)) $ addCell ((elemAt lvl (x, y)), (x + 1, y)) lvl
                            else
                                lvl
    | direction == West  = if validMove (x, y) West lvl && (elemAt lvl (x, y - 1)) == emptySpace
                            then
                                addCell (emptySpace, (x, y)) $ addCell ((elemAt lvl (x, y)), (x, y - 1)) lvl
                            else
                                lvl
    | otherwise          = lvl
{-
    *** HELPER ***

    Verifică dacă două celule se pot conecta.
    Atenție: Direcția indică ce vecin este a
    doua celulă pentru prima.

    ex: connection botLeft horPipe East = True (╚═)
        connection horPipe botLeft East = False (═╚)
-}
connection :: Char -> Char -> Directions -> Bool
connection cell1 cell2 direction
    | cell1 == verPipe && direction == North   = cell2 == topLeft || cell2 == topRight || cell2 == verPipe || cell2 == winDown || cell2 == startDown
    | cell1 == verPipe && direction == South   = cell2 == topLeft || cell2 == topRight || cell2 == botLeft || cell2 == botRight || cell2 == verPipe || cell2 == winUp || cell2 == startUp
    | cell1 == horPipe && direction == West    = cell2 == topLeft || cell2 == botLeft || cell2 == horPipe || cell2 == winRight || cell2 == startRight
    | cell1 == horPipe && direction == East    = cell2 == botRight || cell2 == topRight || cell2 == horPipe || cell2 == winLeft || cell2 == startLeft
    | cell1 == topLeft && direction == South   = cell2 == verPipe || cell2 == botLeft || cell2 == botRight || cell2 == winUp || cell2 == startUp
    | cell1 == topLeft && direction == East    = cell2 == botRight || cell2 == topRight || cell2 == horPipe || cell2 == winLeft || cell2 == startLeft
    | cell1 == botLeft && direction == North   = cell2 == verPipe || cell2 == topRight || cell2 == topLeft || cell2 == winDown || cell2 == startDown
    | cell1 == botLeft && direction == East    = cell2 == horPipe || cell2 == botRight || cell2 == topRight || cell2 == winLeft || cell2 == startLeft
    | cell1 == topRight && direction == South  = cell2 == verPipe || cell2 == botRight || cell2 == botLeft || cell2 == startUp || cell2 == winUp
    | cell1 == topRight && direction == West   = cell2 == horPipe || cell2 == botLeft || cell2 == topLeft || cell2 == winRight || cell2 == startRight
    | cell1 == botRight && direction == North  = cell2 == verPipe || cell2 == topLeft || cell2 == topRight || cell2 == winDown || cell2 == startDown
    | cell1 == botRight && direction == West   = cell2 == horPipe || cell2 == botLeft || cell2 == topLeft || cell2 == winRight || cell2 == startRight
    | cell1 == startDown && direction == South = cell2 == verPipe || cell2 == botLeft || cell2 == botRight || cell2 == winUp
    | cell1 == startRight && direction == East = cell2 == horPipe || cell2 == topRight || cell2 == botRight || cell2 == winLeft
    | cell1 == startUp && direction == North   = cell2 == verPipe || cell2 == topLeft || cell2 == topRight || cell2 == winDown
    | cell1 == startLeft && direction == West  = cell2 == horPipe || cell2 == botLeft || cell2 == topLeft || cell2 == winRight
    | otherwise                                = False

{-  *** TODO ***

    Va returna True dacă jocul este câștigat, False dacă nu.
    Va verifica dacă celulele cu Pipe formează o cale continuă de la celula
    de tip inițial la cea de tip final.
    Este folosită în cadrul Interactive.
-}
-- goFurther :: Position -> Level -> Directions -> Bool
-- goFurther = undefined

isWinningCell :: Position -> Level -> Bool
isWinningCell (x, y) lvl = (length $ filter (\el -> el == (elemAt lvl (x, y))) winningCells) > 0

check :: Position -> Level -> Bool
check (x, y) lvl 
    | isInside (x + 1, y) lvl &&
        connection (elemAt lvl (x, y)) (elemAt lvl (x + 1, y)) South = if isWinningCell (x + 1, y) lvl
                                                                        then
                                                                            True
                                                                        else
                                                                            check (x + 1, y) $ addCell (emptySpace, (x, y)) lvl
    | isInside (x, y + 1) lvl &&
        connection (elemAt lvl (x, y)) (elemAt lvl (x, y + 1)) East  = if isWinningCell (x, y + 1) lvl
                                                                        then
                                                                            True
                                                                        else
                                                                            check (x, y + 1) $ addCell (emptySpace, (x, y)) lvl
    | isInside (x - 1, y) lvl &&
        connection (elemAt lvl (x, y)) (elemAt lvl (x - 1, y)) North = if isWinningCell (x - 1, y) lvl
                                                                        then
                                                                            True
                                                                        else
                                                                            check (x - 1, y) $ addCell (emptySpace, (x, y)) lvl
    | isInside (x, y - 1) lvl &&
        connection (elemAt lvl (x, y)) (elemAt lvl (x, y - 1)) West  = if isWinningCell (x, y - 1) lvl
                                                                        then
                                                                            True
                                                                        else
                                                                            check (x, y - 1) $ addCell (emptySpace, (x, y)) lvl
    | otherwise                                                      = False


findStart :: Level -> Position
findStart lvl = head $ filter (\poz -> (elemAt lvl poz) == startDown || (elemAt lvl poz) == startUp ||
                                        (elemAt lvl poz) == startLeft || (elemAt lvl poz) == startRight) $ A.indices (mat lvl)

wonLevel :: Level -> Bool
wonLevel lvl = check (findStart lvl) lvl
-- wonLevel lvl = isWinningCell (check (findStart lvl) lvl) lvl

type Move = (Position, Directions)

isInside :: Position -> Level -> Bool
isInside (x, y) lvl = (x >= (fst $ fst $ A.bounds (mat lvl))) && (x <= (fst $ snd $ A.bounds (mat lvl)))
                            && (y >= (snd $ fst $ A.bounds (mat lvl))) && (y <= (snd $ snd $ A.bounds (mat lvl)))
 
retMoves :: Position -> Level -> Directions -> Move
retMoves (x, y) lvl direction
    | direction == North && isInside (x - 1, y) lvl && validMove (x - 1, y) South lvl     = ((x - 1, y), South)
    | direction == East && isInside (x, y - 1) lvl && validMove (x, y - 1) East lvl       = ((x, y - 1), East)
    | direction == South && isInside (x + 1, y) lvl && validMove (x + 1, y) North lvl     = ((x + 1, y), North)
    | direction == West && isInside (x, y + 1) lvl && validMove (x, y + 1) West lvl       = ((x, y + 1), West)
    | otherwise                                                                           = ((x, y), North)

directions :: [Directions]
directions = [North, East, South, West]

goAround :: Position -> Level -> [Move]
goAround pos lvl = map (retMoves pos lvl) $ filter (\d -> (fst (retMoves pos lvl d)) /= pos) directions


possibleMoves :: Level -> [Move]
possibleMoves lvl = concat $ map (\pos -> goAround pos lvl) $ filter (\poz -> (RollTheBall.elemAt lvl poz) == emptySpace) $ A.indices (mat lvl)

instance ProblemState Level (Position, Directions) where
    -- successors :: s -> [(a, s)]
    successors lvl = zip (possibleMoves lvl) $ map (\mv -> moveCell (fst mv) (snd mv) lvl) $ possibleMoves lvl
    -- isGoal :: s -> Bool
    isGoal = wonLevel
    -- reverseAction :: (a, s) -> (a, s)
    {-
        Primește un tuplu (a1, s1) pentru a reveni
        la starea precedentă stării s1, adică s0,
        prin inversarea acțiunii a1 cu a0.
        Valoarea de retur este (a0,s0)
        Exemplu: ((South, (1, 0)),s1) -> ((North, (2, 0)),s0)
    -}
    reverseAction (((x, y), North), state) = (((x - 1, y), South), moveCell (x - 1, y) South state)
    reverseAction (((x, y), East), state)  = (((x, y + 1), West), moveCell (x, y + 1) West state)
    reverseAction (((x, y), South), state) = (((x + 1, y), North), moveCell (x + 1, y) North state)
    reverseAction (((x, y), West), state)  = (((x, y - 1), East), moveCell (x, y - 1) East state)

