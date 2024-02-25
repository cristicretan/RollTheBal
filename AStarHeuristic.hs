{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies,
  FlexibleContexts, InstanceSigs #-}

module AStarHeuristic where
import RollTheBall
import ProblemState
import Pipes

import Data.Array as A
import Data.Hashable
import Data.Graph.AStar
import qualified Data.HashSet as H

{-
	Semnătura lui aStar este:
		aStar 	:: (Hashable a, Ord a, Ord c, Num c)
			=> (a -> HashSet a)
			-> (a -> a -> c)
			-> (a -> c)
			-> (a -> Bool)
			-> a
			-> Maybe [a], a fiind tipul Level.
	Vom discuta mai jos despre fiecare parametru în parte.
-}

{-
 	*** TODO ***
	
	O constrângere pe care trebuie să o îndeplinească Level pentru a apela
	AStar pe graful nostru este ca Level să fie instanță a lui Hashable.
	Pentru a face asta, trebuie să definim funcția hashWithSalt. Aceasta
	va primi un Int, Level-ul nostru și va returna un Int, adica hash-ul
	rezultat.

	Hint: Dacă un tip de dată 'a' este instanță de Hashable, atunci și
	'[a]' este. Pe considerentul acesta, o sugestie ar fi de defini o
	funcție care extrage din reprezentarea lui Level, Cells sub formă de
	listă sau lista de liste. Următorul pas ar fi să instanțiem Cell pe
	Hashable. Char este, de asemenea, instanța Hashable.

	În cazul acesta,  avem

	    hashWithSalt i level = hashWithSalt i $ toList level
 -}
levelToList :: Level -> [Cell]
levelToList lvl = map (\(c, poz) -> Cell c poz) $ zip (map (elemAt lvl) $ A.indices (mat lvl)) $ A.indices (mat lvl)

instance Hashable Cell where
	hashWithSalt :: Int -> Cell -> Int
	hashWithSalt i cell = hashWithSalt i (ch cell, p cell)

instance Hashable Level where
	hashWithSalt :: Int -> Level -> Int
	hashWithSalt i level = hashWithSalt i $ levelToList level
{-
	*** TODO ***
	
	Primul parametru al lui aStar ne va returna graf-ul pe care îl
	parcurgem. Acesta este sub forma unei funcții care primește drept
	parametru un nod, aici un Level, și întoarce vecinii săi, sub formă de
	HashSet.

	Hint: fromList
-}

neighbours :: (Level -> H.HashSet Level)
neighbours lvl = H.fromList $ map snd $ successors lvl

{-
 	*** TODO ***

	Urmează distanța dintre noduri. Aceasta este o funcție care primește
	două elemente de tip Level și întoarce un număr care reprezintă
	distanța dintre ele.

	Atenție! Aceasta va fi apelată doar pe nivele adiacente!
 -}
 
distance :: (Num c) => (Level -> Level -> c)
distance _ _ = 1
-- distance lvl1 lvl2 = (heuristic (findStart lvl1) lvl1 0) - (heuristic (findStart lvl1) lvl1 0)

{-
	Urmează euristica folosită.

	Primul apel pe aStar va fi folosind o euristică banală, care întoarce 1,
	indiferent ne nivel.
-}
trivialHeuristic :: (Num a) => Level -> a
trivialHeuristic _ = 1

{-
	*** TODO ***
	
	Dacă există, aStar returnează un drum optim de către nodul final,
	excluzând nodul de început.
-}

heuristic :: (Num c) => Position -> Level -> c -> c
heuristic (x, y) lvl d
    | isInside (x + 1, y) lvl &&
        connection (elemAt lvl (x, y)) (elemAt lvl (x + 1, y)) South = if isWinningCell (x + 1, y) lvl
                                                                        then
                                                                            d
                                                                        else
                                                                            heuristic (x + 1, y) (addCell (emptySpace, (x, y)) lvl) (d + 1)
    | isInside (x, y + 1) lvl &&
        connection (elemAt lvl (x, y)) (elemAt lvl (x, y + 1)) East  = if isWinningCell (x, y + 1) lvl
                                                                        then
                                                                            d
                                                                        else
                                                                            heuristic (x, y + 1) (addCell (emptySpace, (x, y)) lvl) (d + 1)
    | isInside (x - 1, y) lvl &&
        connection (elemAt lvl (x, y)) (elemAt lvl (x - 1, y)) North = if isWinningCell (x - 1, y) lvl
                                                                        then
                                                                            d
                                                                        else
                                                                            heuristic (x - 1, y) (addCell (emptySpace, (x, y)) lvl) (d + 1)
    | isInside (x, y - 1) lvl &&
        connection (elemAt lvl (x, y)) (elemAt lvl (x, y - 1)) West  = if isWinningCell (x, y - 1) lvl
                                                                        then
                                                                            d
                                                                        else
                                                                            heuristic (x, y - 1) (addCell (emptySpace, (x, y)) lvl) (d + 1)
    | otherwise                                                      = d



nonTrivialHeuristic :: (Num c) => Level -> c
nonTrivialHeuristic lvl = 1

{-
 	*** TODO ***

	Penultimul parametru este o funcție care verifică dacă nodul curent
	este final, sau nu.
 -}
isGoalNode :: Level -> Bool
isGoalNode = wonLevel

{-
 	Ultimul parametru dat lui aStar reprezintă nodul de la care se
	începe căutarea.

	Dacă există, aStar returnează un drum optim de către nodul final,
	excluzând nodul de început.
-}
