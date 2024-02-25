{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Search where

import ProblemState
{-
    *** TODO ***

    Tipul unei nod utilizat în procesul de căutare. Recomandăm reținerea unor
    informații legate de:

    * stare;
    * acțiunea care a condus la această stare;
    * nodul părinte, prin explorarea căruia a fost obținut nodul curent;
    * adâncime
    * copiii, ce vor desemna stările învecinate
-}

data Node s a = Node {
state :: s,
action :: Maybe a,
parent :: Maybe (Node s a),
depth :: Int,
children :: [Node s a]} deriving (Show, Eq)

{-
    *** TODO ***
    Gettere folosite pentru accesul la câmpurile nodului
-}
nodeState :: Node s a -> s
nodeState = state

nodeParent :: Node s a -> Maybe (Node s a)
nodeParent = parent

nodeDepth :: Node s a -> Int
nodeDepth = depth

nodeAction :: Node s a -> Maybe a
nodeAction = action

nodeChildren :: Node s a -> [Node s a]
nodeChildren = children

{-
    *** TODO ***

    Generarea întregului spațiu al stărilor
    Primește starea inițială și creează nodul corespunzător acestei stări,
    având drept copii nodurile succesorilor stării curente.
-}

createChildren :: (ProblemState s a, Eq s) => Node s a -> [Node s a]
createChildren root = map (\(a, curent_s) -> Node curent_s (Just a) (Just root) ((nodeDepth root) + 1) []) (successors $ nodeState root)

createStateSpaceHelper :: (ProblemState s a, Eq s) => Node s a -> Int -> Node s a
createStateSpaceHelper root d = Node (state root) (action root) (parent root) (d + 1) (map (flip createStateSpaceHelper (d + 1)) (createChildren root))

createStateSpace :: (ProblemState s a, Eq s) => s -> Node s a
createStateSpace root = createStateSpaceHelper (Node root Nothing Nothing 0 []) 0

{-
    *** TODO ***
   
    Primește un nod inițial și întoarce un flux de perechi formate din:
    * lista nodurilor adăugate în frontieră la pasul curent
    * frontiera

-}

-- testbfs lvl = Prelude.map (\node -> (nodeState node, nodeState (fromJust (nodeParent node)))) b
--     where 
--         x = createStateSpace lvl 
--         -- b =  nodeChildren $ head $ nodeChildren x -- nodeChildren x
--         b = nodeChildren x

-- testbfs1 lvl = Prelude.map (\node -> ((Prelude.map nodeState (fst node)), (Prelude.map nodeState (snd node)))) b
--     where 
--         x = bfs $ createStateSpace lvl 
--         b = take 3 x

-- testbidir lvl1 lvl2 = (((nodeState (fst y)), (if ((nodeParent (fst y)) == Nothing) then Nothing else Just $ (nodeState $ fromJust $ nodeParent (fst y)))), ((nodeState (snd y)), (if (nodeParent (snd y)) == Nothing then Nothing else Just $ (nodeState $ fromJust $ nodeParent (snd y)))) )
--     where 
--         y = bidirBFS (createStateSpace lvl1) (createStateSpace lvl2)

bfs :: Ord s => Node s a -> [([Node s a], [Node s a])]
bfs root = ([root], [root]) : bfsHelper [root] [root] []


bfsHelper :: Ord s => [Node s a] -> [Node s a] -> [Node s a] -> [([Node s a], [Node s a])]
bfsHelper _ [] _ = [([], [])]
bfsHelper _ (x:xs) visited = if not (checkVisited x visited)
                                then
                                    (nodeChildren x, xs ++ (nodeChildren x)) : bfsHelper (nodeChildren x) (xs ++ (nodeChildren x)) (x : visited)
                                else
                                    bfsHelper [] xs visited


checkVisited :: (Eq s) => Node s a -> [Node s a] -> Bool
checkVisited st visited = (length (filter (\x -> (nodeState st) == (nodeState x)) visited)) /= 0

{-1
    *** TODO ***
  
    Primește starea inițială și finală și întoarce o pereche de noduri, reprezentând
    intersecția dintre cele două frontiere.
-}

bidirBFS :: Ord s => Node s a -> Node s a -> (Node s a, Node s a)
bidirBFS start end = bidirBFSHelper1 (zip (bfs start) (bfs end))


bidirBFSHelper1 :: Ord s => [(([Node s a], [Node s a]), ([Node s a], [Node s a]))] -> (Node s a, Node s a)
bidirBFSHelper1 [] = undefined
bidirBFSHelper1 (((n1, f1),(n2, f2)):xs)
    | checkEquality n1 f2 = head $ [(x, y) | x <- n1, y <- f2, (nodeState x) == (nodeState y)]
    | checkEquality n2 f1 = head $ [(x, y) | x <- n2, y <- f1, (nodeState x) == (nodeState y)]
    | otherwise = bidirBFSHelper1 xs

checkEquality :: (Eq s) => [Node s a] -> [Node s a] -> Bool
checkEquality [] [] = False
checkEquality xs ys = foldr (||) False $ [(nodeState x) == (nodeState y) | x <- xs, y <- ys]

{-
    *** TODO ***

    Pornind de la un nod, reface calea către nodul inițial, urmând legăturile
    către părinți.

    Întoarce o listă de perechi (acțiune, stare), care pornește de la starea inițială
    și se încheie în starea finală.

-}

extractPath :: Node s a -> [(Maybe a, s)]
extractPath Node {state = s, action = _, parent = Nothing, depth = _, children = _} = [(Nothing,s)]
extractPath root = case (parent root) of
        Nothing   -> [(Nothing, (state root))]
        Just node -> extractPath node ++ [(nodeAction root, nodeState root)]
{-
    *** TODO ***

    Pornind de la o stare inițială și una finală, se folosește de bidirBFS pentru a găsi
    intersecția dintre cele două frontiere și de extractPath pentru a genera calea.

    Atenție: Pentru calea gasită în a doua parcurgere, trebuie să aveți grijă la a asocia
    corect fiecare stare cu acțiunea care a generat-o.

    Întoarce o listă de perechi (acțiune, stare), care pornește de la starea inițială
    și se încheie în starea finală.
-}

solve :: (ProblemState s a, Ord s)
      => s          -- Starea inițială de la care se pornește
      -> s          -- Starea finală la care se ajunge
      -> [(Maybe a, s)]   -- Lista perechilor
solve start end = extractPath (fst comun) ++ (map (\((Just act), st) -> (Just (fst (reverseAction (act,st))), snd (reverseAction (act,st)))) $ reverse $ tail $ extractPath (snd comun))
        where comun = bidirBFS (createStateSpace start) (createStateSpace end)
