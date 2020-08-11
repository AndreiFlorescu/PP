{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Search where
import Data.Maybe
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

data Node s a = Nil | Node { state_ :: s,
                             action_ :: Maybe a,
                             parent_ :: Node s a,
                             depth_ :: Int,
                             succs_ :: [Node s a] }
    deriving (Eq, Show)



{-
    *** TODO ***
    Gettere folosite pentru accesul la câmpurile nodului
-}
nodeState :: Node s a -> s
nodeState Nil = undefined
nodeState (Node state _ _ _ _) = state

nodeParent :: Node s a -> Maybe (Node s a)
nodeParent Nil = Nothing
nodeParent (Node _ _ parent _ _) = Just parent

nodeDepth :: Node s a -> Int
nodeDepth Nil = 0
nodeDepth (Node _ _ _ depth _) = depth

nodeAction :: Node s a -> Maybe a
nodeAction Nil = Nothing
nodeAction (Node _ action _ _ _) = action

nodeChildren :: Node s a -> [Node s a]
nodeChildren Nil = []
nodeChildren (Node _ _ _ _ succs) = succs

{-
    *** TODO ***

    Generarea întregului spațiu al stărilor
    Primește starea inițială și creează nodul corespunzător acestei stări,
    având drept copii nodurile succesorilor stării curente.
-}

makeNodes :: (ProblemState s a, Eq s) => s -> a -> Node s a -> Int -> [s] -> Node s a
makeNodes s a parent depth vis = newNode
    where
        newNode         = Node s (Just a) parent depth newSuccs
        newVis          = s : vis
        newSuccs        = map (\(act, st) -> makeNodes st act newNode (depth + 1) newVis) $ filteredSuccs
        filteredSuccs   = filter (\(_, st) -> not (st `elem` newVis)) $ successors s


createStateSpace :: (ProblemState s a, Eq s) => s -> Node s a
createStateSpace s = newNode
    where
        newNode     = Node s Nothing Nil 1 newSuccs
        vis         = [s]
        newSuccs    = map (\(act, st) -> makeNodes st act newNode 2 vis) $ successors s

{-
    *** TODO ***
   
    Primește un nod inițial și întoarce un flux de perechi formate din:
    * lista nodurilor adăugate în frontieră la pasul curent
    * frontiera

-}

bfs :: (Ord s, Eq a) => Node s a -> [([Node s a], [Node s a])]
bfs node = map fst $ iterate iterFunction (([node], [node]), [])
    where 
        iterFunction ((_, curFrontier), vis) = ((newNodes, newFrontier), (curNode : vis))
            where
                curNode = head curFrontier
                newNodes = filter (\n -> not (n `elem` vis)) $ nodeChildren curNode
                newFrontier = (tail curFrontier) ++ newNodes


{-
    *** TODO ***
  
    Primește starea inițială și finală și întoarce o pereche de noduri, reprezentând
    intersecția dintre cele două frontiere.

-}

bidirBFS :: (Ord s, Eq a) => Node s a -> Node s a -> (Node s a, Node s a)
bidirBFS node1 node2 = head $ filter (\(n1, n2) -> n1 /= Nil && n2 /= Nil) solStream
    where
        bfs1 = bfs node1
        bfs2 = bfs node2
        func (curNodes1, curFrontier1) (curNodes2, curFrontier2)
            | intersect1 /= []  = head intersect1
            | intersect2 /= []  = head intersect2
            | otherwise         = (Nil, Nil)
                where
                    cmpNodes (Node st1 _ _ _ _) (Node st2 _ _ _ _) = st1 == st2
                    cmpNodes Nil _ = False
                    cmpNodes _ Nil = False
                    intersect1 = [(x, y) | x <- curNodes1, y <- curFrontier2, cmpNodes x y]
                    intersect2 = [(x, y) | y <- curNodes2, x <- curFrontier1, cmpNodes x y]
        solStream = zipWith func bfs1 (([node2], [node2]) : bfs2)



{-
    *** TODO ***

    Pornind de la un nod, reface calea către nodul inițial, urmând legăturile
    către părinți.

    Întoarce o listă de perechi (acțiune, stare), care pornește de la starea inițială
    și se încheie în starea finală.

-}

extractPath :: Node s a -> [(Maybe a, s)]
extractPath node = reverse path
    where
        parents = takeWhile (\n -> nodeDepth n > 0) $ iterate (\n -> fromJust (nodeParent n)) node
        path = map (\n -> (nodeAction n, nodeState n)) parents


{-
    *** TODO ***

    Pornind de la o stare inițială și una finală, se folosește de bidirBFS pentru a găsi
    intersecția dintre cele două frontiere și de extractPath pentru a genera calea.

    Atenție: Pentru calea gasită în a doua parcurgere, trebuie să aveți grijă la a asocia
    corect fiecare stare cu acțiunea care a generat-o.

    Întoarce o listă de perechi (acțiune, stare), care pornește de la starea inițială
    și se încheie în starea finală.
-}

solve :: (ProblemState s a, Ord s, Eq a)
      => s          -- Starea inițială de la care se pornește
      -> s          -- Starea finală la care se ajunge
      -> [(Maybe a, s)]   -- Lista perechilor
solve startState finishState = extractPath (fst intersNodes) ++ map func (reverse (tail (extractPath (snd intersNodes))))
    where 
        startNode = createStateSpace startState
        finishNode = createStateSpace finishState
        intersNodes = bidirBFS startNode finishNode
        func (act, lvl) = (Just newAction, newLvl)
            where
                reverse = reverseAction (fromJust act, lvl)
                newAction = fst reverse
                newLvl = snd reverse

