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


{-

---------------------------------------------------------

-}


data TList = Niil | Cons Int TList 
    deriving (Eq)


data Lst a b = Nl | Consa a (Lst a b) | Consb b (Lst a b)


f = (\x -> x : (f x))


{-
    Sinonim tip de date pentru reprezetarea unei perechi (Int, Int)
    care va reține coordonatele celulelor de pe tabla de joc
-}

type Position = (Int, Int)

{-
    Tip de date pentru reprezentarea celulelor tablei de joc
-}
data Cell = Cell { typeCell :: Char }
    deriving (Eq, Ord)

instance Show Cell where
    show cell = [typeCell cell]



{-
    Tip de date pentru reprezentarea nivelului curent
-}
data Level =    EmptyLevel |
                Lvl { board :: (A.Array Position Cell) }
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

instance Show Level where 
    show EmptyLevel = ""
    show level = foldl showCell "" cellList ++ "\n"
        where
            cellList = A.assocs (board level)
            showCell output (pos, cell)
                | snd pos == 0 = output ++ "\n" ++ show cell
                | otherwise    = output ++ show cell

{-
    *** TODO ***
    Primește coordonatele colțului din dreapta jos a hărții.
    Intoarce un obiect de tip Level în care tabla este populată
    cu EmptySpace. Implicit, colțul din stânga sus este (0,0)
-}

emptyLevel :: Position -> Level
emptyLevel (line, col) = Lvl emptyBoard
    where
        emptyBoard = A.array ((0, 0), (line, col)) [((i, j), newCell) | i <- [0..line], j <- [0..col]]
        newCell = Cell emptySpace


onBoard :: Position -> Level -> Bool
onBoard _ EmptyLevel = False
onBoard pos (Lvl cells)
    |   fst pos < 0 || 
        snd pos < 0 || 
        fst pos > fst boardSize || 
        snd pos > snd boardSize = False
    | otherwise                 = True
        where
            boardSize = snd (A.bounds cells)

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
addCell (_, _) EmptyLevel = EmptyLevel
addCell (typeC, pos) lvl@(Lvl cells) 
    | onBoard pos lvl = Lvl newCells
    | otherwise       = Lvl cells
        where
            newCells = cells A.// [(pos, newCell)]
            newCell = Cell typeC

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
createLevel = foldr addCell . emptyLevel


{-
    *** TODO ***

    Mișcarea unei celule în una din cele 4 direcții 
    Schimbul se poate face doar dacă celula vecină e goală (emptySpace).
    Celulele de tip start și win sunt imutabile.

    Hint: Dacă nu se poate face mutarea puteți lăsa nivelul neschimbat.
-}

moveCell :: Position -> Directions -> Level -> Level
moveCell _ _ EmptyLevel = EmptyLevel
moveCell pos dir lvl@(Lvl cells)
    | curTypeCell `elem` startCells     = lvl
    | curTypeCell `elem` winningCells   = lvl
    | nextTypeCell /= emptySpace        = lvl
    | otherwise                         = Lvl updatedCells
        where
            curTypeCell     = typeCell (cells A.! pos)
            nextTypeCell 
                | onBoard nextPos lvl = typeCell $ cells A.! nextPos
                | otherwise           = 'X'
            nextPos
                | dir == North = (fst pos - 1, snd pos)
                | dir == East  = (fst pos, snd pos + 1)
                | dir == South = (fst pos + 1, snd pos)
                | otherwise    = (fst pos, snd pos - 1)
            updatedCells = cells A.// [(pos, Cell emptySpace), (nextPos, Cell curTypeCell)]

{-
    *** HELPER ***

    Verifică dacă două celule se pot conecta.
    Atenție: Direcția indică ce vecin este a
    doua celulă pentru prima.

    ex: connection botLeft horPipe East = True (╚═)
        connection horPipe botLeft East = False (═╚)
-}
connection :: Cell -> Cell -> Directions -> Bool
connection cell1 cell2 dir
    | dir == North = (type1 `elem` northPipes && type2 `elem` southPipes)
    | dir == East  = (type1 `elem` eastPipes  && type2 `elem` westPipes)
    | dir == South = (type1 `elem` southPipes && type2 `elem` northPipes)
    | otherwise    = (type1 `elem` westPipes  && type2 `elem` eastPipes)
        where
            type1 = typeCell cell1
            type2 = typeCell cell2
            northPipes = [verPipe, botLeft, botRight, startUp, winUp]
            eastPipes  = [horPipe, botLeft, topLeft, startRight, winRight]
            southPipes = [verPipe, topLeft, topRight, startDown, winDown]
            westPipes  = [horPipe, botRight, topRight, startLeft, winLeft]

{-
    *** TODO ***

    Va returna True dacă jocul este câștigat, False dacă nu.
    Va verifica dacă celulele cu Pipe formează o cale continuă de la celula
    de tip inițial la cea de tip final.
    Este folosită în cadrul Interactive.
-}

checkPath :: (Num c) => c -> Level -> Position -> Directions -> c
checkPath _ EmptyLevel _ _ = 2147483647 
checkPath len lvl@(Lvl cells) curPos lastDir
    | curTypeCell `elem` winningCells   = len
    | curTypeCell `elem` northPipes &&
        lastDir /= South                = checkNorth
    | curTypeCell `elem` eastPipes &&
        lastDir /= West                 = checkEast
    | curTypeCell `elem` southPipes &&
        lastDir /= North                = checkSouth
    | curTypeCell `elem` westPipes &&
        lastDir /= East                 = checkWest
    | otherwise                         = 2147483647
        where
            curCell = cells A.! curPos
            curTypeCell = typeCell curCell
            northPipes = [verPipe, botLeft, botRight, startUp]
            eastPipes  = [horPipe, botLeft, topLeft, startRight]
            southPipes = [verPipe, topLeft, topRight, startDown]
            westPipes  = [horPipe, botRight, topRight, startLeft]
            checkNorth 
                | onBoard nextPos lvl && 
                    connection curCell nextCell North   = checkPath (len + 1) lvl nextPos North
                | otherwise                             = 2147483647
                    where 
                        nextPos = (fst curPos - 1, snd curPos)
                        nextCell = cells A.! nextPos
            checkEast 
                | onBoard nextPos lvl &&
                    connection curCell nextCell East    = checkPath (len + 1) lvl nextPos East
                | otherwise                             = 2147483647
                    where 
                        nextPos = (fst curPos, snd curPos + 1)
                        nextCell = cells A.! nextPos
            checkSouth 
                | onBoard nextPos lvl && 
                    connection curCell nextCell South   = checkPath (len + 1) lvl nextPos South
                | otherwise                             = 2147483647
                    where 
                        nextPos = (fst curPos + 1, snd curPos)
                        nextCell = cells A.! nextPos
            checkWest 
                | onBoard nextPos lvl && 
                    connection curCell nextCell West    = checkPath (len + 1) lvl nextPos West
                | otherwise                             = 2147483647
                    where 
                        nextPos = (fst curPos, snd curPos - 1)
                        nextCell = cells A.! nextPos
          

wonLevel :: Level -> Bool
wonLevel EmptyLevel = False
wonLevel lvl = checkPath 1 lvl startPos NoDir /= 2147483647
    where 
        cellList = A.assocs (board lvl)
        startPos = fst $ head (filter getStartPos cellList)
        getStartPos (_, cell)
            | (typeCell cell) `elem` startCells     = True
            | otherwise                             = False


heuristic :: (Num c) => Level -> c
heuristic EmptyLevel = 0
heuristic lvl = checkPath 1 lvl startPos NoDir
    where 
        cellList = A.assocs (board lvl)
        startPos = fst $ head (filter getStartPos cellList)
        getStartPos (_, cell)
            | (typeCell cell) `elem` startCells     = True
            | otherwise                             = False


canMoveCell :: ((Position, Directions), Level) -> Bool
canMoveCell ((_, _), EmptyLevel) = False
canMoveCell ((pos, dir), lvl@(Lvl cells))
    | curTypeCell `elem` startCells     = False
    | curTypeCell `elem` winningCells   = False
    | curTypeCell == emptySpace         = False
    | nextTypeCell /= emptySpace        = False
    | otherwise                         = True
        where
            curTypeCell     = typeCell (cells A.! pos)
            nextTypeCell 
                | onBoard nextPos lvl = typeCell $ cells A.! nextPos
                | otherwise           = 'X'
            nextPos
                | dir == North = (fst pos - 1, snd pos)
                | dir == East  = (fst pos, snd pos + 1)
                | dir == South = (fst pos + 1, snd pos)
                | otherwise    = (fst pos, snd pos - 1)

instance ProblemState Level (Position, Directions) where
    successors EmptyLevel = []
    successors lvl@(Lvl cells) = map updateLevel $ posibleMoves
        where
            posibleMoves = filter canMoveCell [(((i, j), dir), lvl) |   i <- [0..line], 
                                                                        j <- [0..col], 
                                                                        dir <- posibDir]
            updateLevel ((pos, dir), curLvl) = ((pos, dir), moveCell pos dir curLvl)
            boardSize = snd (A.bounds cells)
            line = fst boardSize
            col = snd boardSize
            posibDir = [North, East, South, West]

    isGoal lvl = wonLevel lvl 

    reverseAction ((pos, dir), lvl)
        | dir == North = ((nextPosN, South), moveCell nextPosN South lvl)
        | dir == East  = ((nextPosE, West), moveCell nextPosE West lvl)
        | dir == South = ((nextPosS, North), moveCell nextPosS North lvl)   
        | otherwise    = ((nextPosW, East), moveCell nextPosW East lvl)
            where
                nextPosN = (fst pos - 1, snd pos)
                nextPosE = (fst pos, snd pos + 1)
                nextPosS = (fst pos + 1, snd pos)
                nextPosW = (fst pos, snd pos - 1)