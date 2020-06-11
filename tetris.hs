import Data.Sequence (update, fromList)

data Colour = Blue | Yellow | Red | Green | Pink
    deriving (Show)

data Piece = Piece [(Int, Int)] Colour 
    deriving (Show)

--            [all pieces] ActivePiece
data State = State [Piece] Piece 
    deriving (Show)

data PieceType = Square | Line | S | Z | T

boardW = 10
boardH = 10

newPiece :: PieceType -> Piece
newPiece Square = Piece [(0, 0), (0, 1), (1, 0), (1, 1)] Yellow
newPiece Line   = Piece [(0, 0), (0, 1), (0, 2), (0, 3)] Blue
newPiece S      = Piece [(1, 0), (1, 1), (0, 1), (0, 2)] Green
newPiece Z      = Piece [(0, 0), (0, 1), (1, 1), (1, 2)] Red
newPiece T      = Piece [(0, 0), (0, 1), (0, 2), (1, 1)] Pink


showColour :: Colour -> Char
showColour c = case c of
    Yellow  ->  '1'
    Blue    ->  '2'
    Green   ->  '3'
    Red     ->  '4'
    Pink    ->  '5'


emptyBoard :: Int -> Int -> [String]
emptyBoard w h = 
    replicate h $ concat $ replicate w "-"


showBoard :: [String] -> IO ()
showBoard = putStrLn . foldl1 (\acc a -> acc ++ "\n" ++ a)


updateCell :: [String] -> (Int, Int) -> Char -> [String]
updateCell board (x, y) new = 
    let (rowsBefore, row:rowsAfter) = splitAt y board
        (before, _:after) = splitAt x row
        updateRow = concat [before, new:after]
    in rowsBefore ++ updateRow:rowsAfter 


pieceToBoard :: Piece -> [String] -> [String]
pieceToBoard (Piece cells colour) b = 
    foldl (\board cell -> updateCell board cell c) b cells
        where c = showColour colour


showState :: State -> [String]
showState (State pieces active) = 
    foldl (\b p -> pieceToBoard p b) board allPieces 
    where board     = (emptyBoard boardW boardH)
          allPieces = active:pieces


canDrop :: State -> Bool
canDrop (State pieces (Piece cs _)) = 
    let ps = foldl (\flat (Piece cells _) -> flat ++ cells) [] pieces 
        ps' = ps ++ [(a, boardH) | a <- [0..boardW]]
        cs_moved = map (\(x, y) -> (x, y + 1)) cs
    in all (\p -> not $ elem p ps') cs_moved

--canDrop :: State -> Bool
--canDrop (State pieces (Piece cs _)) = 
--    if any (\(x, y) -> y == boardH - 1) cs 
--        then False
--        else 
--            let ps = foldl (\flat (Piece cells _) -> flat ++ cells) [] pieces 
--                cs_moved = map (\(x, y) -> (x, y + 1)) cs
--            in all (\p -> not $ elem p ps) cs_moved


dropActive :: State -> State
dropActive s@(State pieces active@(Piece cells colour)) = 
    if canDrop s then
        let droppedCells = map (\(x, y) -> (x, y + 1)) cells
        in State pieces (Piece droppedCells colour)
    else
       clearAll $ State (active:pieces) (newPiece Square)
    

--       {-1, 1}
canMove :: Int -> State -> Bool
canMove dir (State pieces (Piece cs _)) = 
    let ps = foldl (\flat (Piece cells _) -> flat ++ cells) [] pieces 
        leftBorder  = [(-1, a) | a <- [0..boardH]]
        rightBorder = [(boardW, a) | a <- [0..boardH]]
        ps' = ps ++ leftBorder ++ rightBorder
        cs_moved = map (\(x, y) -> (x + dir, y)) cs
    in all (\p -> not $ elem p ps') cs_moved


moveRight :: State -> State
moveRight s@(State pieces (Piece cs colour)) = 
    if canMove 1 s then
        let cs_moved = map (\(x, y) -> (x + 1, y)) cs
        in (State pieces (Piece cs_moved colour))
    else
        s


moveLeft :: State -> State
moveLeft s@(State pieces (Piece cs colour)) = 
    if canMove (-1) s then
        let cs_moved = map (\(x, y) -> (x - 1, y)) cs
        in (State pieces (Piece cs_moved colour))
    else
        s


rotateLeft :: State -> State
rotateLeft = id


rotateRight :: State -> State
rotateRight = id


command :: Char -> State -> State
command c 
    | c == 'h' = moveLeft
    | c == 'j' = dropActive
    | c == 'k' = rotate 1 False
    | c == 'l' = moveRight
    | c == 'i' = rotate 1 True



clearRow :: Int -> State -> State
clearRow nrow s@(State pieces active) = 
    let row        = [(xs, nrow) | xs <- [0 .. boardW - 1]]
        cells_flat = foldl (\flat (Piece cs _) -> flat ++ cs) [] pieces
        full       = all (\c -> elem c cells_flat) row
    in if full
        then 
            let f = (\(Piece cells colour) -> 
                    (Piece (filter (\c -> not $ elem c row) cells) colour))
                pieces' = map f pieces
            in (collapse (State pieces' active) nrow)
        else 
            s


clearAll :: State -> State
clearAll s = 
    foldl (\state nrow -> clearRow nrow state) s [0..boardH]


collapse :: State -> Int -> State
collapse s@(State pieces active) row =
    let drop' = (\(x, y) -> if y < row then (x, y + 1) else (x, y))
        f = (\(Piece cs colour) -> (Piece (map drop' cs) colour))
        collapsed = map f pieces
     in (State collapsed active) 


pointOfRotation :: Piece -> Int
pointOfRotation (Piece _ colour) =
    case colour of
      Yellow -> 0
      Blue -> 1
      Pink -> 1
      Green -> 1
      Red -> 1
        

rotate :: Int -> Bool -> State -> State
rotate pivot clockwise s@(State pieces active@(Piece cs colour)) =
    let rot = if clockwise 
                 then (\(x, y) -> (-y, x))
                 else (\(x, y) -> (y, -x))
        (px, py) = cs !! pivot
        relativeVec = map (\(x, y) -> (x - px, y - py)) cs
        rotated = map rot relativeVec
        moved = map (\(x, y) -> (x + px, y + py)) rotated
     in (State pieces (Piece moved colour))


--randomPiece :: Int -> (Piece, Int)
--randomPiece seed = 
--    let rnd = 


initialState = State [] p0 where
    p0 = newPiece S
    
main = do
    let loop s = do
        let board = showState s
        showBoard board
        cmd <- getChar
        putStrLn ""
        print s
        if cmd == 'q'
            then return ()
            else loop (command cmd s)

    loop initialState

