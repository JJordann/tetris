import Data.Sequence (update, fromList)

data Colour = Blue | Yellow | Red | Green | Pink
    deriving (Show)

data Piece = Piece [(Int, Int)] Colour 
    deriving (Show)

--            [all pieces] ActivePiece
data State = State [Piece] Piece 
    deriving (Show)

data PieceType = Square | Line | S | Z | T

-- boardHeight = 20
-- boardWidth  = 10

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
emptyBoard x y = 
    replicate x $ concat $ replicate y "_"


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
    where board     = (emptyBoard 10 10)
          allPieces = active:pieces

-- todo: preveri če je na tleh
canDrop :: State -> Bool
canDrop (State pieces (Piece cs _)) = 
    let ps = foldl (\flat (Piece cells _) -> flat ++ cells) [] pieces 
        cs_moved = map (\(x, y) -> (x, y + 1)) cs
    in all (\p -> not $ elem p ps) cs_moved


dropActive :: State -> State
dropActive s@(State pieces active@(Piece cells colour)) = 
    if canDrop s then
        let droppedCells = map (\(x, y) -> (x, y + 1)) cells
        in State pieces (Piece droppedCells colour)
    else
       State (active:pieces) (newPiece Square)
    


moveRight :: State -> State
moveRight (State pieces (Piece cs colour)) = 
    let cs_moved = map (\(x, y) -> (x + 1, y)) cs
    in (State pieces (Piece cs_moved colour)) where


moveLeft :: State -> State
moveLeft (State pieces (Piece cs colour)) = 
    let cs_moved = map (\(x, y) -> (x - 1, y)) cs
    in (State pieces (Piece cs_moved colour)) where


rotateLeft :: State -> State
rotateLeft = id

rotateRight :: State -> State
rotateRight = id

command :: Char -> State -> State
command c 
    | c == 'h' = moveLeft
    | c == 'j' = dropActive
    | c == 'k' = rotateLeft
    | c == 'l' = moveRight
    | c == 'i' = rotateRight


gameLoop :: State -> IO ()
gameLoop s = do
    let board = showState s
    showBoard board
    cmd <- getChar
    putStrLn ""
    gameLoop (command cmd s)


initialState = State [] p0 where
    p0 = newPiece Square
    

start = gameLoop initialState


main = do
    let loop s = do
        let board = showState s
        showBoard board
        cmd <- getChar
        putStrLn ""
        if cmd == 'q'
            then return ()
            else loop (command cmd s)

    loop initialState

