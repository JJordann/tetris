import Data.Sequence (update, fromList)
import Data.Char (ord)
import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game


data Piece = Piece [(Int, Int)] Color 
    deriving (Show)

--            [all pieces] ActivePiece
data State = State [Piece] Piece 
    deriving (Show)

data PieceType = Square | I | S | Z | T | L1 | L2

boardW = 10
boardH = 20
cellSize = 20

newPiece :: PieceType -> Piece
newPiece Square = Piece [(0, 0), (0, 1), (1, 0), (1, 1)] yellow
newPiece I      = Piece [(0, 0), (0, 1), (0, 2), (0, 3)] blue
newPiece S      = Piece [(1, 0), (1, 1), (0, 1), (0, 2)] green
newPiece Z      = Piece [(0, 0), (0, 1), (1, 1), (1, 2)] red
newPiece T      = Piece [(0, 0), (0, 1), (0, 2), (1, 1)] magenta
newPiece L1     = Piece [(0, 0), (0, 1), (0, 2), (1, 2)] orange
newPiece L2     = Piece [(0, 0), (0, 1), (0, 2), (1, 0)] violet


--showColour :: Color -> Char
--showColour c = case c of
--    yellow  ->  '1'
--    blue    ->  '2'
--    green   ->  '3'
--    red     ->  '4'
--    magenta ->  '5'
--    orange  ->  '6'
--    violet  ->  '7'


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


--pieceToBoard :: Piece -> [String] -> [String]
--pieceToBoard (Piece cells colour) b = 
--    foldl (\board cell -> updateCell board cell c) b cells
--        where c = showColour colour


--showState :: State -> [String]
--showState (State pieces active) = 
--    foldl (\b p -> pieceToBoard p b) board allPieces 
--    where board     = (emptyBoard boardW boardH)
--          allPieces = active:pieces


canDrop :: State -> Bool
canDrop (State pieces (Piece cs _)) = 
    let ps = foldl (\flat (Piece cells _) -> flat ++ cells) [] pieces 
        ps' = ps ++ [(a, boardH) | a <- [0..boardW]]
        cs_moved = map (\(x, y) -> (x, y + 1)) cs
    in all (\p -> not $ elem p ps') cs_moved


dropActive :: State -> State
dropActive s@(State pieces active@(Piece cells colour)) = 
    if canDrop s then
        let droppedCells = map (\(x, y) -> (x, y + 1)) cells
        in State pieces (Piece droppedCells colour)
    else
       let seed = ((fst $ cells !! 1) + length pieces)
           (p, _) = randomPiece seed
        in clearAll $ State (active:pieces) p
    

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


command :: Char -> State -> State
command c 
    | c == 'h' = moveLeft
    | c == 'j' = dropActive 
    | c == 'k' = rotatePiece 1 True
    | c == 'l' = moveRight
    | c == 'i' = rotatePiece 1 False



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




--pointOfRotation :: Piece -> Int
--pointOfRotation (Piece _ colour) =
--    case colour of
--         yellow  ->  0
--         blue    ->  1
--         magenta ->  1
--         green   ->  1
--         red     ->  1
--         orange  ->  1
--         violet  ->  1
        

rotatePiece :: Int -> Bool -> State -> State
rotatePiece pivot clockwise s@(State pieces active@(Piece cs colour)) =
    let rot = if clockwise 
                 then (\(x, y) -> (-y, x))
                 else (\(x, y) -> (y, -x))
        (px, py) = cs !! pivot
        relativeVec = map (\(x, y) -> (x - px, y - py)) cs
        rotated = map rot relativeVec
        moved = map (\(x, y) -> (x + px, y + py)) rotated
     in (State pieces (Piece moved colour))


-- todo
randomPiece :: Int -> (Piece, Int)
randomPiece seed = 
    let rnd = mod seed 7
        t = case rnd of
              0 -> Square
              1 -> I
              2 -> S
              3 -> Z
              4 -> T
              5 -> L1
              6 -> L2
        (Piece cells colour) = newPiece t
        f = (\(x, y) -> (x + (boardW `div` 2 - 1), y))
        shifted = map f cells
        piece = (Piece shifted colour)
    in (piece, (seed + 1))
    

hardDrop :: State -> State
hardDrop s 
    | canDrop s == True = hardDrop $ dropActive s
    | otherwise = s


ghostPiece :: State -> Piece
ghostPiece s@(State pieces active) = 
    let (State _ (Piece cells colour)) = hardDrop s
     in (Piece cells (light black))



pieceToPicture :: Piece -> [Picture]
pieceToPicture (Piece cells colour) =
    let scale = (\x -> fromIntegral $ x * cellSize)
        s = fromIntegral cellSize
        fill = (\(x, y) -> translate (scale x) (scale y) $ color colour $ rectangleSolid s s)
        c = color (light $ light black)
        outline = (\(x, y) -> translate (scale x) (scale y) $ c $ rectangleWire s s)
     in (map fill cells) ++ (map outline cells)

stateToPicture :: State -> [Picture]
stateToPicture s@(State pieces active) = 
    let ghost = ghostPiece s
        pieces' = ghost:active:pieces
     in foldl (\flat piece -> flat ++ (pieceToPicture piece)) [] pieces'


    
window :: Display
window = InWindow "Tedris" (boardW * cellSize, boardH * cellSize) (10, 10)

background :: Color
background = black


drawing :: Picture
drawing = pictures . stateToPicture $ initialState


render :: State -> Picture
render s = 
    let pic = pictures $ stateToPicture s
        scaled = scale 1.0 (-1.0) pic
        dx = (-(fromIntegral (boardW * cellSize) / 2.0)) + fromIntegral cellSize / 2
        dy = ((fromIntegral (boardH * cellSize) / 2.0))  - fromIntegral cellSize / 2
        shifted = translate dx dy scaled
     in shifted



handleKeys :: Event -> State -> State
handleKeys (EventKey (Char c) Down _ _)
  | c == 'h' = moveLeft
  | c == 'j' = dropActive
  | c == 'k' = rotatePiece 1 False
  | c == 'l' = moveRight
  | c == 'i' = rotatePiece 1 True
  | c == 'm' = hardDrop

handleKeys _ = id


initialState = State [] p0 where
    (p0, _) = randomPiece 0


updateState :: Float -> State -> State
updateState _ = dropActive

fps = 4

main :: IO ()
main = play 
        window 
        background 
        fps 
        initialState 
        render 
        handleKeys 
        updateState



