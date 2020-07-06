import Data.Sequence (update, fromList)
import Data.Char (ord)
import Graphics.Gloss.Interface.Pure.Game
import Data.List


data Piece = Piece [(Int, Int)] Color 
    deriving (Show)


data State = State {
    pieces :: [Piece],
    active :: Piece,
    score :: Int,
    recentlyMoved :: Bool,
    pieceQueue :: [Piece],
    canHold :: Bool
   } deriving (Show)


data PieceType = Square | I | S | Z | T | L1 | L2

boardW :: Int
boardW = 10

boardH :: Int
boardH = 20

cellSize :: Int
cellSize = 30


-- Pieces always rotate around the first cell
newPiece :: PieceType -> Piece
newPiece Square = Piece [(0, 1), (0, 0), (1, 0), (1, 1)] yellow
newPiece I      = Piece [(0, 1), (0, 0), (0, 2), (0, 3)] blue
newPiece S      = Piece [(1, 1), (1, 0), (0, 1), (0, 2)] green
newPiece Z      = Piece [(0, 1), (0, 0), (1, 1), (1, 2)] red
newPiece T      = Piece [(0, 1), (0, 0), (0, 2), (1, 1)] magenta
newPiece L1     = Piece [(0, 1), (0, 0), (0, 2), (1, 2)] orange
newPiece L2     = Piece [(0, 1), (0, 0), (0, 2), (1, 0)] violet


-- projections for Piece
pCells :: Piece -> [(Int, Int)]
pCells p = cells where
    (Piece cells _) = p

pColor :: Piece -> Color
pColor p = col where
    (Piece _ col) = p



canDrop :: State -> Bool
canDrop s = 
    let ps = foldl (\flat (Piece cells _) -> flat ++ cells) [] (pieces s)
        ps' = ps ++ [(a, boardH) | a <- [0..boardW]]
        cs_moved = map (\(x, y) -> (x, y + 1)) (pCells $ active s)
    in all (\p -> not $ elem p ps') cs_moved


dropActive :: State -> State
dropActive s = 
    if canDrop s then
         let droppedCells = map (\(x, y) -> (x, y + 1)) (pCells $ active s)
             colour = pColor $ active s
          in s {active = (Piece droppedCells colour) }
    else if (recentlyMoved s) == False 
        then
           let seed = ((fst $ (pCells $ active s) !! 1) + length (pieces s))
               (p, _) = randomPiece seed
               newActive = (pieceQueue s !! 0)
               newQueue = (drop 1 $ pieceQueue s) ++ [p]
               newState = s { pieces = (active s):(pieces s),
                              active = newActive,
                              pieceQueue = newQueue,
                              canHold = True}
            in clearAll newState
        else
            s { recentlyMoved = False }

    

----       {-1, 1}
canMove :: Int -> State -> Bool
canMove dir s = 
    let ps = foldl (\flat (Piece cells _) -> flat ++ cells) [] (pieces s)
        leftBorder  = [(-1, a) | a <- [0..boardH]]
        rightBorder = [(boardW, a) | a <- [0..boardH]]
        ps' = ps ++ leftBorder ++ rightBorder
        cs_moved = map (\(x, y) -> (x + dir, y)) (pCells $ active s)
    in all (\p -> not $ elem p ps') cs_moved



moveRight :: State -> State
moveRight s = 
    if canMove 1 s then
        let cs = pCells $ active s
            cs_moved = map (\(x, y) -> (x + 1, y)) cs
            piece_moved = Piece cs_moved (pColor $ active s)
         in s { active = piece_moved , recentlyMoved = True}
    else
        s


moveLeft :: State -> State
moveLeft s = 
    if canMove (-1) s then
        let cs = pCells $ active s
            cs_moved = map (\(x, y) -> (x - 1, y)) cs
            piece_moved = Piece cs_moved (pColor $ active s)
         in s { active = piece_moved , recentlyMoved = True}
    else
        s


clearRow :: Int -> State -> State
clearRow nrow s = 
    let row        = [(xs, nrow) | xs <- [0 .. boardW - 1]]
        cells_flat = foldl (\flat (Piece cs _) -> flat ++ cs) [] (pieces s)
        full       = all (\c -> elem c cells_flat) row
    in if full then 
            let f = (\(Piece cells colour) -> 
                    (Piece (filter (\c -> not $ elem c row) cells) colour))
                pieces' = map f (pieces s)
                oldScore = score s
                newState = s {pieces = pieces' , score = oldScore + 1}
            in (collapse newState nrow)
        else 
            s


clearAll :: State -> State
clearAll s = 
    let oldScore = score s
        newState = foldl (\state nrow -> clearRow nrow state) s [0..boardH]
        scoreGain = ((score newState) - oldScore) ^ 2
        newScore = oldScore + scoreGain
     in newState { score = newScore } 


collapse :: State -> Int -> State
collapse s row =
    let drop' = (\(x, y) -> if y < row then (x, y + 1) else (x, y))
        f = (\(Piece cs colour) -> (Piece (map drop' cs) colour))
        collapsed = map f (pieces s)
     in s { pieces = collapsed} 



flatten :: State -> [(Int, Int)]
flatten s = 
    let activeCells = pCells $ active s
     in foldl (\acc (Piece c _) -> acc ++ c) activeCells (pieces s)



inBounds :: (Int, Int) -> Bool
inBounds (x, y) = 
    x >= 0 && x < boardW && y >= 0 && y < boardH


canRotate :: State -> Bool -> Bool
canRotate s dir =
        let rotated = rotatePiece 0 dir s 
            flat = flatten rotated
            allInBounds = all inBounds flat
            notOverlapping = length flat == length (nub flat)
         in allInBounds && notOverlapping


rotatePiece :: Int -> Bool -> State -> State
rotatePiece pivot clockwise s =
    let rot = if clockwise 
                 then (\(x, y) -> (-y, x))
                 else (\(x, y) -> (y, -x))
        cs = pCells $ active s
        colour = pColor $ active s
        (px, py) = cs !! pivot
        relativeVec = map (\(x, y) -> (x - px, y - py)) cs
        rotated = map rot relativeVec
        moved = map (\(x, y) -> (x + px, y + py)) rotated
     in s { active = (Piece moved colour) , recentlyMoved = True } 



safeRotate :: Bool -> State -> State
safeRotate dir s = 
    let rot = rotatePiece 0 dir
     in if canRotate s dir
            then rot s
            else if canRotate (moveLeft s) dir
                     then rot (moveLeft s)
                     else if canRotate (moveRight s) dir
                              then rot (moveRight s)
                              else s
       

holdPiece :: State -> State
holdPiece s = 
    if canHold s then
       let seed = ((fst $ (pCells $ active s) !! 1) + length (pieces s))
           (p, _) = randomPiece seed
           newActive = (pieceQueue s !! 0)
           newQueue = (drop 1 $ pieceQueue s) ++ [p]
        in s { active = newActive,
               pieceQueue = newQueue,
               canHold = False }
    else 
        s
        



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
    | otherwise = s {recentlyMoved = False}


ghostPiece :: State -> Piece
ghostPiece s = 
    let dropped = active $ hardDrop s
     in (Piece (pCells dropped) (light black))


gameOver :: State -> Bool
gameOver s = 
    let flat = flatten s
     in length flat /= length (nub flat)


restartGame :: State -> State
restartGame s = 
    if gameOver s 
       then initialState
       else s
        
    

handleKeys :: Event -> State -> State
handleKeys (EventKey (Char c) Down _ _)
  | c == 'h' = moveLeft
  | c == 'j' = dropActive
  | c == 'k' = safeRotate False
  | c == 'l' = moveRight
  | c == 'i' = safeRotate True
  | c == 'm' = hardDrop
  | c == 'f' = holdPiece
  | c == 'r' = restartGame

handleKeys _ = id


pieceToPicture :: Piece -> [Picture]
pieceToPicture (Piece cells colour) =
    let scale = (\x -> fromIntegral $ x * cellSize)
        s = fromIntegral cellSize
        fill = (\(x, y) -> translate (scale x) (scale y) $ color colour $ rectangleSolid s s)
        c = color (light $ light black)
        outline = (\(x, y) -> translate (scale x) (scale y) $ c $ rectangleWire s s)
     in (map fill cells) ++ (map outline cells)


stateToPicture :: State -> [Picture]
stateToPicture s = 
    let ghost = ghostPiece s
        pieces' = ghost:(active s):(pieces s)
     in foldl (\flat piece -> flat ++ (pieceToPicture piece)) [] pieces'



window :: Display
window = InWindow "Tedris" (boardW * cellSize + uiWidth, boardH * cellSize) (10, 10)
            where uiWidth = round (fromIntegral cellSize * (fromIntegral boardW) * 0.5)


drawUI :: State -> Picture
drawUI s = 
    let w = fromIntegral cellSize * (fromIntegral boardW / 4.0)
        halfH = fromIntegral $ boardH * cellSize 
    in pictures $ map (translate w 0) [
        translate 10 (halfH / 2 - 40) $ scale 0.3 0.3 $ color white $ Text ("Score:"),
        translate 10 (halfH / 2 - 80) $ scale 0.3 0.3 $ color white $ Text (show $ score s),
        color (light red) $ Line [(0, halfH), (0, -halfH)]
    ]


drawPieceQueue :: State -> Picture
drawPieceQueue s = 
    let dx = fromIntegral $ cellSize 
        dy = fromIntegral $ cellSize * 5
        queue = pieceQueue s
        p1 = translate dx (dy)  $ pictures . pieceToPicture $ queue !! 0
        p2 = translate dx    0  $ pictures . pieceToPicture $ queue !! 1
        p3 = translate dx (-dy) $ pictures . pieceToPicture $ queue !! 2
     in translate 0 (-100) $ pictures [p1, p2, p3]


render :: State -> Picture
render s = 
    if gameOver s == False then
        let pic = pictures $ stateToPicture s
            scaled = scale 1.0 (-1.0) pic
            uiWidth = fromIntegral (boardW * cellSize) * 0.5
            halfWidth = (-(fromIntegral (boardW * cellSize) / 2.0 + uiWidth / 2.0))
            dx = halfWidth + fromIntegral cellSize / 2
            dy = ((fromIntegral (boardH * cellSize) / 2.0))  - fromIntegral cellSize / 2
            shifted = translate dx dy scaled
         in pictures $ [shifted, drawUI s, drawPieceQueue s]
    else 
        menuScreen s


initialState = State {
    pieces = [],
    active = fst $ randomPiece 0,
    score = 0,
    recentlyMoved = False,
    pieceQueue = map (\i -> fst $ randomPiece i) [1..3],
    canHold = True
}
    
background :: Color
background = black


menuScreen :: State -> Picture
menuScreen s = 
    pictures $ map (color white . scale 0.3 0.3 . translate (-400) (300)) [
        Text "Game Over",
        translate 0 (-200) $ Text ("Score: " ++ (show $ score s)),
        color (light (light red)) $ translate 0 (-500) . scale 0.5 0.5 $ Text "[r]estart | [q]uit"
     ] 


updateState :: Float -> State -> State
updateState _ = dropActive

fps = 3

main :: IO ()
main = play 
        window 
        background 
        fps 
        initialState 
        render 
        handleKeys 
        updateState



