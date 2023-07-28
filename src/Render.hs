module Render where
import Instruction
import Graphics.Gloss
import Graphics.Gloss.Juicy
import Graphics.Gloss.Export.Image
import Graphics.Gloss.Export.PNG
import Graphics.Gloss.Interface.Environment
import RenderSVG
import DisplayMode

-- Render.hs: The renderer, which takes a list of Instructions and produces
-- a Gloss picture.

-- Render state: you will want to change this to keep track of the different
-- properties you need to record while you render the picture (e.g., the current
-- colour, co-ordinates, and whether the pen is up or down)
data RenderState = RState {position :: (Float,Float),
                           rotation :: Int,
                           colour :: Color,
                           penDown :: Bool}

{- Exercise 5 -}
-- Convert degrees to radians
degreesToRadians :: Float -> Float
degreesToRadians degrees = degrees * (pi/180)

newCoords :: (Float, Float) -> Float -> Float -> (Float, Float)
newCoords (x, y) dist angle = (x1,y1) where
    x1 = x + (dist * (sin (degreesToRadians angle)))
    y1 = y + (dist * (cos (degreesToRadians angle)))

{- Exercise 6 -}
newAngle :: Int -> Int -> Int
newAngle angle change = if ((angle + change) < 0) then angle + change + 360 else (angle + change) `mod` 360

-- Implement a function renderInstruction which takes a render state and an
-- instruction, and produces a new state
renderInstruction :: RenderState -> Instruction -> (Picture, RenderState)
renderInstruction _st _instr = (picture,state) where
    oldPos = position _st
    currentAngle = rotation _st
    state = case _instr of
                 IMove i -> _st {position = (newCoords oldPos (fromIntegral i) (fromIntegral currentAngle))}
                 IRotate i -> _st {rotation = (newAngle currentAngle i)}
                 IPenUp -> _st {penDown = False}
                 IPenDown -> _st {penDown = True}
                 IChangeColor c -> _st {colour = c}

    picture = case _instr of
                 IMove _ -> if (penDown state) == True then (color (colour _st) (line [oldPos, (position state)])) else blank
                 IRotate _ -> blank
                 IPenUp -> blank
                 IPenDown -> blank
                 IChangeColor _ -> blank
-- Implement the renderPicture function which renders a list of instructions
-- using renderInstruction.
-- The first argument is the turtle picture; you can then use 'formatTurtle' to
-- place the turtle given the final angle and co-ordinates.
renderPicture :: Picture -> [Instruction] -> Picture
renderPicture _turtle _instr = turtlePic where
    (finalPic,state) = renderInstructions _turtle (RState{position = (0,0),rotation = 0, colour = black,penDown = True}) _instr
    turtlePic = (formatTurtle (fromIntegral(rotation state)) (fst(position state)) (snd (position state))) finalPic


renderInstructions :: Picture -> RenderState -> [Instruction] -> (Picture, RenderState)
renderInstructions pic _st [] = (pic,_st)
renderInstructions pic _st (instruct:rest) = (finalPic,finalState) where
    (newPic,newState) = renderInstruction _st instruct
    combinedPic = pictures [pic,newPic]
    (finalPic,finalState) = renderInstructions combinedPic newState rest
{- ------------- -}

{- The remainder of the file consists of helper functions, which you should
 - not need to modify. -}

{- Translate and rotate turtle picture. Takes the final angle and co-ordinates
 - of the turtle after rendering all instructions. -}
formatTurtle :: Float -> Float -> Float -> Picture -> Picture
formatTurtle angle x y =
    let rotateTurtle = Graphics.Gloss.rotate angle in
    let translateTurtle = Graphics.Gloss.translate x y in
    (translateTurtle . rotateTurtle)

loadTurtlePic :: IO Picture
loadTurtlePic = do
    turtle <- loadJuicyPNG "turtle.png"
    case turtle of
        Just pic -> return pic
        Nothing -> ioError $ userError "Could not load turtle.png"

size :: Size
size = (900, 900)

-- Exports a picture to a PNG
export :: FilePath -> Picture -> IO ()
export path pic = do
    -- First translate the picture so it is in the centre of the exported
    -- viewport
    (w, h) <- getScreenSize
    let pic' = translate (fromIntegral $ -(div w 4)) (fromIntegral $ div h 4) pic
    exportPictureToPNG (w, h) white path pic'

-- Shows a picture
displayPic :: Picture -> IO ()
displayPic pic = do
    let window = InWindow "Turtle Graphics Assignment" size (0, 0)
    display window white pic

-- Main entry point: takes a list of instructions and a display mode, and
-- runs the rendering logic.
render :: [Instruction] -> DisplayMode -> IO ()
render instrs mode = do
    turtlePic <- loadTurtlePic
    let pic = renderPicture turtlePic instrs
    case mode of
        GlossWindow -> displayPic pic
        GlossExport path -> export path pic
        SVGExport path -> exportSVG size path pic
