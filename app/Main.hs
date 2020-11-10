{-# LANGUAGE RecordWildCards  #-}
{-# LANGUAGE TypeApplications #-}

{-

The code below comes from a blog post by Benjamin Kovach

  https://www.kovach.me/posts/2018-03-07-generating-art.html

In the main `do` block below, use `renderBlankSketch` instead of `renderSketch` to
render a "blank drawing."  You can print the blank  drawing and let your kids color it in.

-}

module Main where

import           Control.Arrow
import           Control.Concurrent
import Control.Monad.Random( uniform, weighted, evalRandIO, runRandT, replicateM,  void, mkStdGen,
      MonadRandom(getRandomR), RandT,  StdGen, MonadTrans(lift) )
import           Control.Monad.Reader
import           Data.Colour.RGBSpace
import           Data.Colour.RGBSpace.HSV
import           Data.Foldable            (for_)
import           Data.List                (nub)
import           Data.Semigroup           ((<>))
import           Data.Time.Clock.POSIX
import Graphics.Rendering.Cairo ( closePath, createImageSurface, fill, lineTo, moveTo, newPath,
      rectangle, renderWith, scale, setLineWidth, setSourceRGBA, stroke, surfaceWriteToPNG,
      Render, Format(FormatARGB32) )
import           Linear.V2
import           Linear.Vector
import qualified Numeric.Noise.Perlin     as P
import           Text.Printf

data World = World
  { worldWidth  :: Int
  , worldHeight :: Int
  , worldSeed   :: Int
  , worldScale  :: Double
  }


--- GENERATE A RANDOM WALK ---

type Generate a = RandT StdGen (ReaderT World Render) a


worldWidth_ :: Double
worldWidth_ = 60

worldHeight_ :: Double
worldHeight_ = 60

originX :: Double
originX = worldWidth_/2

originY :: Double
originY = worldHeight_/2

origin_ :: V2 Double 
origin_ = V2 originX originY

genPoints :: Int -> Generate [V2 Double]
genPoints n = replicateM n $ V2 <$> getRandomR  (-1, 1) <*> getRandomR (-1, 1)

genBrownianPath :: Double -> Double -> Int ->  Generate [V2 Double]
genBrownianPath x y n = liftM2 (scanl (^+^)) (pure (V2 (x + originX) (y + originY))) (genPoints n)

--- RENDERING ---

-- Render broken line path connecting the points of the argument
renderLinePath :: [V2 Double] -> Render ()
renderLinePath (V2 x y:vs) = do
  newPath
  moveTo x y
  for_ vs $ \v -> let V2 x' y' = v in lineTo x' y'
renderLinePath [] = pure ()

-- Render a small square of given color with upper left corners at the points of the argument
renderPathWithSquares :: Double -> Double -> Double -> Double -> [V2 Double] -> Render ()
renderPathWithSquares r g b a (V2 x y:vs) = do
  drawSquare r g b a x y
  for_ vs $ \v -> let V2 x' y' = v in drawSquare r g b a x' y'
renderPathWithSquares _ _ _ _ [] = pure ()

drawSquare :: Double -> Double -> Double -> Double -> Double -> Double -> Render ()
drawSquare r g b a x y = do
  setSourceRGBA r g b a
  rectangle x y d d
  fill

-- COLORS

darkBlue :: Double -> Render ()
darkBlue = hsva 243 0.50 0.1

white :: Double -> Render ()
white = hsva 243 0.00 1.0

lightBlue :: Double -> Render ()
lightBlue = hsva 243 0.20 1.0

-- | Lift a Cairo action into a Generate action
cairo :: Render a -> Generate a
cairo = lift . lift

getSize :: Num a => Generate (a, a)
getSize = do
  (w, h) <- asks (worldWidth &&& worldHeight)
  pure (fromIntegral w, fromIntegral h)

fillScreen :: (Double -> Render a) -> Double -> Generate ()
fillScreen color opacity = do
  (w, h) <- getSize @Double
  cairo $ do
    rectangle 0 0 w h
    color opacity *> fill

hsva :: Double -> Double -> Double -> Double -> Render ()
hsva h s v = setSourceRGBA channelRed channelGreen channelBlue
 where RGB{..} = hsv h s v


-- PARAMETERS

d = 8.0
a = 0.01
n = 5000

displacement = 0.33

-- PALETTE

data RGBA = RGBA {red:: Double, green:: Double, blue:: Double, alpha:: Double}

renderWithColor :: RGBA -> [V2 Double] -> Render ()
renderWithColor color points =
  renderPathWithSquares (red color) (green color) (blue color) (alpha color) points


paleBlue :: Double -> RGBA
paleBlue a = RGBA {red = 0.4, green = 0.4, blue = 1.0, alpha = a}

intenseBlue :: Double -> RGBA
intenseBlue a = RGBA {red = 0, green = 0.1, blue = 1.0, alpha = a}

paleCyan :: Double -> RGBA
paleCyan a = RGBA {red = 0.52, green = 1.0, blue = 1.0, alpha = a}

paleYellow :: Double -> RGBA
paleYellow a = RGBA {red = 0.52, green = 0.5, blue = 0.5, alpha = a}

magenta :: Double -> RGBA
magenta a = RGBA {red = 0.52, green = 0, blue = 1, alpha = a}



-- PATHS

data Path = Path { pathX:: Double, pathY:: Double, pathLength:: Int}

translate :: Double -> Double -> Path -> Path 
translate dx dy Path {pathX = x, pathY = y, pathLength = n} =
   Path { pathX = x  + dx, pathY = y + dy, pathLength = n}
   

genPath_ :: Path -> Generate [V2 Double]
genPath_ p = genBrownianPath (pathX p) (pathY p) (pathLength p)


-- COMPASS POINTS


centerPath :: Int -> Path
centerPath n = Path { pathX = 0, pathY = 0, pathLength = n}

northPath :: Double -> Int ->  Path 
northPath delta n = Path { pathX = 0, pathY = -delta*worldWidth_, pathLength = n}

northWestPath :: Double -> Int -> Path 
northWestPath delta n = Path { pathX =  -delta*worldWidth_, pathY = -delta*worldHeight_, pathLength = n}

westPath :: Double -> Int -> Path 
westPath delta n = Path { pathX =  -delta*worldWidth_, pathY = 0, pathLength = n}

southWestPath :: Double -> Int -> Path 
southWestPath delta n = Path { pathX =  -delta*worldWidth_, pathY = delta*worldHeight_, pathLength = n}

southPath :: Double -> Int -> Path 
southPath delta n = Path { pathX =  0, pathY = delta*worldHeight_, pathLength = n}

southEastPath :: Double -> Int -> Path 
southEastPath delta n = Path { pathX =  delta*worldWidth_, pathY = delta*worldHeight_, pathLength = n}

eastPath :: Double -> Int -> Path 
eastPath delta n = Path { pathX =  delta*worldWidth_, pathY = 0, pathLength = n} 

northEastPath :: Double -> Int -> Path 
northEastPath delta n = Path { pathX =  delta*worldWidth_, pathY = -delta*worldHeight_, pathLength = n}



renderSketch :: Generate ()
renderSketch = do
  fillScreen lightBlue 1

  cairo $ setLineWidth 0.15
  
  points1 <- genPath_ $ centerPath  n
  points2 <- genPath_ $ (translate 1 0 $ centerPath n)
  points3 <- genPath_ $ (northWestPath 0.33 n)
  points4 <- genPath_ $ (southWestPath 0.33 n)
  points5 <- genPath_ $ (southEastPath 0.33 n)
  points6 <- genPath_ $ (northEastPath 0.33 n)
  cairo $ do 
    renderWithColor (intenseBlue a) points1
    renderWithColor (paleCyan a) points2
    renderWithColor (paleYellow a) points2
    renderWithColor (magenta a) points3
    renderWithColor (magenta a) points4
    renderWithColor (paleCyan a) points4
    renderWithColor (paleYellow a) points5
    renderWithColor (magenta a) points6



--- MAIN ---

main :: IO ()
main = do
  seed <- round . (*1000) <$> getPOSIXTime
  let
    stdGen = mkStdGen seed
    width = round worldWidth_
    height = round worldHeight_
    scaleAmount = 20

    scaledWidth = round $ fromIntegral width * scaleAmount
    scaledHeight = round $ fromIntegral height * scaleAmount

  surface <- createImageSurface FormatARGB32 scaledWidth scaledHeight
  -- The "world" thinks the width and height are the initial values, not scaled.
  let world = World width height seed scaleAmount

  void
    . renderWith surface
    . flip runReaderT world
    . flip runRandT stdGen
    $ do
      cairo $ scale scaleAmount scaleAmount
      renderSketch

  putStrLn "Generating art..."
  surfaceWriteToPNG surface
    $ "images/example_sketch/"
    <> show seed <> "-" <> show (round scaleAmount :: Int) <> ".png"
  surfaceWriteToPNG surface "images/example_sketch/latest.png"
