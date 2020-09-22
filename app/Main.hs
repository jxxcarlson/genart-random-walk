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

origin_ :: V2 Double 
origin_ = V2 30 30

genPoints :: Int -> Generate [V2 Double]
genPoints n = replicateM n $ V2 <$> getRandomR  (-1, 1) <*> getRandomR (-1, 1)

genBrownianPath :: Int -> Generate [V2 Double]
genBrownianPath n = liftM2 (scanl (^+^)) (pure origin_) (genPoints n)


--- RENDERING ---

-- Render broken line path connecting the points of the argument
renderPath :: [V2 Double] -> Render ()
renderPath (V2 x y:vs) = do
  newPath
  moveTo x y
  for_ vs $ \v -> let V2 x' y' = v in lineTo x' y'
renderPath [] = pure ()

-- Render a small square of given color with upper left corners at the points of the argument
renderPath2 :: Double -> Double -> Double -> Double -> [V2 Double] -> Render ()
renderPath2 r g b a (V2 x y:vs) = do
  drawSquare r g b a x y
  for_ vs $ \v -> let V2 x' y' = v in drawSquare r g b a x' y'
renderPath2 _ _ _ _ [] = pure ()

drawSquare :: Double -> Double -> Double -> Double -> Double -> Double -> Render ()
drawSquare r g b a x y = do
  setSourceRGBA r g b a
  rectangle x y 0.4 0.4
  fill


darkBlue :: Double -> Render ()
darkBlue = hsva 243 0.50 0.1

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


renderSketch :: Generate ()
renderSketch = do
  fillScreen darkBlue 1

  cairo $ setLineWidth 0.15
  
  points1 <- genBrownianPath 5000
  points2 <- genBrownianPath 5000
  points3 <- genBrownianPath 5000
  points4 <- genBrownianPath 5000
  cairo $ do 
    renderPath2 0 0 1 0.3 points1
    renderPath2 0.52 0 1 0.3 points2
    renderPath2 0 0 1 0.3 points3
    renderPath2 0.52 0 1 0.3 points4


--- MAIN ---

main :: IO ()
main = do
  seed <- round . (*1000) <$> getPOSIXTime
  let
    stdGen = mkStdGen seed
    width = 60
    height = 60
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
