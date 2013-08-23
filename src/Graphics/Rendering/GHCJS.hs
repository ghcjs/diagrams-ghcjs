{-# LANGUAGE OverloadedStrings #-}
module Graphics.Rendering.GHCJS
  ( Render(..)
  , Context(..)
  , doRender

  , newPath
  , moveTo
  , relLineTo
  , relCurveTo
  , arc
  , clip
  , closePath
  , stroke
  , fill
  , setFill
  , getFill
  , fillRule
  , transform
  , save
  , restore
  , translate
  , scale
  , rotate
  , strokeColor
  , dashing
  , fillColor
  , lineWidth
  , lineCap
  , lineJoin
  , globalAlpha
  , withStyle
  ) where

import           Control.Monad.Reader
import           Control.Monad.State
import           Data.Colour
import           Data.Colour.SRGB.Linear
import           Data.NumInstances       ()
import           Data.Maybe
import           Diagrams.Attributes     (Color (..), Dashing (..),
                                          LineCap (..), LineJoin (..),
                                          colorToRGBA)
import           Diagrams.TwoD.Path      (FillRule(..))    

import           JavaScript.Canvas       (Context)
import qualified JavaScript.Canvas       as C

import Debug.Trace

type Render = StateT ((Double,Double), Maybe FillRule) (ReaderT Context IO)

doRender :: Context -> Render a -> IO a
doRender c r = runReaderT (evalStateT r ((0,0), Nothing)) c

ctx f = lift ask >>= liftIO . f

move :: (Double,Double) -> Render ()
move p = do
    (_, fr) <- get
    put (p, fr)

at :: Render (Double,Double)
at = fmap fst get

getFill :: Render (Maybe FillRule)
getFill = fmap snd get

setFill :: FillRule -> Render ()
setFill fr = do
    (p, _) <- get
    put (p, Just fr)

newPath :: Render ()
newPath = ctx C.beginPath

closePath :: Render ()
closePath = ctx C.closePath

arc :: Double -> Double -> Double -> Double -> Double -> Render ()
arc a b c d e = ctx (C.arc a b c d e True)

clip :: Render ()
clip = ctx (C.clip)

moveTo :: Double -> Double -> Render ()
moveTo x y = do
  ctx (C.moveTo x y)
  move (x,y)

relLineTo :: Double -> Double -> Render ()
relLineTo x y = do
  p <- at
  let p'@(x',y') = p + (x,y)
  ctx (C.lineTo x' y')
  move p'

relCurveTo :: Double -> Double -> Double -> Double -> Double -> Double -> Render ()
relCurveTo ax ay bx by cx cy = do
  p <- at
  let [(ax',ay'),(bx',by'),(cx',cy')] = map (p+) [(ax,ay),(bx,by),(cx,cy)]
  ctx (C.bezierCurveTo ax' ay' bx' by' cx' cy')
  move (cx',cy')

stroke :: Render ()
stroke = ctx C.stroke

fill :: Render ()
fill = maybe (ctx C.fill) fillRule =<< getFill

fillRule :: FillRule -> Render ()
fillRule Winding = ctx (C.fillRule "nonzero")
fillRule EvenOdd = ctx (C.fillRule "evenodd")

save :: Render ()
save = ctx C.save

restore :: Render ()
restore = ctx C.restore

colorToJSRGBA :: Color c => c -> (Int, Int, Int, Double)
colorToJSRGBA c = (f r, f g, f b, alphaChannel c')
  where
   c'          = toAlphaColour c
   (RGB r g b) = toRGB (alphaToColour c')
   f = floor . transferFunction
   transferFunction :: Double -> Double
   transferFunction lin | lin == 1         = 255
                        | lin <= 0.0031308 = 255*12.92*lin
                        | otherwise        = 255*((1 + a)*lin**(1/2.4) - a)
     where a = 0.055

   alphaToColour :: AlphaColour Double -> Colour Double
   alphaToColour ac | alphaChannel ac == 0 = ac `over` black
                    | otherwise = darken (recip (alphaChannel ac)) (ac `over` black)

transform :: Double -> Double -> Double -> Double -> Double -> Double -> Render ()
transform ax ay bx by tx ty = ctx (C.transform ax ay bx by tx ty)

strokeColor :: (Color c) => c -> Render ()
strokeColor c = ctx (C.strokeStyle r g b a)
  where (r,g,b,a) = colorToJSRGBA c

fillColor :: (Color c) => c -> Render ()
fillColor c = ctx (C.fillStyle r g b a)
  where (r,g,b,a) = colorToJSRGBA c

dashing :: Dashing -> Render ()
dashing (Dashing a o) =  ctx (C.setLineDash a)
                      >> ctx (C.lineDashOffset o)

lineWidth :: Double -> Render ()
lineWidth w | abs w < 0.00001 = ctx (C.lineWidth 0.00001)
            | otherwise       = ctx (C.lineWidth w)

lineCap :: LineCap -> Render ()
lineCap lc = ctx (C.lineCap $ n lc)
  where
    n LineCapButt  = C.LineCapButt
    n LineCapRound = C.LineCapRound
    n _            = C.LineCapSquare

lineJoin :: LineJoin -> Render ()
lineJoin lj = ctx (C.lineJoin $ n lj)
  where
    n LineJoinMiter = C.LineJoinMiter
    n LineJoinRound = C.LineJoinRound
    n _             = C.LineJoinBevel

globalAlpha :: Double -> Render ()
globalAlpha a = ctx (C.globalAlpha a)

translate :: Double -> Double -> Render ()
translate x y = ctx (C.translate x y)

scale :: Double -> Double -> Render ()
scale x y = ctx (C.scale x y)

rotate :: Double -> Render ()
rotate t = ctx (C.rotate t)

withStyle :: Render () -> Render () -> Render () -> Render ()
withStyle t s r = do
  ctx C.save
  r >> t >> s
  stroke
  fill
  ctx C.restore

