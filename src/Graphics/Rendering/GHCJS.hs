{-# LANGUAGE OverloadedStrings #-}
module Graphics.Rendering.GHCJS
  ( Render(..)
  , RenderState(..)
  , Context(..)
  , render

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
  , fillText
  , getIgnoreFill
  , setIgnoreFill
  , transform
  , strokeText
  , setTransform
  , save
  , restore
  , translate
  , scale
  , rotate
  , strokeColor
  , setFont
  , dashing
  , fillColor
  , lineWidth
  , lineCap
  , fromFontSlant
  , fromFontWeight
  , measureText
  , lineJoin
  , globalAlpha
  , tempState
  , colorToJSRGBA
  ) where

import           Control.Monad.Reader
import           Control.Monad.State
import           Data.Colour
import           Data.Colour.SRGB.Linear
import           Data.Maybe
import           Data.Monoid
import           Data.NumInstances       ()
import           Data.Text               (Text, unpack)
import qualified Data.Text               as T

import           Diagrams.Attributes     (Color (..), LineCap (..),
                                          LineJoin (..), colorToRGBA)
import           Diagrams.Attributes     (Dashing (..))
import           Diagrams.Core           (Style)
import           Diagrams.TwoD.Path      (FillRule (..))
import qualified Diagrams.TwoD.Text      as D
import           Linear                  (V2)

import           JavaScript.Canvas       (Context)
import qualified JavaScript.Canvas       as C

data RenderState = RenderState
-- | Did we see any lines in the most recent path (as opposed to loops)? If
-- so then we should ignore any fill attribute because diagrams-lib
-- separates lines and loops into separate path primitives so we don't have
-- to worry about seeing them in the same path
    { ignoreFill      :: Bool
    , currentLocation :: (Double, Double)
    , currentFillRule :: Maybe FillRule
    , fontSize        :: Double
    , accumStyle      :: Style V2 Double
    }
type Render = StateT RenderState (ReaderT Context IO)

render :: Context -> Render a -> IO a
render c r = runReaderT (evalStateT r startState) c
    where startState = RenderState False (0, 0) Nothing 10 mempty

ctx :: (Context -> IO a) -> Render a
ctx f = lift ask >>= liftIO . f

move :: (Double,Double) -> Render ()
move p = modify $ \state -> state {currentLocation=p}

at :: Render (Double,Double)
at = fmap currentLocation get

getFill :: Render (Maybe FillRule)
getFill = fmap currentFillRule get

setFill :: FillRule -> Render ()
setFill fr = modify $ \state -> state {currentFillRule=Just fr}

getIgnoreFill :: Render Bool
getIgnoreFill = fmap ignoreFill get

setIgnoreFill :: Bool -> Render ()
setIgnoreFill ignore = modify$ \state -> state {ignoreFill=ignore}

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

relCurveTo :: Double -> Double -> Double -> Double -> Double -> Double
           -> Render ()
relCurveTo ax ay bx by cx cy = do
  p <- at
  let [(ax',ay'),(bx',by'),(cx',cy')] = map (p+) [(ax,ay),(bx,by),(cx,cy)]
  ctx (C.bezierCurveTo ax' ay' bx' by' cx' cy')
  move (cx',cy')

fillText :: Text -> Render ()
fillText txt = ctx (C.fillText txt 0 0)

strokeText :: Text -> Render ()
strokeText txt = ctx (C.strokeText txt 0 0)

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

tempState :: Render () -> Render ()
tempState r = save >> r >> restore

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

transform :: Double -> Double -> Double -> Double -> Double -> Double
          -> Render ()
transform ax ay bx by tx ty = ctx (C.transform ax ay bx by tx ty)

setTransform :: Double -> Double -> Double -> Double -> Double -> Double
             -> Render ()
setTransform ax ay bx by tx ty = ctx (C.setTransform ax ay bx by tx ty)

strokeColor :: (Color c) => c -> Render ()
strokeColor c = ctx (C.strokeStyle r g b a)
    where (r,g,b,a) = colorToJSRGBA c

setFont :: Text -> Render ()
setFont f = ctx (C.font f)

fillColor :: (Color c) => c -> Render ()
fillColor c = ctx (C.fillStyle r g b a)
    where (r,g,b,a) = colorToJSRGBA c

dashing :: Dashing Double -> Render ()
dashing (Dashing a o) = ctx (C.setLineDash a)
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

fromFontSlant :: D.FontSlant -> T.Text
fromFontSlant D.FontSlantNormal = "" -- " normal "
fromFontSlant D.FontSlantItalic = " italic "
fromFontSlant D.FontSlantOblique = " oblique "

fromFontWeight :: D.FontWeight -> T.Text
fromFontWeight D.FontWeightNormal = "" -- " normal "
fromFontWeight D.FontWeightBold = " bold "

measureText :: Text -> Render Double
measureText t = ctx (C.measureText t)

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
