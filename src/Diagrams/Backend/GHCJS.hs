{-# LANGUAGE TypeFamilies
           , MultiParamTypeClasses
           , FlexibleInstances
           , FlexibleContexts
           , TypeSynonymInstances
           , DeriveDataTypeable
           , ViewPatterns
           , TypeHoles
           , InstanceSigs
  #-}

module Diagrams.Backend.GHCJS
  ( Canvas(..)
  , Options(..)
  ) where

import           Control.Monad (when)
import qualified Data.Foldable as F
import           Data.Maybe (catMaybes)
import           Data.Typeable
import           Control.Monad.Reader

import           Diagrams.Prelude
import           Diagrams.TwoD.Adjust (adjustDia2D)
import           Diagrams.Segment

import qualified Graphics.Rendering.GHCJS as G

import Debug.Trace

-- | This data declaration is simply used as a token to distinguish this rendering engine.
data Canvas = Canvas
    deriving Typeable

instance Monoid (Render Canvas R2) where
  mempty  = C $ return ()
  (C c1) `mappend` (C c2) = C (c1 >> c2)

instance Backend Canvas R2 where
  data Render  Canvas R2 = C (G.Render ())
  type Result  Canvas R2 = IO ()
  data Options Canvas R2 = CanvasOptions
          { canvasSize   :: SizeSpec2D   -- ^ the requested size
          , context      :: G.Context    -- ^ drawing context to render to
          }

  withStyle _ s t (C r) = C $ 
    G.withStyle (canvasTransf t) (canvasStyle s) r

  doRender _ (CanvasOptions _ c) (C r) = G.doRender c r

  adjustDia c opts d = adjustDia2D canvasSize setCanvasSize c opts
                       (d # reflectY # fcA transparent # lw 0.01)
    where setCanvasSize sz o = o { canvasSize = sz }

renderC :: (Renderable a Canvas, V a ~ R2) => a -> G.Render ()
renderC a = case (render Canvas a) of C r -> r

canvasStyle :: Style v -> G.Render ()
canvasStyle s = foldr (>>) (return ())
              . catMaybes $ [ handle fColor
                            , handle lColor
                            , handle lWidth
                            , handle lJoin
                            , handle lCap
                            , handle opacity_
                            ]
  where handle :: (AttributeClass a) => (a -> G.Render ()) -> Maybe (G.Render ())
        handle f = f `fmap` getAttr s
        lColor   = G.strokeColor . getLineColor
        fColor   = G.fillColor   . getFillColor
        lWidth   = G.lineWidth   . getLineWidth
        lCap     = G.lineCap     . getLineCap
        lJoin    = G.lineJoin    . getLineJoin
        opacity_ = G.globalAlpha . getOpacity

canvasTransf :: Transformation R2 -> G.Render ()
canvasTransf t = do
    traceM $ show (a1,a2,b1,b2,c1,c2)
    G.transform a1 a2 b1 b2 c1 c2
  where (unr2 -> (a1,a2)) = apply t unitX
        (unr2 -> (b1,b2)) = apply t unitY
        (unr2 -> (c1,c2)) = transl t

instance Renderable (Segment Closed R2) Canvas where
  render _ (Linear (OffsetClosed v)) = C $ 
      uncurry G.relLineTo (unr2 v)
  render _ (Cubic (unr2 -> (x1,y1))
                  (unr2 -> (x2,y2))
                  (OffsetClosed (unr2 -> (x3,y3))))
    = C $ G.relCurveTo x1 y1 x2 y2 x3 y3

instance Renderable (Trail R2) Canvas where
    render :: Canvas -> (Trail R2) -> Render Canvas (V (Trail R2))
    render c = render c . pathFromTrail
        
instance Renderable (Path R2) Canvas where
  render _ (Path trs) = C $ 
      G.newPath >> F.mapM_ renderTrail trs
      
renderTrail :: Located (Trail R2) -> G.Render ()
renderTrail (viewLoc -> (unp2 -> (x,y), t)) = do
    G.moveTo x y
    mapM_ renderC (trailSegments t)
    if isLoop t
       then G.closePath
       else return ()
