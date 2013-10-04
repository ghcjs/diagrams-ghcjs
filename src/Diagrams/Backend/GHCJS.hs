{-# LANGUAGE TypeFamilies
           , MultiParamTypeClasses
           , FlexibleInstances
           , FlexibleContexts
           , TypeSynonymInstances
           , DeriveDataTypeable
           , ViewPatterns
           , OverloadedStrings
           , TypeHoles
           , InstanceSigs
  #-}

module Diagrams.Backend.GHCJS
  ( Canvas(..)
  , Options(..)
  ) where

import           Control.Monad        (when)
import qualified Data.Foldable        as F
import           Data.Maybe           (catMaybes, maybe, isJust, fromJust)
import           Data.Monoid          ((<>))
import           Data.Typeable
import qualified Data.Text            as T
import           Control.Monad.Reader
import           Control.Monad.State

import           Diagrams.Prelude     hiding ((<>))
import           Diagrams.TwoD.Adjust (adjustDia2D)
import           Diagrams.TwoD.Path   (getFillRule, getClip, Clip)
import           Diagrams.TwoD.Text   (getFont, getFontSize)
import qualified Diagrams.TwoD.Text   as D
import           Diagrams.Segment

import qualified Graphics.Rendering.GHCJS as G

-- | This data declaration is simply used as a token to distinguish this
-- rendering engine.
data Canvas = Canvas deriving Typeable

instance Monoid (Render Canvas R2) where
  mempty  = C $ return ()
  (C c1) `mappend` (C c2) = C (c1 >> c2)

-- | Did we see any lines in the most recent path (as opposed to loops)? If
-- so then we should ignore any fill attribute because diagrams-lib
-- separates lines and loops into separate path primitives so we don't have
-- to worry about seeing them in the same path
-- type G.Render a = StateT Bool G.Render a

instance Backend Canvas R2 where
    data Render  Canvas R2 = C (G.Render ())
    type Result  Canvas R2 = IO ()
    data Options Canvas R2 = CanvasOptions
            { canvasSize   :: SizeSpec2D   -- ^ the requested size
            , context      :: G.Context    -- ^ drawing context to render to
            }

    withStyle _ s t (C r) = C $ G.tempState $ do
        clearPath  -- path is not part of the drawing state :/
        canvasMiscStyle s
        G.setIgnoreFill False
        r
        ignoreFill <- G.getIgnoreFill
        canvasTransf t
        canvasStyle ignoreFill s
        G.stroke

    doRender _ (CanvasOptions _ c) (C r) = G.doRender c r

    adjustDia c opts d = adjustDia2D canvasSize setCanvasSize c opts
                         -- (d # reflectY # fcA transparent # lw 0.01)
                         (d # reflectY)
        where setCanvasSize sz o = o { canvasSize = sz }

clearPath :: G.Render()
clearPath = G.newPath >> G.closePath

renderC :: (Renderable a Canvas, V a ~ R2) => a -> G.Render ()
renderC a = case (render Canvas a) of C r -> r

canvasMiscStyle :: Style v -> G.Render ()
canvasMiscStyle s = sequence_ $ catMaybes
    [ handleClipping s
    , handleFont s
    , handle fColor
    , handle fRule
    ] where
        handle :: AttributeClass a => (a -> G.Render ()) -> Maybe (G.Render ())
        handle f = f `fmap` getAttr s
        fColor   = G.fillColor   . getFillColor
        fRule    = G.setFill     . getFillRule

handleFont s = Just $ G.setFont $
    G.fromFontSlant fontSlant
    <> G.fromFontWeight fontWeight
    <> T.pack (show fontSize)
    <> "px "
    <> (T.pack fontFamily)
    where
        fontFamily' = getAttr s :: Maybe (D.Font)
        fontSize'   = getAttr s :: Maybe (D.FontSize)
        fontSlant'  = getAttr s :: Maybe (D.FontSlantA)
        fontWeight' = getAttr s :: Maybe (D.FontWeightA)
        fontFamily  = maybe "Arial" getFont fontFamily'
        fontSize    = maybe 10 getFontSize fontSize'
        fontSlant   = maybe D.FontSlantNormal D.getFontSlant fontSlant'
        fontWeight  = maybe D.FontWeightNormal D.getFontWeight fontWeight'

handleClipping :: Style v -> Maybe (G.Render ())
handleClipping s = (clipCanv . getClip) `fmap` getAttr s

canvasStyle :: Bool -> Style v -> G.Render ()
canvasStyle ignoreFill s = foldr (>>) (return ()) . catMaybes $
    [ handle lColor
    , handle lWidth
    , handle lJoin
    , handle lCap
    , handle opacity_
    , handle dashing_
    , if ignoreFill then Nothing else handle fRule
    , if ignoreFill then Nothing else handle fColor
    ] where
        handle :: AttributeClass a => (a -> G.Render ()) -> Maybe (G.Render ())
        handle f = f `fmap` getAttr s
        lColor   = G.strokeColor . getLineColor
        fColor s = (G.fillColor   $ getFillColor s) >> G.fill
        lWidth   = G.lineWidth   . getLineWidth
        lCap     = G.lineCap     . getLineCap
        lJoin    = G.lineJoin    . getLineJoin
        opacity_ = G.globalAlpha . getOpacity
        fRule    = G.setFill     . getFillRule
        dashing_ = G.dashing     . getDashing

clipCanv :: [Path R2] -> G.Render ()
clipCanv pths = mapM_ renderPath pths >> G.clip

renderPath :: Path R2 -> G.Render ()
renderPath (Path trs) = G.newPath >> F.mapM_ renderTrail trs

canvasTransf :: Transformation R2 -> G.Render ()
canvasTransf = uncurry6 G.setTransform . getMatrix

getMatrix :: Transformation R2
          -> (Double, Double, Double, Double, Double, Double)
getMatrix t = (a1,a2,b1,b2,c1,c2) where
    (a1,a2) = unr2 $ apply t unitX
    (b1,b2) = unr2 $ apply t unitY
    (c1,c2) = unr2 $ transl t

instance Renderable (Segment Closed R2) Canvas where
  -- The atomic constituents of paths are segments, which are single
  -- straight lines or cubic Bezier curves.
  render _ (Linear (OffsetClosed v)) = C $ uncurry G.relLineTo (unr2 v)
  render _ (Cubic (unr2 -> (x1,y1))
                  (unr2 -> (x2,y2))
                  (OffsetClosed (unr2 -> (x3,y3))))
    = C $ G.relCurveTo x1 y1 x2 y2 x3 y3

instance Renderable (Trail R2) Canvas where
  -- A trail is a sequence of segments placed end-to-end. Trails are thus
  -- translationally invariant, and form a monoid under concatenation.
  -- Trails can also be open (the default) or closed (the final point in
  -- a closed trail is implicitly connected back to the starting point).
  render _ t = flip withLine t $ renderT . lineSegments where
      renderT segs = C $ do
          mapM_ renderC segs
          when (isLoop t) G.closePath

          when (isLine t) (G.setIgnoreFill True)
          -- remember that we saw a Line, so we will ignore fill attribute

instance Renderable (Path R2) Canvas where
  -- A path is a (possibly empty) list of trails, with each trail paired
  -- with an absolute starting point.
  -- "Note that paths with multiple trails are necessary for being able to
  -- draw e.g. filled objects with holes in them."
  render _ (Path trs) = C $ G.newPath >> F.mapM_ renderTrail trs

instance Renderable D.Text Canvas where
    render _ (D.Text tt a str) = C $ G.tempState $ do
        uncurry6 G.setTransform $ getMatrix $ tt <> reflectionY
        -- TODO(joel) - major hack here
        fontSz <- (/10) <$> G.fontSize <$> get

        (refX, refY) <- case a of
            D.BoxAlignedText xt yt -> do
                tExt <- G.measureText $ T.pack str
                return (lerp 0 tExt xt, lerp (-fontSz/4) fontSz yt)

                -- text metrics are in the canvas v5 standard, but not yet
                -- implemented
                -- http://lists.whatwg.org/pipermail/whatwg-whatwg.org/2012-March/035239.html
                -- Until then estimate the ascent and descent in a rather
                -- gross way.
            D.BaselineText -> return (0, 0)
        uncurry6 G.transform $ getMatrix $
            moveOriginBy (r2 (refX, -refY)) mempty
        G.fillText (T.pack str)

renderTrail :: Located (Trail R2) -> G.Render ()
renderTrail (viewLoc -> (unp2 -> p, tr)) = do
    uncurry G.moveTo p
    renderC tr

uncurry6 :: (a -> b -> c -> d -> e -> f -> g) -> (a, b, c, d, e, f) -> g
uncurry6 f (x, y, z, w, s, t) = f x y z w s t
