{-# LANGUAGE DeriveDataTypeable    #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE InstanceSigs          #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeSynonymInstances  #-}
{-# LANGUAGE ViewPatterns          #-}

module Diagrams.Backend.GHCJS
  ( Canvas(..)
  , Options(..)
  ) where

import           Prelude                  hiding (FilePath, foldl, foldl1,
                                           foldr, foldr1, head, init, last,
                                           lines, map, mapM, mapM_, sequence,
                                           sequence_, tail, (!!), (++))

import           Control.Lens             hiding (( # ))
import           Control.Monad            (when)
import           Control.Monad.Reader     ()
import           Control.Monad.State      (get)
import           Data.Foldable
import           Data.Maybe
import qualified Data.Text                as T
import           Data.Tree                (Tree (Node))
import           Data.Typeable

import           Diagrams.Core.Types
import           Diagrams.Prelude         hiding (view)
import           Diagrams.Segment
import           Diagrams.TwoD.Adjust     (adjustDia2D)
import           Diagrams.TwoD.Path       (Clip (..), getFillRule)
import           Diagrams.TwoD.Text       (getFont, getFontSize)
import qualified Diagrams.TwoD.Text       as D

import qualified Graphics.Rendering.GHCJS as G

-- | This data declaration is simply used as a token to distinguish this
-- rendering engine.
data Canvas = Canvas deriving Typeable

instance Monoid (Render Canvas V2 Double) where
  mempty  = C $ return ()
  (C c1) `mappend` (C c2) = C (c1 >> c2)

-- | Did we see any lines in the most recent path (as opposed to loops)? If
-- so then we should ignore any fill attribute because diagrams-lib
-- separates lines and loops into separate path primitives so we don't have
-- to worry about seeing them in the same path
-- type G.Render a = StateT Bool G.Render a

instance Backend Canvas V2 Double where
    data Render  Canvas V2 Double = C (G.Render ())
    type Result  Canvas V2 Double = IO ()
    data Options Canvas V2 Double = CanvasOptions
            { canvasSize   :: SizeSpec V2 Double   -- ^ the requested size
            , context      :: G.Context    -- ^ drawing context to render to
            }

    -- doRender _ (CanvasOptions _ c) (C r) = G.doRender c r

    renderRTree :: Canvas -> Options Canvas V2 Double -> RTree Canvas V2 Double Annotation
                        -> Result Canvas V2 Double
    renderRTree _canvas (CanvasOptions _ c) rt = G.render c r where
      (C r) = renderFromRTree rt

    adjustDia c opts d = adjustDia2D size c opts (d # reflectY)
        where
          setCanvasSize o sz = o { canvasSize = sz }
          size :: Lens' (Options Canvas V2 Double) (SizeSpec V2 Double)
          size = lens canvasSize setCanvasSize

renderFromRTree :: RTree Canvas V2 Double Annotation -> Render Canvas V2 Double
-- renderFromRTree = C $ return ()
renderFromRTree (Node (RPrim p ) _) = render Canvas p
renderFromRTree (Node (RStyle sty) rs) = C . G.tempState $ do
    clearPath  -- path is not part of the drawing state :/
    canvasMiscStyle sty
    G.setIgnoreFill False
    runC $ foldMap renderFromRTree rs
    ignoreFill <- G.getIgnoreFill
    canvasStyle ignoreFill sty
    G.stroke
renderFromRTree (Node _ rs) = foldMap renderFromRTree rs

clearPath :: G.Render()
clearPath = G.newPath >> G.closePath

runC :: Render Canvas V2 Double -> G.Render ()
runC (C r) = r

-- | Get an accumulated style attribute from the render monad state.
getStyleAttrib :: AttributeClass a => (a -> b) -> G.Render (Maybe b)
getStyleAttrib f = fmap f . getAttr . G.accumStyle <$> get

renderC :: (Renderable a Canvas, Vn a ~ V2 Double) => a -> G.Render ()
renderC a = case (render Canvas a) of C r -> r

canvasMiscStyle :: Style v n -> G.Render ()
canvasMiscStyle s = sequence_ $ catMaybes
    [ handleClipping s
    -- , handleFont s
    , handle fColor
    , handle fRule
    ] where
        handle :: AttributeClass a => (a -> G.Render ()) -> Maybe (G.Render ())
        handle f = f `fmap` getAttr s
        fColor   = G.fillColor . colorFromTexture . getFillTexture
        fRule    = G.setFill     . getFillRule

-- handleFont :: Style v -> G.Render ()
-- handleFont s = G.setFont $
--     G.fromFontSlant fontSlant
--     <> G.fromFontWeight fontWeight
--     <> T.pack (show fontSize)
--     <> "px "
--     <> (T.pack fontFamily)
--     where
--         fontFamily' = getAttr s :: Maybe (D.Font)
--         fontSize'   = getAttr s :: Maybe (D.FontSize)
--         fontSlant'  = getAttr s :: Maybe (D.FontSlantA)
--         fontWeight' = getAttr s :: Maybe (D.FontWeightA)
--         fontFamily  = maybe "Arial" getFont fontFamily'
--         fontSize    = maybe (Output 10) getFontSize fontSize'
--         fontSlant   = maybe D.FontSlantNormal D.getFontSlant fontSlant'
--         fontWeight  = maybe D.FontWeightNormal D.getFontWeight fontWeight'

handleClipping :: Style v n -> Maybe (G.Render ())
handleClipping s = (clipCanv . \(Clip x) -> x) `fmap` getAttr s

canvasStyle :: Bool -> Style v n -> G.Render ()
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
        lColor   = G.strokeColor . colorFromTexture . getLineTexture
        fColor s = (G.fillColor . colorFromTexture . getFillTexture $ s) >> G.fill
        lWidth   = G.lineWidth   . getLineWidth
        lCap     = G.lineCap     . getLineCap
        lJoin    = G.lineJoin    . getLineJoin
        opacity_ = G.globalAlpha . getOpacity
        fRule    = G.setFill     . getFillRule
        dashing_ = G.dashing     . getDashing

-- TODO handle gradients properly
colorFromTexture :: Texture Double -> SomeColor
colorFromTexture (SC c) = c
-- For gradients, just pick the first color we can find
colorFromTexture (LG g) = g ^?! lGradStops . ix 0 . stopColor
colorFromTexture (RG g) = g ^?! rGradStops . ix 0 . stopColor

clipCanv :: [Path V2 Double] -> G.Render ()
clipCanv pths = mapM_ renderPath pths >> G.clip

renderPath :: Path V2 Double -> G.Render ()
renderPath (Path trs) = G.newPath >> mapM_ renderTrail trs

canvasTransf :: Transformation V2 Double -> G.Render ()
canvasTransf = uncurry6 G.setTransform . getMatrix

getMatrix :: Transformation V2 Double
          -> (Double, Double, Double, Double, Double, Double)
getMatrix t = (a1,a2,b1,b2,c1,c2) where
    (a1,a2) = unr2 $ apply t unitX
    (b1,b2) = unr2 $ apply t unitY
    (c1,c2) = unr2 $ transl t

instance Renderable (Segment Closed V2 Double) Canvas where
  -- The atomic constituents of paths are segments, which are single
  -- straight lines or cubic Bezier curves.
  render _ (Linear (OffsetClosed v)) = C $ uncurry G.relLineTo (unr2 v)
  render _ (Cubic (unr2 -> (x1,y1))
                  (unr2 -> (x2,y2))
                  (OffsetClosed (unr2 -> (x3,y3))))
    = C $ G.relCurveTo x1 y1 x2 y2 x3 y3

instance Renderable (Trail V2 Double) Canvas where
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

instance Renderable (Path V2 Double) Canvas where
  -- A path is a (possibly empty) list of trails, with each trail paired
  -- with an absolute starting point.
  -- "Note that paths with multiple trails are necessary for being able to
  -- draw e.g. filled objects with holes in them."
  render _ (Path trs) = C $ G.newPath >> mapM_ renderTrail trs

showFontJS :: D.FontWeight -> D.FontSlant -> Double -> String -> T.Text
showFontJS wgt slant sz fnt = T.concat [a, " ", b, " ", c, " ", d]
  where
    a = case wgt of
          D.FontWeightNormal -> ""
          D.FontWeightBold   -> "bold"
    b = case slant of
          D.FontSlantNormal  -> ""
          D.FontSlantItalic  -> "italic"
          D.FontSlantOblique -> "oblique"
    c = T.concat [T.pack $ show sz, "pt"]
    d = T.pack fnt

instance Renderable (D.Text Double)Canvas where
    render _ (D.Text tt al str) = C $ G.tempState $ do
        -- handle font size
        sz <- fromMaybe 12 <$> getStyleAttrib getFontSize
        -- font description
        ff <- fromMaybe "Calibri" <$> getStyleAttrib getFont
        slant   <- fromMaybe D.FontSlantNormal <$> getStyleAttrib D.getFontSlant
        fw      <- fromMaybe D.FontWeightNormal <$> getStyleAttrib D.getFontWeight
        G.setFont $ showFontJS fw slant sz ff
        -- color
        fill <- fromMaybe (SC (SomeColor (black :: Colour Double))) <$>
                getStyleAttrib getFillTexture
        G.fillColor $ colorFromTexture fill
        -- text alignment & positioning
        uncurry6 G.setTransform $ getMatrix $ tt <> reflectionY
        (refX, refY) <- case al of
            D.BoxAlignedText xt yt -> do
                -- TODO(joel) - major hack here
                width <- G.measureText $ T.pack str
                height <- (/10) <$> G.fontSize <$> get
                let lerp' alpha u v = alpha * u + (1 - alpha) * v
                return (lerp' 0 width xt, lerp' (-height/4) height yt)
                -- text metrics are in the canvas v5 standard, but not yet
                -- implemented
                -- http://lists.whatwg.org/pipermail/whatwg-whatwg.org/2012-March/035239.html
                -- Until then estimate the ascent and descent in a rather
                -- gross way.
            D.BaselineText -> return (0, 0)
        uncurry6 G.transform $ getMatrix $
            moveOriginBy (r2 (refX, -refY)) mempty
        G.fillText (T.pack str)

renderTrail :: Located (Trail V2 Double) -> G.Render ()
renderTrail (viewLoc -> (unp2 -> p, tr)) = do
    uncurry G.moveTo p
    renderC tr

uncurry6 :: (a -> b -> c -> d -> e -> f -> g) -> (a, b, c, d, e, f) -> g
uncurry6 f (x, y, z, w, s, t) = f x y z w s t
