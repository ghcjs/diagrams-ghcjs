{-# LANGUAGE DeriveDataTypeable        #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE MultiParamTypeClasses     #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE RankNTypes                #-}

module Tests
        ( Test(..)
        , examples
        ) where

import           Data.Typeable
import           Diagrams.Coordinates ((^&))
import           Diagrams.Core.Points
import           Diagrams.Prelude     hiding (connect)
import           Diagrams.TwoD.Text

-----------------------------------------------------------------------

data Test = Test
        String -- ^ the name of the test
        (forall canvas .
                ( Renderable (Path R2) canvas
                , Renderable Text      canvas
                , Backend canvas R2
                ) => Diagram canvas R2
        ) -- ^ and the diagram

-----------------------------------------------------------------------

-- ^ list of cannonical examples.
examples :: [Test]
examples =
        [ Test "square1" $ square 1
        , Test "circle1" $ circle 1
        , Test "circle-square" $
                circle 1 ||| square 1
        , Test "2-circles" $
                circle 0.5 <> unitCircle
        , Test "ellipse" $
                unitCircle # scaleX 0.5 # rotateBy (1/6)
        , Test "arc" $
                arc (tau/4 @@ rad) (4 * tau / 7 @@ rad)
        , Test "Pre-defined-shapes" $
                square 1 ||| rect 0.3 0.5 ||| eqTriangle 1 ||| roundedRect 0.7 0.4 0.1
        , Test "circle-hrule-circle" $
                circle 1 ||| hrule 2 ||| circle 1
        , Test "poly-example" $
                poly_example
        , Test "star-polygon" $
                star (StarSkip 3) (regPoly 13 1) # stroke
        , Test "star-skip" $
                stroke (star (StarSkip 2) (regPoly 8 1))
                       ||| strutX 1
                       ||| stroke (star (StarSkip 3) (regPoly 8 1))
        , Test "superimposing" $
                circle 1 `atop` square (sqrt 2)
        , Test "superimposing-color" $
                mconcat [ circle 0.1 # fc green
                        , eqTriangle 1 # scale 0.4 # fc yellow
                        , square 1 # fc blue
                        , circle 1 # fc red
                        ]
        , Test "juxtaposing1" $
                beside (20 ^& 30) (circle 1 # fc orange) (circle 1.5 # fc purple)
                        # showOrigin
        , Test "juxtaposing2" $
                let d1 = circle 1 # fc red
                    d2 = square 1 # fc blue
                in  (d1 ||| d2) ||| strutX 3 ||| ( d1
                                                   ===
                                                   d2  )

                    -- TODO port freeze example to Measure
        -- , Test "freeze" $
        --        (square 1 ||| square 1 # freeze # scale 2
        --                  ||| circle 1 # freeze # scaleX 3
        --        ) # lw 0.03

        , Test "line-attributes" $
               let path = fromVertices [0 ^& 0, 1 ^& 0.3, 2 ^& 0, 2.2 ^& 0.3] # lwG 0.1
               in pad 1.1 . centerXY . vcat' (with & sep .~ 0.1)
                  $ map (path #)
                  [ lineCap LineCapButt   . lineJoin LineJoinMiter
                  , lineCap LineCapRound  . lineJoin LineJoinRound
                  , lineCap LineCapSquare . lineJoin LineJoinBevel
                  , dashingG [0.1,0.2,0.3,0.1] 0
                  ]

        , Test "text-basic" $
               text "Hello world!" <> rect 8 1

        , Test "text-alignment" $
               let pt = circle 0.1 # fc red

                   t1 = pt <> topLeftText         "top left"   <> rect 8 1
                   t2 = pt <> baselineText        "baseline"   <> rect 8 1
                   t3 = pt <> alignedText 0.7 0.5 "(0.7, 0.5)" <> rect 8 1

                   d1 =/= d2 = d1 === strutY 2 === d2

               in  t1 =/= t2 =/= t3

        , Test "text-attributes" $
               let text' s t = text t # fontSizeG s <> strutY (s * 1.3)
               in pad 1.1 . centerXY $
                    text' 10 "Hello" # italic
                    === text' 5 "there"  # bold # font "freeserif"
                    === text' 3 "world"  # fc green

        , Test "text-transforms" $
               let eff = text "F" <> square 1 # lwG 0
                   ts  = [ scale (1/2), id, scale 2, scaleX 2, scaleY 2
                         , scale (-1), scaleX (-1), scaleY (-1)
                         ]

               in  pad 1.1 . hcat . map (eff #) $ ts

        , Test "ring" $
               let ring :: Path R2
                   ring = circle 3 <> circle 2

               in  stroke ring # fc purple # fillRule EvenOdd # pad 1.1

        , Test "fill-rules" $
               let loopyStar = fc red
                             . mconcat . map (cubicSpline True)
                             . pathVertices
                             . star (StarSkip 3)
                             $ regPoly 7 1
               in   loopyStar # fillRule EvenOdd
                    ||| strutX 1
                    ||| loopyStar # fillRule Winding

        , Test "clip" $
                square 3
                # fc green
                # lwG 0.05
                # clipBy (square 3.2 # rotateBy (1/10))

        , Test "alpha-color" $

               let colors  = map (blue `withOpacity`) [0.1, 0.2 .. 1.0]
               in  hcat' (with & catMethod .~ Distrib & sep .~ 1)
                     (zipWith fcA colors (repeat (circle 1)))

        , Test "opacity1" $
               let s c     = square 1 # fc c
                   reds    = (s darkred ||| s red) === (s pink ||| s indianred)
               in  hcat' (with & sep .~ 1) . take 4 . iterate (opacity 0.7) $ reds

        , Test "fat" $
               unitCircle # lwG 0.3 # scaleX 2 # pad 1.3

        , Test "connect" $ connect_example

        , Test "fill-line" $
               strokeLine (fromVertices [origin, 0 ^& 2, 3 ^& 3, 4 ^& 1])
                 # fc blue

        , Test "fill-loop" $
               strokeLoop (fromVertices [origin, 0 ^& 2, 3 ^& 3, 4 ^& 1] # closeLine)
                 # fc blue

        , Test "line-loop" $
               fc green $
               stroke $
               trailLike ((fromVertices [origin, 0 ^& 2, 3 ^& 3, 4 ^& 1] # rotateBy (1/12) # closeLine # wrapLoop) `at` origin)
               <>
               trailLike ((fromVertices [origin, 0 ^& 2, 3 ^& 3, 4 ^& 1] # wrapLine) `at` origin)

        ]

poly_example = (poly1 ||| strutX 1 ||| poly2) # lwG 0.05
  where
          poly1 = polygon (with & polyType .~ PolyRegular 13 5
                           & polyOrient .~ OrientV)
          poly2 = polygon (with & polyType .~ PolyPolar  (repeat (1/40 @@ turn))
                           (take 40 $ cycle [2,7,4,6]))

data Corner = NW | NE | SW | SE
  deriving (Typeable, Eq, Ord, Show)
instance IsName Corner

connect n1 n2
  = withName n1 $ \b1 ->
    withName n2 $ \b2 ->
      atop ((location b1 ~~ location b2) # lc red # lwG 0.05)

squares =  (s # named NW ||| s # named NE)
       === (s # named SW ||| s # named SE)
  where s = square 1 # lwG 0.05

d = hcat' (with & sep .~ 0.5) (zipWith (|>) [0::Int ..] (replicate 5 squares))

pairs = [ ((0::Int) .> NE, (2::Int) .> SW)
        , ((1::Int) .> SE, (4::Int) .> NE)
        , ((3::Int) .> NW, (3::Int) .> SE)
        , ((0::Int) .> SE, (1::Int) .> NW)
        ]

connect_example = d # applyAll (map (uncurry connect) pairs)
