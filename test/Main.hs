{-# LANGUAGE OverloadedStrings, ViewPatterns, GADTs #-}
module Main where

import Data.Monoid ((<>))
import Diagrams.Prelude hiding ((<>))
import Diagrams.Backend.GHCJS
import Control.Monad
import Control.Monad.Trans
import JavaScript.JQuery
import JavaScript.Canvas (Context, getContext)
import GHCJS.Foreign
import GHCJS.Types
import qualified Data.Text as T
import qualified Graphics.Rendering.GHCJS as G
import Diagrams.Backend.GHCJS
import qualified JavaScript.Canvas as C
import Tests

import Debug.Trace

mkContext nm = do
    testarea <- select "#main"    
    let img = "<img style=\"border:1px solid #d3d3d3;\" "
              <> "src=\"../ref/" <> nm <> ".png\" />"
    let canvas = "<canvas id=\"" <> nm <> "\" width=\"200\" height=\"200\""
                 <> "style=\"border:1px solid #d3d3d3;\">"
                 <> "</canvas><br />"
    append ("<tr><td valign=\"top\" bgcolor=\"#eeeeee\">"
            <> nm <> "</td>"
            <> "<td valign=\"top\">" <> img <> "</td>"
            <> "<td valign=\"top\">" <> canvas <> "</td>") testarea
            
    getContext =<< indexArray 0 . castRef =<< select ("#" <> nm)

renderDia' :: Context -> Diagram Canvas R2 -> IO ()
renderDia' c = renderDia Canvas (CanvasOptions (Dims 200 200) c)

main = do
    body <- select "#main"    
    forM_ examples $ \(Test testName dia) -> do
        ctx <- mkContext (T.pack testName)
        renderDia' ctx dia
    putStrLn "end"
   
