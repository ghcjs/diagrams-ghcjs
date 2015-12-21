{-# LANGUAGE GADTs             #-}
{-# LANGUAGE OverloadedStrings #-}
module Main where

import           Control.Monad
import           Control.Monad.Trans
import           Data.Coerce
import           Data.Monoid                    ((<>))
import qualified Data.Text                      as T
import           Diagrams.Backend.GHCJS         as D
import           Diagrams.Prelude               hiding ((<>))
import           GHCJS.Foreign
import           GHCJS.Types
import qualified Graphics.Rendering.GHCJS       as G
import           JavaScript.Array               as J
import           JavaScript.JQuery              as JQ
import qualified JavaScript.Web.Canvas          as C
import qualified JavaScript.Web.Canvas.Internal as C
import           Tests
import           Unsafe.Coerce

import           Debug.Trace

mkContext nm = do
    testarea <- select "#main"
    let img = "<img style=\"border:1px solid #d3d3d3;\" "
              <> "src=\"../ref/" <> nm <> ".png\" />"
    let canvas = "<canvas id=\"" <> nm <> "\" width=\"200\" height=\"200\""
                 <> "style=\"border:1px solid #d3d3d3;\">"
                 <> "</canvas><br />"
    JQ.append ("<tr><td valign=\"top\" bgcolor=\"#eeeeee\">"
            <> nm <> "</td>"
            <> "<td valign=\"top\">" <> img <> "</td>"
            <> "<td valign=\"top\">" <> canvas <> "</td>") testarea

    c <- select $ "#" <> nm
    C.getContext . unsafeCoerce . J.index 0 . unsafeCoerce $ c

renderDia' :: C.Context -> Diagram D.Canvas -> IO ()
renderDia' c = renderDia D.Canvas (CanvasOptions (dims2D 200 200) c)

main = do
    body <- select "#main"
    forM_ examples $ \(Test testName dia) -> do
        ctx <- mkContext testName
        renderDia' ctx dia
    putStrLn "end"
