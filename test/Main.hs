{-# LANGUAGE GADTs             #-}
{-# LANGUAGE OverloadedStrings #-}
module Main where

import           Control.Monad
import           Data.Coerce
import           Data.JSString                  (JSString)
import           Data.Monoid                    ((<>))
import           Diagrams.Backend.GHCJS         as D
import           Diagrams.Prelude               hiding ((<>))
import           GHCJS.DOM
import           GHCJS.DOM.Document
import           GHCJS.DOM.Element
import           GHCJS.DOM.Node
import           GHCJS.DOM.Types
import qualified JavaScript.Array               as J
import qualified JavaScript.Web.Canvas          as C
import qualified JavaScript.Web.Canvas.Internal as C
import           Tests

mkContext :: JSString -> IO C.Context
mkContext nm = do
    Just doc <- currentDocument
    Just testarea <- getElementById doc ("main" :: JSString)
    let img = "<img style=\"border:1px solid #d3d3d3;\" "
              <> "src=\"../ref/" <> nm <> ".png\" />"
    let canvas = "<canvas id=\"" <> nm <> "\" width=\"200\" height=\"200\""
                 <> "style=\"border:1px solid #d3d3d3;\">"
                 <> "</canvas><br />"
    Just tr <- createElement doc $ Just ("tr" :: JSString)
    setInnerHTML tr . Just $ "<td valign=\"top\" bgcolor=\"#eeeeee\">"
            <> nm <> "</td>"
            <> "<td valign=\"top\">" <> img <> "</td>"
            <> "<td valign=\"top\">" <> canvas <> "</td>"
    appendChild testarea $ Just tr

    Just c <- getElementById doc nm
    C.getContext . coerce  $ c

renderDia' :: C.Context -> Diagram D.Canvas -> IO ()
renderDia' c = renderDia D.Canvas (CanvasOptions (dims2D 200 200) c)

main = do
    forM_ examples $ \(Test testName dia) -> do
        ctx <- mkContext testName
        renderDia' ctx dia
    putStrLn "end"
