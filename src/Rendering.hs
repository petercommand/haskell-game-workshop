{-# LANGUAGE RankNTypes #-}
module Rendering (renderGame) where
import Types
import Graphics.Gloss
import Graphics.Gloss.Rendering
import qualified Data.Map as M
import GHC.Float
import Debug.Trace
import Data.Monoid
import Data.Time.Clock

import "GLFW-b" Graphics.UI.GLFW as GLFW

renderGame :: Window -> State -> Config -> (GameState, GameStatus) -> IO GameState
renderGame win glossState config (gameState, _) =
    let
        w = width config
        h = height config
        dt = diffTime gameState
        drawObj :: [(ObjectName, PolyObject)] -> ([(ObjectName, PolyObject)], [Picture])
        drawObj ((name, PolyObject x):xs) =
            let (xpos, ypos) = objPos x
                render = objRender x
                rot :: Double
                rot = case objRot x of
                        Degree d -> d
                        Radian r -> (r / pi) * 180
                newObj = nextFrame x dt
                (rObj, rPic) = drawObj xs
            in
                ( (name, PolyObject newObj):rObj, (rotate (double2Float rot) $
                                  translate (double2Float xpos) (double2Float ypos) $
                                            toPicture render) : rPic)
        drawObj ((name, PolyObjects y):xs) = let (pair1, pict1) = drawObj $ map (\x -> (name, x)) y
                                                 (pair2, pict2) = drawObj xs
                                             in ((name, PolyObjects $ map snd pair1):pair2, pict1 <> pict2)
        drawObj [] = ([], [])
    in
      do
        let renderObjs = drawObj $ M.toList $ objs gameState
            incrementCount = case debugInfo gameState of
                               Just a@(GameDebugInfo {updateCount = c}) -> Just $ a { updateCount = c + 1 }
                               Nothing -> Nothing
        displayPicture (w, h) white glossState 1.0 $ Pictures $ snd renderObjs
        swapBuffers win
        return $ gameState { objs = M.fromList $ fst renderObjs, debugInfo = incrementCount }


