{-# LANGUAGE OverloadedStrings #-}
module Main where

import Control.Monad (unless)
import Control.Concurrent (threadDelay)

import Data.Set (Set)
import qualified Data.Set as Set

import Data.List (foldl')

import Foreign.C.Types (CInt (..) )

import SDL
import SDL.Time (time, delay)
import Linear (V4(..))

import TextureMap (TextureMap, TextureId (..))
import qualified TextureMap as TM

import Sprite (Sprite)
import qualified Sprite as S

import SpriteMap (SpriteMap, SpriteId (..))
import qualified SpriteMap as SM

import Keyboard (Keyboard)
import qualified Keyboard as K

import qualified Debug.Trace as T

import Model (GameState)
import qualified Model as M

import SDL.Input.Mouse
import qualified SDL.Input.Mouse as MO

import System.Random as R

import Control.Monad (when)


getMouseLocation :: Event -> Maybe (V2 Int)
getMouseLocation event =
    case eventPayload event of
        MouseButtonEvent mouseButton ->
            let P coordinates = fmap fromIntegral (mouseButtonEventPos mouseButton) in
                Just coordinates
        _ -> Nothing

-- | Verification si un evenement est un clic de souris
isMouseButtonEvent :: SDL.Event -> Bool
isMouseButtonEvent SDL.Event { SDL.eventPayload = SDL.MouseButtonEvent _ } = True
isMouseButtonEvent _ = False

-- | Traitement d'un clic de souris
processMouseEvent :: Int -> Int -> Int -> Int -> SDL.Event -> IO ()
processMouseEvent xMouse yMouse xPerso yPerso (SDL.Event _ (SDL.MouseButtonEvent eventData)) =
  if ((xMouse >= xPerso) && (xMouse <= xPerso+100)) && ((yMouse >= yPerso) && (yMouse <= yPerso+100)) then 
    putStrLn $ "Touche perso avec" ++ show (SDL.mouseButtonEventButton eventData)  
  else putStrLn $ "PAS touche"
  -- putStrLn $ "Mouse button " ++ show (SDL.mouseButtonEventButton eventData) ++ " pressed || X mouse test: " <> (show x) <> " Y mouse: " <> (show y)
processMouseEvent _ _ _ _ _ = return ()

loadBackground :: Renderer-> FilePath -> TextureMap -> SpriteMap -> IO (TextureMap, SpriteMap)
loadBackground rdr path tmap smap = do
  tmap' <- TM.loadTexture rdr path (TextureId "background") tmap
  let sprite = S.defaultScale $ S.addImage S.createEmptySprite $ S.createImage (TextureId "background") (S.mkArea 0 0 640 480)
  let smap' = SM.addSprite (SpriteId "background") sprite smap
  return (tmap', smap')

loadPerso :: Renderer-> FilePath -> TextureMap -> SpriteMap -> IO (TextureMap, SpriteMap)
loadPerso rdr path tmap smap = do
  tmap' <- TM.loadTexture rdr path (TextureId "perso") tmap
  let sprite = S.defaultScale $ S.addImage S.createEmptySprite $ S.createImage (TextureId "perso") (S.mkArea 0 0 100 100)
  let smap' = SM.addSprite (SpriteId "perso") sprite smap
  return (tmap', smap')

loadPerso2 :: Renderer-> FilePath -> TextureMap -> SpriteMap -> IO (TextureMap, SpriteMap)
loadPerso2 rdr path tmap smap = do
  tmap' <- TM.loadTexture rdr path (TextureId "virus") tmap
  let sprite = S.defaultScale $ S.addImage S.createEmptySprite $ S.createImage (TextureId "virus") (S.mkArea 0 0 100 100)
  let smap' = SM.addSprite (SpriteId "virus") sprite smap
  return (tmap', smap')

main :: IO ()
main = do
  initializeAll
  window <- createWindow "Minijeu" $ defaultWindow { windowInitialSize = V2 640 480 }
  renderer <- createRenderer window (-1) defaultRenderer
  -- chargement de l'image du fond
  (tmap, smap) <- loadBackground renderer "assets/background.bmp" TM.createTextureMap SM.createSpriteMap
  -- chargement du personnage 1
  (tmap', smap') <- loadPerso renderer "assets/perso.bmp" tmap smap
   -- chargement du personnage 2
  (tmap2, smap2) <- loadPerso2 renderer "assets/virus.bmp" tmap' smap'
  -- initialisation de l'état du jeu
  let gameState_perso1 = M.initGameState
  virusX <- randomRIO (0, 640-100) :: IO Int
  virusY <- randomRIO (0, 480-100) :: IO Int
  let gameState_perso2 = M.GameState virusX virusY 0
  -- initialisation de l'état du clavier
  let kbd = K.createKeyboard
  -- lancement de la gameLoop
  gameLoop 60 renderer tmap2 smap2 kbd [gameState_perso1, gameState_perso2] 1

gameLoop :: (RealFrac a, Show a) => a -> Renderer -> TextureMap -> SpriteMap -> Keyboard -> [GameState] -> Int -> IO ()
gameLoop frameRate renderer tmap smap kbd [gameState_perso1, gameState_perso2] affichage = do
  startTime <- time
  --- ensemble des events
  events <- pollEvents
  --- events du clavier
  let kbd' = K.handleEvents events kbd
  --- events souris
  let mouseButtonEvents = filter isMouseButtonEvent events

  clear renderer
  
  let isCollision = ((fromIntegral (M.persoX gameState_perso1)) + 50 >= (fromIntegral (M.persoX gameState_perso2)) &&
                   (fromIntegral (M.persoX gameState_perso1)) + 50 <= (fromIntegral (M.persoX gameState_perso2)+100) &&
                   (fromIntegral (M.persoY gameState_perso1)) + 50 >= (fromIntegral (M.persoY gameState_perso2)) &&
                   (fromIntegral (M.persoY gameState_perso1)) + 50 <= (fromIntegral (M.persoY gameState_perso2)+100))

  gameState_perso2 <- if isCollision
            then do
              putStrLn "Collision detected, reomving virus."
              return $ M.GameState (-300) (-300) 0
            else return gameState_perso2

  -- when (((fromIntegral (M.persoX gameState_perso1)) + 50 >= (fromIntegral (M.persoX gameState_perso2)) && (fromIntegral (M.persoX gameState_perso1)) + 50 <= (fromIntegral (M.persoX gameState_perso2)+100))  &&
  --   ((fromIntegral (M.persoY gameState_perso1)) + 50 >= (fromIntegral (M.persoY gameState_perso2)) && (fromIntegral (M.persoY gameState_perso1)) + 50 <= (fromIntegral (M.persoY gameState_perso2)+100))) 
  --   $ do (SM.removeSprite (SpriteId "virus") smap)
    

  --- display background
  S.displaySprite renderer tmap (SM.fetchSprite (SpriteId "background") smap)
  --- display perso 1
  S.displaySprite renderer tmap (S.moveTo (SM.fetchSprite (SpriteId "perso") smap) (fromIntegral (M.persoX gameState_perso1)) (fromIntegral (M.persoY gameState_perso1)))
  --- display perso 2
  S.displaySprite renderer tmap (S.moveTo (SM.fetchSprite (SpriteId "virus") smap) (fromIntegral (M.persoX gameState_perso2)) (fromIntegral (M.persoY gameState_perso2)))

  --- location de la souris
  test <- MO.getAbsoluteMouseLocation
  let SDL.P (SDL.V2 x1 y1) = test
  let mousePos = SDL.P (SDL.V2 x1 y1) 
      x = case mousePos of
            SDL.P (SDL.V2 x _) -> fromIntegral x
      y = case mousePos of
            SDL.P (SDL.V2 _ y) -> fromIntegral y
 
  mapM_ (\event -> processMouseEvent x y (M.persoX gameState_perso1) (M.persoY gameState_perso1) event) mouseButtonEvents
  ---
  -- putStrLn $ "X perso: " <> (show (M.persoX gameState))
  -- putStrLn $ "Y perso: " <> (show (M.persoY gameState))

  ---
  present renderer
  endTime <- time
  let refreshTime = endTime - startTime
  let delayTime = floor (((1.0 / frameRate) - refreshTime) * 1000)
  threadDelay $ delayTime * 1000 -- microseconds
  endTime <- time
  let deltaTime = endTime - startTime
  
  -- location de la souris
  -- test <- MO.getAbsoluteMouseLocation
  -- let SDL.P (SDL.V2 x y) = test
  -- -- putStrLn $ "Location mouse: " <> (show x) <> " " <> (show y)

  -- putStrLn $ "Delta time: " <> (show (deltaTime * 1000)) <> " (ms)"
  -- putStrLn $ "Frame rate: " <> (show (1 / deltaTime)) <> " (frame/s)"
  --- update du game state
  let gameState' = M.gameStep gameState_perso1 kbd' deltaTime
  ---
  -- let (M.GameState pX pY sp) = gameState'
  -- if (pX > x+100) && (pX < x-100) then putStrLn $ "toucheee" else putStrLn $ "non"

    -- if MO.getAbsoluteMouseLocation
  
  unless (K.keypressed KeycodeEscape kbd') (gameLoop frameRate renderer tmap smap kbd' [gameState', gameState_perso2] affichage)
