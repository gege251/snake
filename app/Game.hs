{-# LANGUAGE TemplateHaskell #-}

module Game (newgame) where

import Lib 
import Prelude hiding (Left, Right, Up, Down)
import System.Console.ANSI
import System.IO
import System.Random 
import GHC.Conc
import Data.List
import Control.Monad.State
import Control.Lens
import Control.Exception.Base

type CanvasSize = (Int, Int)

type Position = (Int, Int)

type Apple = Position

data Snake = Snake { _body      :: [Position]
                   , _direction :: Direction 
                   , _size      :: Int }

data GameState = GameState { _snake  :: Snake
                           , _apple  :: Maybe Apple
                           , _points :: Int
                           , _speed  :: Int
                           , _canvas :: CanvasSize }

makeLenses ''Snake
makeLenses ''GameState


moveup :: Position -> CanvasSize -> Position
moveup (x,y) (_, height) = ( x, if 0 < y then y - 1 else height )

movedown :: Position -> CanvasSize -> Position
movedown (x,y) (_, height) = ( x, if y < height then y + 1 else 0 )

moveleft :: Position -> CanvasSize -> Position
moveleft (x,y) (width, _) = ( if 0 < x then x - 1 else width, y )

moveright :: Position -> CanvasSize -> Position
moveright (x,y) (width, _) = ( if x < width then x + 1 else 0, y )

genApple :: StateT GameState IO ()
genApple  = do
  appear <- randomnum (0, 5)
  if appear == 0
  then do
    newapple  <- genApple'
    apple .= Just newapple
  else
    return ()

  where randomnum range = liftIO $ (getStdRandom (randomR range) :: IO Int)
        genApple' = do
          (width, height) <- use canvas
          x <- randomnum (0, width)
          y <- randomnum (0, height)
          snkBody <- use $ snake.body
          let newapple = (x, y)
          if newapple `elem` snkBody
          then genApple'
          else return newapple


newgame :: IO ()
newgame = do
  evalStateT control $ GameState 
    { _snake  = Snake [(0, 5)] Right 4
    , _apple  = Nothing
    , _points = 0
    , _speed  = 1
    , _canvas = (30, 30) }


control :: StateT GameState IO ()
control = do
  str <- liftIO $ getLineNB

  case str of
    "q" -> liftIO $ putStrLn "Exit"
    _   -> case getDirection str of
              Just dir -> snake.direction .= dir >> gameloop
              _ -> gameloop

  where gameloop = step >> eval 


step :: StateT GameState IO ()
step = do
  canv    <- use canvas
  snk     <- use snake

  let snkPos = head $ snk^.body
  let newhead = case snk^.direction of
                    Up    -> moveup    snkPos canv
                    Down  -> movedown  snkPos canv
                    Left  -> moveleft  snkPos canv
                    Right -> moveright snkPos canv

  snake.body %= (\newtail -> take (snk^.size) (newhead : newtail) )


eval :: StateT GameState IO ()
eval = do
  apl     <- use apple
  snkBody <- use $ snake.body

  if (head snkBody) `elem` (tail snkBody)
  then gameOver
  else do
    case apl of
      Nothing -> genApple
      Just apl'  -> do
        if apl' == (head snkBody)
        then do
          snake.size += 1
          points     += 1
          speed      += 1
          apple      .= Nothing
        else return ()
    render >> control


render :: StateT GameState IO ()
render = do
  snkBody <- use $ snake.body
  apple   <- use apple
  points  <- use points
  speed   <- use speed
  (width, height) <- use canvas

--   let scFrame = 
--         [ ScObject '┌' [(0, 0)]
--         , ScObject '┐' [(width * 2 + 3, 0)]
--         , ScObject '└' [(0, height + 2)]
--         , ScObject '┘' [(width * 2 + 3, height + 2)]
--         , ScObject '─' [ (x, 0)             | x <- [1..(width * 2 + 2)] ]
--         , ScObject '─' [ (x, height + 2)    | x <- [1..(width * 2 + 2)] ]
--         , ScObject '│' [ (0, y)             | y <- [1..(height + 1)] ]
--         , ScObject '│' [ (width * 2 + 3, y) | y <- [1..(height + 1)] ]
--         ]

  let scFrame = mkFrame (0, 0) (width * 2 + 3, height + 2)
  let scSnake = [ prepCanv $ ScObject 'x' snkBody ]
  let scApple = [ prepCanv $ ScObject 'o' $ case apple of
                                Just a  -> [a]
                                Nothing -> [] ]

  let scText = 
        toGroup (1, height + 3) ("Points : " ++ (show points))
        ++ toGroup (1, height + 4) "Control: arrows"
        ++ toGroup (1, height + 5) "Quit   : Q"
        

  let screen = mkScreen $ [ scFrame, scSnake, scApple, scText ]

  liftIO $ do
    setCursorPosition 0 0
    renderScreen (width * 2 + 3, height + 5) screen 
    threadDelay (150000 - speed * 3000)

  where prepCanv = stretch . relocate (1,1)
  

gameOver :: StateT GameState IO ()
gameOver = do
  (width, height) <- use canvas
  let scFrame = mkFrame (0, 0) (width * 2 + 3, height + 2)
  let scText = toGroup (width - 4, height `div` 2) "GAME OVER"
  liftIO $ renderScreen (width * 2 + 3, height + 2) $ mkScreen [scFrame, scText]
