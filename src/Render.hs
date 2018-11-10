module Render
  ( mkScreen
  , renderScreen
  , ScObject(..)
  , mkFrame
  , stretch
  , relocate
  , toGroup
  ) where

import Data.List
import Data.Ord
import Data.Tuple
import System.Console.ANSI

type CanvSize = (Int, Int)

type Screen   = [Point]

type Position = (Int, Int)

class Coordinate a where
  overlapping    :: a -> a -> Bool
  hasOverlapping :: a -> [a] -> Bool
  findAt         :: Position -> [a] -> Maybe a

data Point = Point
  { char       :: Char
  , coordinate :: Position }

instance Coordinate Point where
  overlapping a b = coordinate a == coordinate b
  hasOverlapping a bs = any (\b -> coordinate a == coordinate b) bs
  findAt a bs = find (\b -> a == coordinate b) bs

data ScObject = ScObject 
  { chars       :: Char 
  , coordinates :: [Position] }
  deriving Show

type ScGroup = [ScObject]


toGroup :: Position -> String -> ScGroup
toGroup _ [] = []
toGroup (x, y) (s:ss) = ScObject s [(x, y)] : toGroup (x + 1, y) ss


stretch :: ScObject -> ScObject
stretch = stretchBy 2


stretchBy :: Int -> ScObject -> ScObject
stretchBy ratio obj = obj { coordinates = map (\(a,b) -> (ratio*a,b)) $ coordinates obj }


relocate :: Position -> ScObject -> ScObject
relocate (x, y) obj = obj { coordinates = map (\(a,b) -> (a + x, b + y)) $ coordinates obj }


mkScreen :: [ScGroup] -> Screen
mkScreen objs = nubBy overlapping flattened
  where mkPoints obj = map (Point $ chars obj) (coordinates obj)
        flattened = (concat. reverse) $ map mkPoints $ foldr (++) [] objs
 
mkFrame :: Position -> Position -> ScGroup
mkFrame (x1, y1) (x2, y2) = 
        [ ScObject '┌' [ (x1, y1) ]
        , ScObject '┐' [ (x2, y1) ]
        , ScObject '└' [ (x1, y2) ]
        , ScObject '┘' [ (x2, y2) ]
        , ScObject '─' [ (x, y1) | x <- [1..(x2 - 1)] ]
        , ScObject '─' [ (x, y2) | x <- [1..(x2 - 1)] ]
        , ScObject '│' [ (x1, y) | y <- [1..(y2 - 1)] ]
        , ScObject '│' [ (x2, y) | y <- [1..(y2 - 1)] ]
        ]
 
renderScreen :: CanvSize -> Screen -> IO ()
renderScreen (width, height) screen = do
  setCursorPosition 0 0
  (renderPoints . sortByY) screen (0, 0)

  where sortByY :: Screen -> Screen
        sortByY = sortBy (comparing (swap . coordinate))
  
        renderPoints :: [Point] -> Position -> IO ()
        renderPoints [] (x, y)
          | height < y = return ()
          | width  < x = nextLn y >> renderPoints [] (0, y + 1)
          | otherwise  = do
              putStrLn (replicate (width - x + 1) ' ') 
              nextLn y
              renderPoints [] (0, y + 1)

        renderPoints pps@(p:ps) (x, y)
          | height < y             = return ()
          | width  < x             = nextLn y >> renderPoints pps (0, y + 1)
          | coordinate p == (x, y) = putStr [char p] >> renderPoints ps (x + 1, y)
          | otherwise              = putStr " " >> renderPoints pps (x + 1, y)

        nextLn ln = putStrLn ""
