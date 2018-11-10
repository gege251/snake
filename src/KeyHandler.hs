module KeyHandler
    ( Direction(..)
    , readKeypress
    , getLineNB
    , getDirection
    ) where

import Prelude hiding (Up, Down, Left, Right)
import System.IO
import Render 

data Direction = Up | Down | Left | Right deriving Show

readKeypress :: IO (Maybe Char)
readKeypress = hReady stdin >>= f
   where f True = getChar >>= return . Just
         f _    = return Nothing


getLineNB :: IO String
getLineNB = do
  isReady <- hReady stdin
  if isReady then do
    ch <- getChar
    rest <- getLineNB
    return (ch : rest)
  else
    return []


getDirection :: String -> Maybe Direction
getDirection str = case str of
                    "\ESC[A" -> Just Up
                    "\ESC[B" -> Just Down
                    "\ESC[C" -> Just Right
                    "\ESC[D" -> Just Left
                    _         -> Nothing

