{-# LANGUAGE TemplateHaskell #-}

module Main where

import Lib 
import System.Console.ANSI
import System.IO
import Control.Exception.Base
import Game

main :: IO ()
main = finally
    (do 
      clearScreen
      hideCursor
      hSetEcho stdin False
      hSetBuffering stdin NoBuffering
      newgame)
    (showCursor)
