module Main
  ( main
  )
  where

-- doctest
import Test.DocTest


main :: IO ()
main =
  doctest ["-isrc", "src/Workdays.hs"]
