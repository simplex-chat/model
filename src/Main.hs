module Main where

import Control.Monad.State
import qualified Data.Map as Map
import SimplexModelOld

main :: IO ()
main = do
  let model = SimplexModel Map.empty Map.empty
  let (_, model') = (runState scenario) model
  print model'
  return ()
