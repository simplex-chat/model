module Model
( Model
, addItem
, getItem
, MutableModel
, updateItem
, itemAction
) where

import Control.Lens
import Control.Monad.State
import Data.Function
import qualified Data.Map as M


class Model s where
  addItem :: Ord k => Lens' s (M.Map k v) -> k -> v -> State s ()
  addItem lens key v = do
    modelState <- get
    let vs = view lens modelState
    if M.member key vs
      then error "item already exists"
      else put $ set lens (M.insert key v vs) modelState

  hasItem :: Ord k => Lens' s (M.Map k v) -> k -> State s Bool
  hasItem lens key = do
    modelState <- get
    let vs = view lens modelState
    return $ M.member key vs

  getItem :: Ord k => Lens' s (M.Map k v) -> k -> State s (Maybe v)
  getItem lens key = do
    modelState <- get
    let vs = view lens modelState
    return $ M.lookup key vs


class Model s => MutableModel s where
  deleteItem :: Ord k => Lens' s (M.Map k v) -> k -> State s ()
  deleteItem lens key = do
    modelState <- get
    let vs = view lens modelState
    put $ set lens (M.delete key vs) modelState

  updateItem :: Ord k => Lens' s (M.Map k v) -> k -> v -> State s ()
  updateItem lens key v = do
    modelState <- get
    let vs = view lens modelState
    if M.member key vs
      then put $ set lens (M.insert key v vs) modelState
      else error "item does not exist"

  itemAction :: Ord k => Lens' s (M.Map k v) -> k -> State v a -> State s a
  itemAction lens key action = do
    -- Just v <- getItem lens key
    -- let (result, v') = (runState action) v
    -- updateItem lens key v'
    -- return result
    maybeItem <- getItem lens key
    case maybeItem of
      Just v -> do
        let (result, v') = (runState action) v
        updateItem lens key v'
        return result
      Nothing -> error "item does not exist"
