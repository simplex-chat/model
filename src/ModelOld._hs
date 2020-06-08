{-# LANGUAGE RankNTypes #-}

module ModelOld
( Model
, addItem
, getItem
, MutableModel
, updateItem
, itemAction
) where

import Control.Lens
import Control.Monad.State
import qualified Data.Map as M


class Model s where
  addItem :: Ord k => Lens' s (M.Map k v) -> k -> v -> State s ()
  addItem prop key v = do
    modelState <- get
    let vs = view prop modelState
    if M.member key vs
      then error "item already exists"
      else put $ set prop (M.insert key v vs) modelState

  -- hasItem :: Ord k => Lens' s (M.Map k v) -> k -> State s Bool
  -- hasItem prop key = do
  --   modelState <- get
  --   let vs = view prop modelState
  --   return $ M.member key vs

  getItem :: Ord k => Lens' s (M.Map k v) -> k -> State s (Maybe v)
  getItem prop key = do
    modelState <- get
    let vs = view prop modelState
    return $ M.lookup key vs


class Model s => MutableModel s where
  -- deleteItem :: Ord k => Lens' s (M.Map k v) -> k -> State s ()
  -- deleteItem prop key = do
  --   modelState <- get
  --   let vs = view prop modelState
  --   put $ set prop (M.delete key vs) modelState

  updateItem :: Ord k => Lens' s (M.Map k v) -> k -> v -> State s ()
  updateItem prop key v = do
    modelState <- get
    let vs = view prop modelState
    if M.member key vs
      then put $ set prop (M.insert key v vs) modelState
      else error "item does not exist"

  itemAction :: Ord k => Lens' s (M.Map k v) -> k -> State v a -> State s a
  itemAction prop key action = do
    -- Just v <- getItem prop key
    -- let (result, v') = (runState action) v
    -- updateItem prop key v'
    -- return result
    maybeItem <- getItem prop key
    case maybeItem of
      Just v -> do
        let (result, v') = (runState action) v
        updateItem prop key v'
        return result
      Nothing -> error "item does not exist"
