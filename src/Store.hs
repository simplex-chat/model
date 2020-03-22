{-# LANGUAGE MultiParamTypeClasses #-}

module Store
  ( Store(..)
  , MutableStore(..)
  , MonadStore(..)
  ) where
-- Store defines monadic stateful storage that has:
-- - multiple properties of type p (e.g. String, accessor function or Lens, depending on implementation)
-- - in each property it has key-value store with keys of type k and values of type v

class Monad m => Store m p where
  addItem :: Ord k => p -> k -> v -> m ()
  hasItem :: Ord k => p -> k -> m Bool
  getItem :: Ord k => p -> k -> m (Maybe v)

class Store m p => MutableStore m p where
  deleteItem :: Ord k => p -> k -> m ()
  updateItem :: Ord k => p -> k -> v -> m ()

class MutableStore m p => MonadStore m p where
  itemAction :: Ord k => p -> k -> m1 a -> m a
