module Monad.Bind where

import GHC.Base (join)

bind :: Monad m => (a -> m b) -> m a -> m b
bind f a = join $ fmap f a
