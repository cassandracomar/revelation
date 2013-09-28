{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Revelation.Core where
import Control.Monad.IO.Class
import Control.Monad.Trans.Class

newtype CV a = CV { runCV :: IO a } deriving (Monad, Functor, MonadIO)

{-# INLINABLE liftCV #-}
liftCV :: MonadTrans m => IO a -> m CV a
liftCV ma = lift . CV $ ma
