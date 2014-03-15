{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Revelation.Core where
import Control.Monad.IO.Class
import Control.Monad.Trans.Class
import Control.Applicative

newtype CV a = CV { runCV :: IO a } deriving (Monad, Functor, MonadIO, Applicative)

{-# INLINABLE liftCV #-}
liftCV :: MonadTrans m => IO a -> m CV a
liftCV ma = lift . CV $ ma
