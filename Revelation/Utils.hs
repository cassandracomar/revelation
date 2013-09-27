module Revelation.Utils where
import Revelation.Bindings.RawTypes
import Pipes

liftCV :: MonadTrans m => IO a -> m CV a
liftCV ma = lift . CV $ ma
