module HsBowling.Util where

import Control.Lens.Operators
import Control.Monad.Reader

returnError :: error -> ReaderT r (Either error) a
returnError error = ReaderT $ \r -> Left error

getResult :: MonadReader r m1 => ReaderT r m2 a -> m1 (m2 a)
getResult reader = ask <&> runReaderT reader
