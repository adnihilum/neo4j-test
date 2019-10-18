module IOHelpers where

import Control.Monad.Except

print_ :: (MonadIO m, Show a) => a -> m ()
print_ = liftIO . print

catchIOError ::
     forall e m. (MonadError e m, MonadIO m, Show e)
  => m ()
  -> m ()
catchIOError ma = catchError ma printError
  where
    printError :: e -> m ()
    printError err = liftIO (putStr "IO exception: " >> print err)
