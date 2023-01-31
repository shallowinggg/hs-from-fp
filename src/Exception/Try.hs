module Exception.Try where

import Control.Exception (Exception, throwIO, try)

canICatch :: (Exception e1, Exception e2) => e2 -> IO (Either e1 a)
canICatch e = try $ throwIO e
