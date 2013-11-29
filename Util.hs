module Util where

import Control.Monad ((>=>), MonadPlus(..))

manyM :: (Monad m, MonadPlus m) => (a -> m a) -> a -> m a
manyM m x = (m >=> manyM m) x `mplus` return x
