{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -Wall #-}
module Control.Monad.Update
  ( RightMonoidAction(..)
  , UpdateT(..)
  , update, runUpdate
  , CoupdateT(..)
  , coupdate, runCoupdate
  ) where

import Control.Applicative
import Control.Comonad
import Control.Comonad.Trans.Class
import Control.Monad (liftM)
import Control.Monad.Trans.Class
import Data.Bifunctor
import Data.Coerce
import Data.Functor.Identity
import Data.Monoid

class Monoid m => RightMonoidAction m s where
  act :: s -> m -> s
  default act :: (s ~ m) => s -> m -> s
  act = (<>)

instance RightMonoidAction [a] [a]
instance Monoid a => RightMonoidAction (Maybe a) (Maybe a)
instance RightMonoidAction (Endo s) s where
  act s (Endo f) = f s
instance RightMonoidAction (First a) (First a)
instance RightMonoidAction (Last a) (Last a)

-- | The update monad transformer
newtype UpdateT e s m a = UpdateT { runUpdateT :: s -> m (e, a) }
  deriving Functor

type Update e s = UpdateT e s Identity

update :: (s -> (e, a)) -> Update e s a
update f = UpdateT (coerce f) 

runUpdate :: Update e s a -> s -> (e, a)
runUpdate (UpdateT f) = coerce f

instance (Functor m, Monad m, RightMonoidAction e s) => Applicative (UpdateT e s m) where
  pure a = UpdateT $ \_ -> return (mempty, a)
  UpdateT mf <*> UpdateT ma = UpdateT $ \s -> do
    (m, f) <- mf s
    bimap (mappend m) f <$> ma (act s m)

instance (Monad m, RightMonoidAction e s) => Monad (UpdateT e s m) where
  return a = UpdateT $ \_ -> return (mempty, a)
  UpdateT f >>= k = UpdateT $ \s -> do
    (m, a) <- f s
    (n, b) <- runUpdateT (k a) (act s m)
    return (mappend m n, b)

instance RightMonoidAction e s => MonadTrans (UpdateT e s) where
  lift m = UpdateT $ \_ -> (,) mempty `liftM` m

-- | The coupdate comonad transformer
data CoupdateT e s w a = CoupdateT (w (e -> a)) s
  deriving Functor

type Coupdate e s = CoupdateT e s Identity

coupdate :: (e -> a) -> s -> Coupdate e s a
coupdate f = CoupdateT (coerce f)

runCoupdate :: Coupdate e s a -> (e -> a, s)
runCoupdate (CoupdateT (Identity f) s) = (f, s)

instance (Comonad w, RightMonoidAction e s) => Comonad (CoupdateT e s w) where
  extract (CoupdateT wf _) = extract wf mempty
  duplicate (CoupdateT wf s) = CoupdateT (extend (\we -> CoupdateT we . act s) wf) s 
  extend k (CoupdateT wf s) = CoupdateT (extend (\wea -> k . CoupdateT wea . act s) wf) s 

instance RightMonoidAction e s => ComonadTrans (CoupdateT e s) where
  lower (CoupdateT wf _) = fmap ($ mempty) wf
