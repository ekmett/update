{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -Wall #-}
-- | Based on <http://citeseerx.ist.psu.edu/viewdoc/download;jsessionid=B5415A02F7DD5AD7665B59E390E70BCD?doi=10.1.1.358.1211&rep=rep1&type=pdf "Update monads: Cointerpreting Directed Containers"> by Danel Ahman and Tarmo Uustalu
module Control.Monad.Update
  ( MonoidAction(..)
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

class Monoid e => MonoidAction s e where
  act :: s -> e -> s
  default act :: (s ~ e) => s -> e -> s
  act = (<>)

instance MonoidAction [a] [a]
instance Monoid a => MonoidAction (Maybe a) (Maybe a)
instance MonoidAction s (Endo s) where
  act s (Endo f) = f s
instance MonoidAction (First a) (First a)
instance MonoidAction (Last a) (Last a)

-- | The update monad transformer
newtype UpdateT s e m a = UpdateT { runUpdateT :: s -> m (e, a) }
  deriving Functor

type Update s e = UpdateT s e Identity

update :: (s -> (e, a)) -> Update s e a
update f = UpdateT (coerce f) 

runUpdate :: Update s e a -> s -> (e, a)
runUpdate (UpdateT f) = coerce f

instance (Functor m, Monad m, MonoidAction s e) => Applicative (UpdateT s e m) where
  pure a = UpdateT $ \_ -> return (mempty, a)
  UpdateT mf <*> UpdateT ma = UpdateT $ \s -> do
    (m, f) <- mf s
    bimap (mappend m) f <$> ma (act s m)

instance (Monad m, MonoidAction s e) => Monad (UpdateT s e m) where
  return a = UpdateT $ \_ -> return (mempty, a)
  UpdateT f >>= k = UpdateT $ \s -> do
    (m, a) <- f s
    (n, b) <- runUpdateT (k a) (act s m)
    return (mappend m n, b)

instance MonoidAction s e => MonadTrans (UpdateT s e) where
  lift m = UpdateT $ \_ -> (,) mempty `liftM` m

-- | The coupdate comonad transformer
data CoupdateT s e w a = CoupdateT (w (e -> a)) s
  deriving Functor

type Coupdate s e = CoupdateT s e Identity

coupdate :: (e -> a) -> s -> Coupdate s e a
coupdate f = CoupdateT (coerce f)

runCoupdate :: Coupdate s e a -> (e -> a, s)
runCoupdate (CoupdateT (Identity f) s) = (f, s)

instance (Comonad w, MonoidAction s e) => Comonad (CoupdateT s e w) where
  extract (CoupdateT wf _) = extract wf mempty
  duplicate (CoupdateT wf s) = CoupdateT (extend (\we -> CoupdateT we . act s) wf) s 
  extend k (CoupdateT wf s) = CoupdateT (extend (\wea -> k . CoupdateT wea . act s) wf) s 

instance MonoidAction s e => ComonadTrans (CoupdateT s e) where
  lower (CoupdateT wf _) = fmap ($ mempty) wf
