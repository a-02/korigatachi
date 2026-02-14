{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE InstanceSigs #-}

module Korigatachi.Monad where

-- import Prelude hiding (Monad(..))
-- import qualified Prelude as P

-- | Flipped version of <$>.
(<&>) :: Functor f => f a -> (a -> b) -> f b
(<&>) = flip fmap

-- | A monad transformer adding an environment of type @r@,
-- collecting state between types @i@ and @j@, and writing to an
-- output of type @w@.
--
-- This is the value level version of RWIT.
-- For a version that carries its state at the type level only,
-- see RWIPT.
newtype RWIT r w m i j a = RWIT { runRWIT :: r -> i -> m (a, j, w) }

instance (Functor m) => Functor (RWIT r w m i j) where
  fmap :: (a -> b) -> RWIT r w m i j a -> RWIT r w m i j b
  fmap f (RWIT k) = RWIT $ \r i -> k r i <&> \case (a, j, w) -> (f a, j, w)

class (forall i j. Functor (f i j)) => IndexedApplicative f where
  ixpure :: a -> f i i a
  ixap :: f i j (a -> b) -> f j k a -> f i k b

(<*>) :: IndexedApplicative f => f i j (a -> b) -> f j k a -> f i k b
(<*>) = ixap

instance (Monoid w, Monad f) => IndexedApplicative (RWIT r w f) where
  ixpure a = RWIT $ \_ i -> pure (a, i, mempty)
  ixap fijab fjka = ixbind fijab $ \ab -> ixbind fjka $ \a -> ixpure (ab a)

class IndexedMonad m where
  ixbind :: m i j a -> (a -> m j k b) -> m i k b

(>>=) :: IndexedMonad m => m i j a -> (a -> m j k b) -> m i k b
(>>=) = ixbind

instance (Monad m, Monoid w) => IndexedMonad (RWIT r w m) where
  ixbind m f =
    RWIT $ \r i -> do
      ~(a, j, w) <- runRWIT m r i
      ~(b, k, w') <- runRWIT (f a) r j
      pure (b, k, w <> w')

-- | A monad transformer adding an environment of type @r@,
-- collecting state between phantom types @i@ and @j@,
-- and writing to an output of type @w@.
--
-- This is the type level version of RWIT.
newtype RWIPT r w m i j a = RWIPT { runRWIPT :: r -> m (a, w) }

instance (Functor m) => Functor (RWIPT r w m i j) where
  fmap :: (a -> b) -> RWIPT r w m i j a -> RWIPT r w m i j b
  fmap f (RWIPT k) = RWIPT (fmap (\case (a, w) -> (f a, w)) . k)

instance (Monoid w, Monad f) => IndexedApplicative (RWIPT r w f) where
  ixpure :: a -> RWIPT r w f i j a
  ixpure a = RWIPT $ \_ -> pure (a, mempty)
  ixap ::
    forall kind (i :: kind) (j :: kind) (k :: kind) a b.
    RWIPT r w f i j (a -> b) ->
    RWIPT r w f j k a ->
    RWIPT r w f i k b
  ixap fijab fjka = ixbind fijab $ \ab -> ixbind fjka $ \a -> ixpure (ab a)

instance (Monad m, Monoid w) => IndexedMonad (RWIPT r w m) where
  ixbind m f =
    RWIPT $ \r -> do
      ~(a, v) <- runRWIPT m r
      ~(b, w) <- runRWIPT (f a) r
      pure (b, v <> w)



