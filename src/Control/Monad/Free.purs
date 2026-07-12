module Control.Monad.Free
  ( Free
  , suspendF
  , wrap
  , liftF
  , hoistFree
  , foldFree
  , substFree
  , runFree
  , runFreeM
  , resume
  , resumePrime
  , resume'
  , bindImpl
  , BindNodeClass
  , BindLeafClass
  , FreeObjClass
  , bindNodeClass
  , bindLeafClass
  , freeObjClass
  ) where

import Prelude

import Control.Apply (lift2)
import Control.Monad.Rec.Class (class MonadRec, Step(..), tailRecM)
import Control.Monad.Trans.Class (class MonadTrans)

import Data.Either (Either(..))
import Data.Eq (class Eq1, eq1)
import Data.Foldable (class Foldable, foldMap, foldl, foldr)
import Data.Ord (class Ord1, compare1)
import Data.Traversable (class Traversable, traverse)

foreign import data Free :: (Type -> Type) -> Type -> Type
type role Free representational representational

foreign import pureImpl :: forall f a. a -> Free f a
foreign import bindImpl :: forall f a b. Free f a -> (a -> Free f b) -> Free f b
foreign import liftF :: forall f a. f a -> Free f a

foreign import resumePrime :: forall f a r. (forall b. f b -> (b -> Free f a) -> r) -> (a -> r) -> Free f a -> r

resume' :: forall f a r. (forall b. f b -> (b -> Free f a) -> r) -> (a -> r) -> Free f a -> r
resume' = resumePrime

instance functorFree :: Functor (Free f) where
  map k f = bindImpl f (pureImpl <<< k)

instance applyFree :: Apply (Free f) where
  apply = ap

instance applicativeFree :: Applicative (Free f) where
  pure = pureImpl

instance bindFree :: Bind (Free f) where
  bind = bindImpl

instance monadFree :: Monad (Free f)

instance monadTransFree :: MonadTrans Free where
  lift = liftF

instance monadRecFree :: MonadRec (Free f) where
  tailRecM k a = bindImpl (k a) \res -> case res of
    Loop b -> tailRecM k b
    Done r -> pureImpl r

resume :: forall f a. Functor f => Free f a -> Either (f (Free f a)) a
resume = resumePrime (\g i -> Left (map i g)) Right

wrap :: forall f a. f (Free f a) -> Free f a
wrap f = liftF f >>= identity

suspendF :: forall f. Applicative f => Free f ~> Free f
suspendF f = wrap (pure f)

-- We implement the rest in PureScript using resumePrime
substFree :: forall f g. (f ~> Free g) -> Free f ~> Free g
substFree k = go
  where
  go :: Free f ~> Free g
  go f = resumePrime (\g i -> bindImpl (k g) (\x -> go (i x))) pureImpl f

hoistFree :: forall f g. (f ~> g) -> Free f ~> Free g
hoistFree k = substFree (\fa -> liftF (k fa))

foldFree :: forall f m. MonadRec m => (f ~> m) -> Free f ~> m
foldFree k = tailRecM go
  where
  go :: forall a. Free f a -> m (Step (Free f a) a)
  go f = resumePrime (\g i -> map (\x -> Loop (i x)) (k g)) (\a -> pure (Done a)) f

runFree :: forall f a. Functor f => (f (Free f a) -> Free f a) -> Free f a -> a
runFree k = go
  where
  go :: Free f a -> a
  go f = resumePrime (\g i -> go (k (map i g))) identity f

runFreeM :: forall f m a. Functor f => MonadRec m => (f (Free f a) -> m (Free f a)) -> Free f a -> m a
runFreeM k = tailRecM go
  where
  go :: Free f a -> m (Step (Free f a) a)
  go f = resumePrime (\g i -> map Loop (k (map i g))) (\a -> pure (Done a)) f

instance eqFree :: (Functor f, Eq1 f, Eq a) => Eq (Free f a) where
  eq x y = case resume x, resume y of
    Left fa, Left fb -> eq1 fa fb
    Right a, Right b -> a == b
    _, _ -> false

instance eq1Free :: (Functor f, Eq1 f) => Eq1 (Free f) where
  eq1 = eq

instance ordFree :: (Functor f, Ord1 f, Ord a) => Ord (Free f a) where
  compare x y = case resume x, resume y of
    Left fa, Left fb -> compare1 fa fb
    Left _, _ -> LT
    _, Left _ -> GT
    Right a, Right b -> compare a b

instance ord1Free :: (Functor f, Ord1 f) => Ord1 (Free f) where
  compare1 = compare

instance foldableFree :: (Functor f, Foldable f) => Foldable (Free f) where
  foldMap f = go
    where
    go = resume >>> case _ of
      Left fa -> foldMap go fa
      Right a -> f a
  foldl f = go
    where
    go r = resume >>> case _  of
      Left fa -> foldl go r fa
      Right a -> f r a
  foldr f = go
    where
    go r = resume >>> case _ of
      Left fa -> foldr (flip go) r fa
      Right a -> f a r

instance traversableFree :: Traversable f => Traversable (Free f) where
  traverse f = go
    where
    go = resume >>> case _ of
      Left fa -> join <<< liftF <$> traverse go fa
      Right a -> pure <$> f a
  sequence tma = traverse identity tma

instance semigroupFree :: Semigroup a => Semigroup (Free f a) where
  append = lift2 append

instance monoidFree :: Monoid a => Monoid (Free f a) where
  mempty = pure mempty
 

foreign import data BindNodeClass :: Type
foreign import data BindLeafClass :: Type
foreign import data FreeObjClass :: Type

foreign import bindNodeClass :: BindNodeClass
foreign import bindLeafClass :: BindLeafClass
foreign import freeObjClass :: FreeObjClass
