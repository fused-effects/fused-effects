{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
-- | Labelled effects, allowing flexible disambiguation and dependency of parametric effects.
--
-- Among other things, this can be used to:
--
-- * Improve inference by relating parametric effect types to some arbitrary label. This can be used to lift existing effect operations, or to define new ones; cf "Control.Effect.Reader.Labelled", "Control.Effect.State.Labelled" for examples of lifting effect operations into labelled effect operations.
--
-- * Express stronger relationships between an effect and the context it’s run in, e.g. to give an effect shadowing semantics, allowing only one instance of it to be active at a time in a given context.
--
-- * Resolve ambiguous types by relating parameters to a concrete label type.
--
-- @since 1.0.2.0
module Control.Effect.Labelled
( runLabelled
, Labelled(Labelled)
, LabelledMember(..)
, HasLabelled
, sendLabelled
, runUnderLabel
, UnderLabel(UnderLabel)

, HasLabelledLift
, runLabelledLift
, LabelledLift
, sendM

, module Control.Algebra
) where

import Control.Algebra
import Control.Applicative (Alternative)
import Control.Effect.Sum (reassociateSumL)
import Control.Monad (MonadPlus)
import Control.Monad.Fail as Fail
import Control.Monad.Fix
import Control.Monad.IO.Class
import Control.Monad.Trans.Class
import Data.Functor.Identity
import Data.Kind (Type)
import Control.Effect.Lift (Lift)
import qualified Control.Effect.Lift as Lift

-- | An effect transformer turning effects into labelled effects, and a carrier transformer turning carriers into labelled carriers for the same (labelled) effects.
--
-- @since 1.0.2.0
newtype Labelled (label :: k) (sub :: (Type -> Type) -> (Type -> Type)) m a = Labelled (sub m a)
  deriving
    ( Alternative
    , Applicative
    , Functor
    , Monad
    , Fail.MonadFail
    , MonadFix -- ^ @since 1.1.1
    , MonadIO
    , MonadPlus
    , MonadTrans
    )

-- | @since 1.0.2.0
runLabelled :: forall label sub m a . Labelled label sub m a -> sub m a
runLabelled (Labelled l) = l
{-# INLINE runLabelled #-}

instance Algebra (eff :+: sig) (sub m) => Algebra (Labelled label eff :+: sig) (Labelled label sub m) where
  alg hdl = \case
    L eff -> Labelled . alg (runLabelled . hdl) (L (runLabelled eff))
    R sig -> Labelled . alg (runLabelled . hdl) (R sig)
  {-# INLINE alg #-}

-- | A carrier transformer turning carriers into labelled carriers for the @Labelled Lift (Lift n)@ effects.
--
-- @since 1.1.2.2
newtype LabelledLift (label :: k) m a = LabelledLift (m a)
  deriving
    ( Alternative
    , Applicative
    , Functor
    , Monad
    , Fail.MonadFail
    , MonadFix
    , MonadIO
    , MonadPlus
    )

--  @since 1.1.2.2
runLabelledLift :: forall m a. LabelledLift Lift m a -> m a
runLabelledLift (LabelledLift l) = l
{-# INLINE runLabelledLift #-}

instance Algebra (Lift n) n => Algebra (Labelled Lift (Lift n)) (LabelledLift Lift n) where
    alg hdl (Labelled sub) = LabelledLift . alg (runLabelledLift . hdl) sub
    {-# INLINE alg #-}

-- | Given a @HasLabelledLift n sig m@ constraint in a signature carried by @m@, 'sendM'
-- promotes arbitrary actions of type @n a@ to @m a@. It is spiritually
-- similar to @lift@ from the @MonadTrans@ typeclass.
-- @since 1.1.2.2
sendM
    :: forall a n m sig
     . (HasLabelled Lift (Lift n) sig m, Monad n)
    => n a
    -> m a
sendM n = runUnderLabel @Lift (Lift.sendM @n n)
{-# INLINE sendM #-}

-- | @m@ is a carrier for @sig@ containing @Lift n@ associated with label @Lift@.
--
-- Note that if @eff@ is a sum, it will /not/ be decomposed into multiple 'LabelledMember' constraints. While this technically is possible, it results in unsolvable constraints, as the functional dependencies in 'Labelled' prevent assocating the same label with multiple distinct effects within a signature.
--
-- @since 1.1.2.2
type HasLabelledLift n sig m = (LabelledMember Lift (Lift n) sig, Algebra sig m)

-- | The class of labelled types present in a signature.
--
-- @since 1.0.2.0
class LabelledMember label (sub :: (Type -> Type) -> (Type -> Type)) sup | label sup -> sub where
  -- | Inject a member of a signature into the signature.
  --
  -- @since 1.0.2.0
  injLabelled :: Labelled label sub m a -> sup m a

-- | Reflexivity: @t@ is a member of itself.
instance LabelledMember label t (Labelled label t) where
  injLabelled = id
  {-# INLINE injLabelled #-}

-- | Left-recursion: if @t@ is a member of @l1 ':+:' l2 ':+:' r@, then we can inject it into @(l1 ':+:' l2) ':+:' r@ by injection into a right-recursive signature, followed by left-association.
instance {-# OVERLAPPABLE #-}
         LabelledMember label t (l1 :+: l2 :+: r)
      => LabelledMember label t ((l1 :+: l2) :+: r) where
  injLabelled = reassociateSumL . injLabelled
  {-# INLINE injLabelled #-}

-- | Left-occurrence: if @t@ is at the head of a signature, we can inject it in O(1).
instance {-# OVERLAPPABLE #-}
         LabelledMember label l (Labelled label l :+: r) where
  injLabelled = L
  {-# INLINE injLabelled #-}

-- | Right-recursion: if @t@ is a member of @r@, we can inject it into @r@ in O(n), followed by lifting that into @l ':+:' r@ in O(1).
instance {-# OVERLAPPABLE #-}
         LabelledMember label l r
      => LabelledMember label l (l' :+: r) where
  injLabelled = R . injLabelled
  {-# INLINE injLabelled #-}


-- | @m@ is a carrier for @sig@ containing @eff@ associated with @label@.
--
-- Note that if @eff@ is a sum, it will /not/ be decomposed into multiple 'LabelledMember' constraints. While this technically is possible, it results in unsolvable constraints, as the functional dependencies in 'Labelled' prevent assocating the same label with multiple distinct effects within a signature.
--
-- @since 1.0.2.0
type HasLabelled label eff sig m = (LabelledMember label eff sig, Algebra sig m)

-- | Construct a request for a labelled effect to be interpreted by some handler later on.
--
-- @since 1.0.2.0
sendLabelled :: forall label eff sig m a . HasLabelled label eff sig m => eff m a -> m a
sendLabelled op = runIdentity <$> alg (fmap Identity . runIdentity) (injLabelled @label (Labelled op)) (Identity ())
{-# INLINABLE sendLabelled #-}


-- | A transformer to lift effectful actions to labelled effectful actions.
--
-- @since 1.0.2.0
newtype UnderLabel (label :: k) (sub :: (Type -> Type) -> (Type -> Type)) (m :: Type -> Type) a = UnderLabel (m a)
  deriving
    ( Alternative
    , Applicative
    , Functor
    , Monad
    , Fail.MonadFail
    , MonadFix -- ^ @since 1.1.1
    , MonadIO
    , MonadPlus
    )

-- | @since 1.0.2.0
runUnderLabel :: forall label sub m a . UnderLabel label sub m a -> m a
runUnderLabel (UnderLabel l) = l
{-# INLINE runUnderLabel #-}

instance MonadTrans (UnderLabel sub label) where
  lift = UnderLabel
  {-# INLINE lift #-}

instance (LabelledMember label sub sig, Algebra sig m) => Algebra (sub :+: sig) (UnderLabel label sub m) where
  alg hdl = \case
    L sub -> UnderLabel . alg (runUnderLabel . hdl) (injLabelled @label (Labelled sub))
    R sig -> UnderLabel . alg (runUnderLabel . hdl) sig
  {-# INLINE alg #-}
