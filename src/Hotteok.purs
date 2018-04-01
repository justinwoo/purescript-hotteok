module Hotteok where

import Data.Either (Either(..))
import Data.Foreign (Foreign, toForeign)
import Type.Prelude (class RowToList, SProxy)
import Type.Row (Cons, Nil)
import Unsafe.Coerce (unsafeCoerce)

data JSUnion (members :: # Type) = JSUnion

newtype UnsafeGuardFor (name :: Symbol) ty =
  UnsafeGuardFor (Foreign -> Boolean)

unsafeGuardMember
  :: forall name ty members' members
   . RowCons name ty members' members
  => UnsafeGuardFor name ty
  -> JSUnion members
  -> Either (JSUnion members') ty
unsafeGuardMember (UnsafeGuardFor check) jsUnion =
  if check (toForeign jsUnion)
     then Right (unsafeCoerce jsUnion)
     else Left (unsafeCoerce jsUnion)

unsafeCoerceMember
  :: forall name ty members' members
   . RowCons name ty members' members
  => SProxy name
  -> JSUnion members
  -> ty
unsafeCoerceMember _ x =
  unsafeCoerce x

unsafeExtractSingleton
  :: forall members name ty
   . RowToList members (Cons name ty Nil)
  => JSUnion members
  -> ty
unsafeExtractSingleton =
  unsafeCoerce

fromMember
  :: forall name ty members' members
   . RowCons name ty members' members
  => SProxy name
  -> ty
  -> JSUnion members
fromMember _ x = unsafeCoerce x
