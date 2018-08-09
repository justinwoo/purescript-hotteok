module Hotteok where

import Prelude

import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))
import Foreign (Foreign, unsafeToForeign)
import Prim.Row as Row
import Record as Record
import Type.Prelude (class IsSymbol, class RowToList, RLProxy(RLProxy), SProxy(SProxy))
import Type.Row (Cons, Nil, kind RowList)
import Unsafe.Coerce (unsafeCoerce)

data JSUnion (members :: # Type)

newtype UnsafeGuardFor (name :: Symbol) ty =
  UnsafeGuardFor (Foreign -> Boolean)

unsafeGuardMember
  :: forall name ty members' members
   . Row.Cons name ty members' members
  => UnsafeGuardFor name ty
  -> JSUnion members
  -> Either (JSUnion members') ty
unsafeGuardMember (UnsafeGuardFor check) jsUnion =
  if check (unsafeToForeign jsUnion)
     then Right (unsafeCoerce jsUnion)
     else Left (unsafeCoerce jsUnion)

unsafeCoerceMember
  :: forall name ty members' members
   . Row.Cons name ty members' members
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
   . Row.Cons name ty members' members
  => SProxy name
  -> ty
  -> JSUnion members
fromMember _ x = unsafeCoerce x

matchJSUnion
  :: forall members xs pairs result
   . RowToList members xs
  => MatchMembers xs members pairs result
  => { | pairs }
  -> JSUnion members
  -> Maybe result
matchJSUnion =
  matchMembers (RLProxy :: RLProxy xs)

class MatchMembers (xs :: RowList) (members :: # Type) (pairs :: # Type) result
  | xs result -> members pairs
  where
    matchMembers :: RLProxy xs -> { | pairs } -> JSUnion members -> Maybe result

instance matchMembersNil :: MatchMembers Nil members pairs result where
  matchMembers _ _ _ = Nothing

instance matchMembersCons ::
  ( IsSymbol name
  , Row.Cons name ty members' members
  , Row.Cons name (Tuple (UnsafeGuardFor name ty) (ty -> result)) pairs' pairs
  , MatchMembers tail members pairs result
  ) => MatchMembers (Cons name ty tail) members pairs result where
  matchMembers _ pairs union =
    case unsafeGuardMember unsafeGuard union of
      Right x -> Just $ fn x
      Left _ -> matchMembers (RLProxy :: RLProxy tail) pairs union
    where
      nameP = SProxy :: SProxy name
      Tuple unsafeGuard fn = Record.get nameP pairs
