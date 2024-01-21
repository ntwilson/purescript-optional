module Optional where

import Prelude

import Data.Maybe (Maybe(..))
import Data.Symbol (class IsSymbol, reflectSymbol)
import Prim.Row (class Nub, class Union)
import Prim.Row as Row
import Prim.RowList (class RowToList, RowList)
import Prim.RowList as RL
import Record as Record
import Record.Builder (Builder)
import Record.Builder as Builder
import Record.Unsafe as URecord
import Type.Proxy (Proxy(..))

type Unlifted :: Type -> Type
type Unlifted a = a


class OptionalDefaultArgs :: Row Type -> Row Type -> Constraint
class OptionalDefaultArgs all given where
  defaultArgs :: Record all -> Record given -> Record all

instance (Union given all total, Nub total all) => OptionalDefaultArgs all given where
  defaultArgs = flip Record.merge

class OptionalMaybeArgs :: Row Type -> Row Type -> Row Type -> Constraint
class OptionalMaybeArgs allUnlifted allMaybe given | allUnlifted -> allMaybe where
  maybeArgs :: Record given -> Record allMaybe

instance
  ( Union given allUnlifted trash
  , Nub trash allUnlifted
  , RowToList allUnlifted allList
  , OptionalMaybeArgs' allList allUnlifted given allMaybe
  ) => OptionalMaybeArgs allUnlifted allMaybe given
  where
  maybeArgs given = Builder.build builder {}
    where
      builder = maybeArgsImpl @allList @allUnlifted given

class OptionalMaybeArgs' :: RowList Type -> Row Type -> Row Type -> Row Type -> Constraint
class OptionalMaybeArgs' list unlifted given to | list -> to, unlifted -> to where
  maybeArgsImpl :: Record given -> Builder (Record ()) (Record to)

instance recordSingle :: 
  ( IsSymbol name
  , Row.Cons name ty trash unlifted
  , Row.Lacks name ()
  , Row.Cons name (Maybe ty) () to
  ) => OptionalMaybeArgs' (RL.Cons name ty RL.Nil) unlifted given to where
  maybeArgsImpl a = Builder.insert (Proxy @name) valA
    where
      namep = Proxy @name
      name = reflectSymbol namep
      valA = 
        if URecord.unsafeHas name a
        then Just (URecord.unsafeGet name a)
        else Nothing

else instance recordCons ::
  ( IsSymbol name
  , Row.Cons name ty trash unlifted
  , OptionalMaybeArgs' tail unlifted given from
  , Row.Lacks name from
  , Row.Cons name (Maybe ty) from to
  ) => OptionalMaybeArgs' (RL.Cons name ty tail) unlifted given to where
  maybeArgsImpl a = Builder.insert namep valA <<< rest
    where
      namep = Proxy @name
      name = reflectSymbol namep
      valA = 
        if URecord.unsafeHas name a
        then Just (URecord.unsafeGet name a)
        else Nothing
      rest = maybeArgsImpl @tail @unlifted a

instance recordNil :: OptionalMaybeArgs' RL.Nil given unlifted () where
  maybeArgsImpl _ = identity

