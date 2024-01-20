module Main where

import Prelude

import Data.Maybe (Maybe, fromMaybe)
import Effect (Effect)
import Effect.Console (log)
import Optional (class OptionalDefaultArgs, class OptionalMaybeArgs, Unlifted, defaultArgs, maybeArgs)

type OptionalArgsF :: (Type -> Type) -> Row Type
type OptionalArgsF f =
  ( headerClassName :: f String
  , contentStyle :: f Int
  )
type OptionalArgs = OptionalArgsF Unlifted
type MaybeArgs = OptionalArgsF Maybe

defaults :: Record (OptionalArgs)
defaults =
  { headerClassName: "class"
  , contentStyle: 1
  }

fnWithMaybe :: ∀ given. OptionalMaybeArgs OptionalArgs MaybeArgs given => Record given -> String
fnWithMaybe optionalArgs =
  let args = maybeArgs @OptionalArgs optionalArgs
  in fromMaybe "class" args.headerClassName <> fromMaybe "" (show <$> args.contentStyle)

fnWithDefault :: ∀ given. OptionalDefaultArgs OptionalArgs given => Record given -> String
fnWithDefault optionalArgs =
  let args = defaultArgs defaults optionalArgs
  in args.headerClassName <> show args.contentStyle

main :: Effect Unit
main = do
  log ("fnWithMaybe {}: " <> fnWithMaybe {})
  log ("fnWithMaybe {contentStyle}: " <> fnWithMaybe {contentStyle: 5})

  log ("fnWithDefault {}: " <> fnWithDefault {})
  log ("fnWithDefault {contentStyle}: " <> fnWithDefault {contentStyle: 5})

