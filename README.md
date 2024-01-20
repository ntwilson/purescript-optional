# purescript-optional

This library is for ergonomically writing PureScript functions that take in optional parameters. The intent is specifically for functions written in and called from PureScript - it is not intended to be used for FFI. The recommendation for FFI would be to use any of a number of existing libraries designed for it, including [purescript-options](https://pursuit.purescript.org/packages/purescript-options), [purescript-untagged-union](https://pursuit.purescript.org/packages/purescript-untagged-union), [purescript-undefined-is-not-a-problem](https://pursuit.purescript.org/packages/purescript-undefined-is-not-a-problem), or [purescript-convertable-options](https://pursuit.purescript.org/packages/purescript-convertable-options).

The simplest case for a function with optional parameters is to just use the `merge` function for records plus some defaults:

```purescript
type OptionalArgs =
  ( name :: String
  , shout :: Boolean
  )

sayHello :: ∀ given all. Union given OptionalArgs all => Nub all OptionalArgs => Record given -> String
sayHello givenArgs = 
  let args = Record.merge givenArgs { name: "PureScript user", shout: false }
  in "Hello " <> args.name <> if args.shout then "!!!" else "."

sayHello {} `shouldEqual` "Hello PureScript user."
sayHello {shout: true} `shouldEqual` "Hello PureScript user!!!"
sayHello {name: "Susan"} `shouldEqual` "Hello Susan."
```

This library slightly simplifies that case with the `OptionalDefaultArgs` class and its method `defaultArgs`:

```purescript
type OptionalArgs =
  ( name :: String
  , shout :: Boolean
  )

sayHello :: ∀ given. OptionalDefaultArgs OptionalArgs given => Record given -> String
sayHello givenArgs = 
  let args = defaultArgs { name: "PureScript user", shout: false } givenArgs
  in "Hello " <> args.name <> if args.shout then "!!!" else "."

sayHello {} `shouldEqual` "Hello PureScript user."
sayHello {shout: true} `shouldEqual` "Hello PureScript user!!!"
sayHello {name: "Susan"} `shouldEqual` "Hello Susan."
```

This approach breaks down slightly when an optional value doesn't have an appropriate default. You could make the optional args include a `Maybe` value:

```purescript
type OptionalArgs2 =
  ( name :: String
  , age :: Maybe Int
  , shout :: Boolean
  )

sayHello :: ∀ given. OptionalDefaultArgs OptionalArgs2 given => Record given -> String
sayHello givenArgs = 
  let 
    args = defaultArgs { name: "PureScript user", age: Nothing, shout: false } givenArgs
    greeting = case args.age of
      Nothing -> "Hello "
      Just age | age < 20 -> "Hey there "
      Just _   | otherwise -> "Good day "
  in 
    greeting <> args.name <> if args.shout then "!!!" else "."

sayHello {} `shouldEqual` "Hello PureScript user."
sayHello {shout: true, age: Just 15} `shouldEqual` "Hey there PureScript user!!!"
sayHello {name: "Susan", age: Just 30} `shouldEqual` "Good day Susan."
```

But this is inconvenient for the caller, who has to use `Just` for the parameter, and it exposes the implementation details of your function (some optional arguments are `Maybe` and others aren't). This is the main case that this library aims to simplify. You may use the `OptionalMaybeArgs` class with its method `maybeArgs` to make the optional values all `Maybe` within your function without the caller having to use `Just`. The simplest way to do this would be:

```purescript
sayHello :: ∀ given. OptionalMaybeArgs OptionalArgs2 _ given => Record given -> String
sayHello givenArgs = 
  let 
    args = maybeArgs @OptionalArgs2 givenArgs
    name = fromMaybe "PureScript user" args.name
    shout = fromMaybe false args.shout
    greeting = case args.age of
      Nothing -> "Hello "
      Just age | age < 20 -> "Hey there "
      Just _   | otherwise -> "Good day "
  in 
    greeting <> name <> if shout then "!!!" else "."

sayHello {} `shouldEqual` "Hello PureScript user."
sayHello {shout: true, age: 15} `shouldEqual` "Hey there PureScript user!!!"
sayHello {name: "Susan", age: 30} `shouldEqual` "Good day Susan."
```

By using the wildcard `_` in the type signature of `sayHello2`, we don't need to specify the return type of `maybeArgs` (which in this case would be `{ name :: Maybe String, age :: Maybe Int, shout :: Maybe Boolean }`), but it comes at the cost of slightly worse error messages. The compiler doesn't determine the return type of `maybeArgs` until it commits to an `OptionalMaybeArgs` instance, which it can't do until `given` is known. That means that if you introduce a bug into `sayHello`, the error message won't show up in `sayHello`, but rather at the call site. 

```pureScript
sayHello :: ∀ given. OptionalMaybeArgs OptionalArgs2 _ given => Record given -> String
sayHello givenArgs = 
  let 
    args = maybeArgs @OptionalArgs2 givenArgs
    name = fromMaybe "PureScript user" args.name
    shout = fromMaybe false args.shoot -- typo: `shoot` instead of `shout` but no compile error
    greeting = case args.age of
      Nothing -> "Hello "
      Just age | age < 20 -> "Hey there "
      Just _   | otherwise -> "Good day "
  in 
    greeting <> name <> if shout then "!!!" else "."


sayHello {} `shouldEqual` "Hello PureScript user." -- compile error here instead of in `sayHello`: 
  -- Could not match type ( shout :: Maybe Boolean ... ) with type ( shoot :: Maybe Boolean ... | t3 )
```

For this reason, it can be good to provide an annotation for the `Maybe` version of all the optional parameters. This library advocates using the strategy suggested in [this discourse post](https://discourse.purescript.org/t/is-it-possible-to-define-type-that-enchances-record-with-maybe/1780/2?u=ntwilson), and provides an `Unlifted` alias to do just that:

```purescript
type OptionalArgs3F :: (Type -> Type) -> Row Type
type OptionalArgs3F f =
  ( name :: f String
  , age :: f Int
  , shout :: f Boolean
  )

type GivenArgs3 = OptionalArgs3F Unlifted
type OptionalArgs3 = OptionalArgs3F Maybe

sayHello :: ∀ given. OptionalMaybeArgs GivenArgs3 OptionalArgs3 given => Record given -> String
sayHello givenArgs = 
  let 
    args = maybeArgs @GivenArgs3 givenArgs
    name = fromMaybe "PureScript user" args.name
    shout = fromMaybe false args.shout
    greeting = case args.age of
      Nothing -> "Hello "
      Just age | age < 20 -> "Hey there "
      Just _   | otherwise -> "Good day "
  in 
    greeting <> name <> if shout then "!!!" else "."

sayHello {} `shouldEqual` "Hello PureScript user."
sayHello {shout: true, age: 15} `shouldEqual` "Hey there PureScript user!!!"
sayHello {name: "Susan", age: 30} `shouldEqual` "Good day Susan."
```

If you make a typo, the error shows up in the correct spot in code.
