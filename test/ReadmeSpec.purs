module Test.ReadmeSpec where

import Prelude

import Data.Maybe (Maybe(..), fromMaybe)
import Optional (class OptionalDefaultArgs, class OptionalMaybeArgs, Unlifted, defaultArgs, maybeArgs)
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)

type OptionalArgs =
  ( name :: String
  , shout :: Boolean
  )

type OptionalArgs2 =
  ( name :: String
  , age :: Maybe Int
  , shout :: Boolean
  )

type OptionalArgs3 =
  ( name :: String
  , age :: Int
  , shout :: Boolean
  )

type OptionalArgs4F :: (Type -> Type) -> Row Type
type OptionalArgs4F f =
  ( name :: f String
  , age :: f Int
  , shout :: f Boolean
  )

type GivenArgs4 = OptionalArgs4F Unlifted
type OptionalArgs4 = OptionalArgs4F Maybe

spec :: Spec Unit
spec = do
  describe "Readme" do
    describe "Snippet 1" do
      it "should run as expected" do
        let 
          sayHello :: ∀ given. OptionalDefaultArgs OptionalArgs given => Record given -> String
          sayHello givenArgs = 
            let args = defaultArgs { name: "PureScript user", shout: false } givenArgs
            in "Hello " <> args.name <> if args.shout then "!!!" else "."

        sayHello {} `shouldEqual` "Hello PureScript user."
        sayHello {shout: true} `shouldEqual` "Hello PureScript user!!!"
        sayHello {name: "Susan"} `shouldEqual` "Hello Susan."

    describe "Snippet 2" do
      it "should run as expected" do
        let 
          sayHello :: ∀ given. OptionalMaybeArgs OptionalArgs3 _ given => Record given -> String
          sayHello givenArgs = 
            let 
              args = maybeArgs @OptionalArgs3 givenArgs
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

    describe "Snippet 3" do
      it "should fail to compile" do
        let 
          _sayHello :: ∀ given. OptionalMaybeArgs OptionalArgs3 _ given => Record given -> String
          _sayHello givenArgs = 
            let 
              args = maybeArgs @OptionalArgs3 givenArgs
              name = fromMaybe "PureScript user" args.name
              shout = fromMaybe false args.shoot
              greeting = case args.age of
                Nothing -> "Hello "
                Just age | age < 20 -> "Hey there "
                Just _   | otherwise -> "Good day "
            in 
              greeting <> name <> if shout then "!!!" else "."

        -- _sayHello {} `shouldEqual` "Hello PureScript user."
        -- _sayHello {shout: true, age: 15} `shouldEqual` "Hey there PureScript user!!!"
        -- _sayHello {name: "Susan", age: 30} `shouldEqual` "Good day Susan."

        pure unit

    describe "Snippet 4" do
      it "should run as expected" do
        let 
          sayHello :: ∀ given. OptionalMaybeArgs GivenArgs4 OptionalArgs4 given => Record given -> String
          sayHello givenArgs = 
            let 
              args = maybeArgs @GivenArgs4 givenArgs
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
