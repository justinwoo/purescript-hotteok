module Test.Main where

import Prelude

import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (log)
import Control.Monad.Eff.Unsafe (unsafeCoerceEff)
import Control.Monad.Except (runExcept)
import Data.Either (Either(..), isRight)
import Data.Foreign (readInt, readString)
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))
import Hotteok as H
import Test.Unit as T
import Test.Unit.Assert as Assert
import Test.Unit.Main as TM
import Type.Prelude (SProxy(..))
import Unsafe.Coerce (unsafeCoerce)

type TestUnion = H.JSUnion
  ( name :: String
  , count :: Int
  )

nameP = SProxy :: SProxy "name"
countP = SProxy :: SProxy "count"

countGuard :: H.UnsafeGuardFor "count" Int
countGuard =
  H.UnsafeGuardFor
      $ isRight
    <<< runExcept
    <<< readInt

nameGuard :: H.UnsafeGuardFor "name" String
nameGuard =
  H.UnsafeGuardFor
     $ isRight
   <<< runExcept
   <<< readString

main :: Eff () Unit
main = do
  chalkMain

  unsafeCoerceEff $ TM.runTest do

    T.test "fromMember" do
      let (union :: TestUnion) = H.fromMember nameP "banana"
      T.success

    T.test "unsafeCoerceMember" do
      let
        (union :: TestUnion) = H.fromMember nameP "banana"
        value = H.unsafeCoerceMember nameP union
      Assert.equal value "banana"

    T.test "unsafeExtractSingleton" do
      let
        (union :: H.JSUnion (name :: String)) = H.fromMember nameP "banana"
        value = H.unsafeExtractSingleton union
      Assert.equal value "banana"

    T.test "unsafeGuardMember 1" do
      let
        (union :: TestUnion) = H.fromMember nameP "banana"
        guarded = H.unsafeGuardMember countGuard union
      case guarded of
        Right e ->
          T.failure "incorrect branch from JSUnion"
        Left singleton -> do
          let value = H.unsafeExtractSingleton singleton
          Assert.equal value "banana"

    T.test "unsafeGuardMember 2" do
      let
        (union :: TestUnion) = H.fromMember countP 1
        guarded = H.unsafeGuardMember countGuard union
      case guarded of
        Right value -> do
          Assert.equal value 1
        Left e ->
          T.failure "incorrect branch from JSUnion"

    T.test "matchMembers 1" do
      let
        (union :: TestUnion) = H.fromMember countP 1
        match = H.matchJSUnion
          { count: Tuple countGuard show
          , name: Tuple nameGuard id
          }
          union
      case match of
        Just value -> do
          Assert.equal value "1"
        Nothing ->
          T.failure "incorrect result from matchJSUnion"

    T.test "matchMembers 2" do
      let
        (union :: TestUnion) = unsafeCoerce { crap: "some bullshit from JS" }
        match = H.matchJSUnion
          { count: Tuple countGuard show
          , name: Tuple nameGuard id
          }
          union
      case match of
        Just value -> do
          T.failure "incorrect result from matchJSUnion"
        Nothing ->
          T.success


chalkMain :: Eff () Unit
chalkMain = unsafeCoerceEff do
  let
    fnP = SProxy :: SProxy "fn"
    objP = SProxy :: SProxy "obj"
    blueFn = H.unsafeCoerceMember fnP chalk.blue
    blueObj = H.unsafeCoerceMember objP chalk.blue
  log $ blueFn "blue text"
  log $ blueObj.bgYellow "yellow background blue text"

foreign import chalk
  :: { blue
       :: H.JSUnion
            ( fn :: String -> String
            , obj :: { bgYellow :: String -> String }
            )
     }
