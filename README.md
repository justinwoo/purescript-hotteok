# PureScript-Hotteok

A thing for working with JSUnions by guarding members and otherwise creating and extracting members.

![](https://i.imgur.com/N4xa6La.png)

See [the tests](test/Main.purs) for all of the examples.

## Examples:

### Guarding a two-member union and extracting out the remaining singleton

```hs
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

-- ...
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
```

### Matching on union members, with a Nothing returned for no matches

```hs
-- ...
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
```

### Using Chalk.js properties as both functions and objects

```hs
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
```
