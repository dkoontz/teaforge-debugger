module DiffTest exposing (suite)

{-| Unit tests for the Diff module.

Tests cover:
- Correctly identifying added fields
- Correctly identifying removed fields
- Correctly identifying modified values
- Detecting type changes (e.g., string to object)
- Handling deeply nested structures
- Returning empty result for identical states
- JSON Patch (RFC 6902) application via applyPatch / diffOperationDecoder

-}

import Dict
import Diff exposing (..)
import Expect
import Json.Decode as D
import Json.Encode as E
import Test exposing (..)


suite : Test
suite =
    describe "Diff"
        [ describe "compareStates"
            [ test "returns empty list for identical states" <|
                \_ ->
                    let
                        state =
                            E.object [ ( "name", E.string "test" ) ]
                    in
                    compareStates state state
                        |> Expect.equal []
            , test "detects modified primitive value" <|
                \_ ->
                    let
                        before =
                            E.object [ ( "count", E.int 1 ) ]

                        after =
                            E.object [ ( "count", E.int 2 ) ]
                    in
                    compareStates before after
                        |> Expect.equal [ [ "count" ] ]
            , test "detects modified string value" <|
                \_ ->
                    let
                        before =
                            E.object [ ( "name", E.string "Alice" ) ]

                        after =
                            E.object [ ( "name", E.string "Bob" ) ]
                    in
                    compareStates before after
                        |> Expect.equal [ [ "name" ] ]
            , test "detects modified boolean value" <|
                \_ ->
                    let
                        before =
                            E.object [ ( "active", E.bool True ) ]

                        after =
                            E.object [ ( "active", E.bool False ) ]
                    in
                    compareStates before after
                        |> Expect.equal [ [ "active" ] ]
            , test "returns empty for identical nested objects" <|
                \_ ->
                    let
                        state =
                            E.object
                                [ ( "user"
                                  , E.object
                                        [ ( "name", E.string "test" )
                                        , ( "age", E.int 25 )
                                        ]
                                  )
                                ]
                    in
                    compareStates state state
                        |> Expect.equal []
            ]
        , describe "findChangedPaths"
            [ test "detects added field" <|
                \_ ->
                    let
                        before =
                            E.object []

                        after =
                            E.object [ ( "newField", E.string "value" ) ]

                        result =
                            findChangedPaths before after
                    in
                    Dict.get "newField" result.changes
                        |> Expect.equal (Just Added)
            , test "detects removed field" <|
                \_ ->
                    let
                        before =
                            E.object [ ( "oldField", E.string "value" ) ]

                        after =
                            E.object []

                        result =
                            findChangedPaths before after
                    in
                    Dict.get "oldField" result.changes
                        |> Expect.equal (Just Removed)
            , test "detects type change from string to object" <|
                \_ ->
                    let
                        before =
                            E.object [ ( "data", E.string "simple" ) ]

                        after =
                            E.object [ ( "data", E.object [ ( "complex", E.bool True ) ] ) ]

                        result =
                            findChangedPaths before after
                    in
                    Dict.get "data" result.changes
                        |> Expect.equal (Just TypeChanged)
            , test "detects type change from int to string" <|
                \_ ->
                    let
                        before =
                            E.object [ ( "value", E.int 42 ) ]

                        after =
                            E.object [ ( "value", E.string "42" ) ]

                        result =
                            findChangedPaths before after
                    in
                    Dict.get "value" result.changes
                        |> Expect.equal (Just TypeChanged)
            , test "counts changes correctly" <|
                \_ ->
                    let
                        before =
                            E.object
                                [ ( "removed", E.string "gone" )
                                , ( "modified", E.int 1 )
                                ]

                        after =
                            E.object
                                [ ( "added", E.string "new" )
                                , ( "modified", E.int 2 )
                                ]

                        result =
                            findChangedPaths before after
                    in
                    Expect.all
                        [ \_ -> Expect.equal 1 result.addedCount
                        , \_ -> Expect.equal 1 result.removedCount
                        , \_ -> Expect.equal 1 result.modifiedCount
                        ]
                        ()
            ]
        , describe "nested structures"
            [ test "detects change in nested object" <|
                \_ ->
                    let
                        before =
                            E.object
                                [ ( "user"
                                  , E.object
                                        [ ( "profile"
                                          , E.object [ ( "name", E.string "Alice" ) ]
                                          )
                                        ]
                                  )
                                ]

                        after =
                            E.object
                                [ ( "user"
                                  , E.object
                                        [ ( "profile"
                                          , E.object [ ( "name", E.string "Bob" ) ]
                                          )
                                        ]
                                  )
                                ]
                    in
                    compareStates before after
                        |> Expect.equal [ [ "user", "profile", "name" ] ]
            , test "detects added nested field" <|
                \_ ->
                    let
                        before =
                            E.object
                                [ ( "user", E.object [ ( "name", E.string "test" ) ] )
                                ]

                        after =
                            E.object
                                [ ( "user"
                                  , E.object
                                        [ ( "name", E.string "test" )
                                        , ( "email", E.string "test@example.com" )
                                        ]
                                  )
                                ]

                        result =
                            findChangedPaths before after
                    in
                    Dict.get "user.email" result.changes
                        |> Expect.equal (Just Added)
            ]
        , describe "array handling"
            [ test "detects change in array element" <|
                \_ ->
                    let
                        before =
                            E.object [ ( "items", E.list E.int [ 1, 2, 3 ] ) ]

                        after =
                            E.object [ ( "items", E.list E.int [ 1, 5, 3 ] ) ]
                    in
                    compareStates before after
                        |> Expect.equal [ [ "items", "1" ] ]
            , test "detects added array element" <|
                \_ ->
                    let
                        before =
                            E.object [ ( "items", E.list E.int [ 1, 2 ] ) ]

                        after =
                            E.object [ ( "items", E.list E.int [ 1, 2, 3 ] ) ]

                        result =
                            findChangedPaths before after
                    in
                    Dict.get "items.2" result.changes
                        |> Expect.equal (Just Added)
            , test "detects removed array element" <|
                \_ ->
                    let
                        before =
                            E.object [ ( "items", E.list E.int [ 1, 2, 3 ] ) ]

                        after =
                            E.object [ ( "items", E.list E.int [ 1, 2 ] ) ]

                        result =
                            findChangedPaths before after
                    in
                    Dict.get "items.2" result.changes
                        |> Expect.equal (Just Removed)
            ]
        , describe "pathToString"
            [ test "converts path to dot notation" <|
                \_ ->
                    pathToString [ "user", "profile", "name" ]
                        |> Expect.equal "user.profile.name"
            , test "handles empty path" <|
                \_ ->
                    pathToString []
                        |> Expect.equal ""
            , test "handles single segment" <|
                \_ ->
                    pathToString [ "name" ]
                        |> Expect.equal "name"
            ]
        , describe "diffOperationDecoder"
            [ test "decodes add operation with correct fields" <|
                \_ ->
                    let
                        json =
                            E.object
                                [ ( "op", E.string "add" )
                                , ( "path", E.string "/foo" )
                                , ( "value", E.int 42 )
                                ]
                    in
                    D.decodeValue diffOperationDecoder json
                        |> Expect.equal (Ok (AddOp "/foo" (E.int 42)))
            , test "decodes replace operation with correct fields" <|
                \_ ->
                    let
                        json =
                            E.object
                                [ ( "op", E.string "replace" )
                                , ( "path", E.string "/bar" )
                                , ( "value", E.string "hello" )
                                ]
                    in
                    D.decodeValue diffOperationDecoder json
                        |> Expect.equal (Ok (ReplaceOp "/bar" (E.string "hello")))
            , test "decodes remove operation with correct fields" <|
                \_ ->
                    let
                        json =
                            E.object
                                [ ( "op", E.string "remove" )
                                , ( "path", E.string "/baz" )
                                ]
                    in
                    D.decodeValue diffOperationDecoder json
                        |> Expect.equal (Ok (RemoveOp "/baz"))
            , test "fails on unknown op" <|
                \_ ->
                    let
                        json =
                            E.object
                                [ ( "op", E.string "move" )
                                , ( "path", E.string "/foo" )
                                ]
                    in
                    case D.decodeValue diffOperationDecoder json of
                        Err _ ->
                            Expect.pass

                        Ok _ ->
                            Expect.fail "Expected decode failure for unknown op"
            , test "decodes list of operations" <|
                \_ ->
                    let
                        json =
                            E.list identity
                                [ E.object
                                    [ ( "op", E.string "add" )
                                    , ( "path", E.string "/x" )
                                    , ( "value", E.bool True )
                                    ]
                                , E.object
                                    [ ( "op", E.string "remove" )
                                    , ( "path", E.string "/y" )
                                    ]
                                ]
                    in
                    D.decodeValue (D.list diffOperationDecoder) json
                        |> Expect.equal
                            (Ok
                                [ AddOp "/x" (E.bool True)
                                , RemoveOp "/y"
                                ]
                            )
            ]
        , describe "applyPatch"
            [ describe "add operations"
                [ test "add to empty object" <|
                    \_ ->
                        let
                            base =
                                E.object []

                            ops =
                                [ AddOp "/name" (E.string "Alice") ]

                            result =
                                applyPatch base ops
                        in
                        D.decodeValue (D.field "name" D.string) result
                            |> Expect.equal (Ok "Alice")
                , test "add new key to existing object" <|
                    \_ ->
                        let
                            base =
                                E.object [ ( "existing", E.int 1 ) ]

                            ops =
                                [ AddOp "/newKey" (E.string "value") ]

                            result =
                                applyPatch base ops
                        in
                        Expect.all
                            [ \_ ->
                                D.decodeValue (D.field "existing" D.int) result
                                    |> Expect.equal (Ok 1)
                            , \_ ->
                                D.decodeValue (D.field "newKey" D.string) result
                                    |> Expect.equal (Ok "value")
                            ]
                            ()
                , test "add to array by index" <|
                    \_ ->
                        let
                            base =
                                E.object [ ( "items", E.list E.int [ 1, 3 ] ) ]

                            ops =
                                [ AddOp "/items/1" (E.int 2) ]

                            result =
                                applyPatch base ops
                        in
                        D.decodeValue (D.field "items" (D.index 1 D.int)) result
                            |> Expect.equal (Ok 2)
                , test "add appends to array with '-' index" <|
                    \_ ->
                        let
                            base =
                                E.object [ ( "items", E.list E.int [ 1, 2 ] ) ]

                            ops =
                                [ AddOp "/items/-" (E.int 3) ]

                            result =
                                applyPatch base ops
                        in
                        D.decodeValue (D.field "items" (D.list D.int)) result
                            |> Expect.equal (Ok [ 1, 2, 3 ])
                , test "add appends to empty array with '-' index" <|
                    \_ ->
                        let
                            base =
                                E.object [ ( "items", E.list E.int [] ) ]

                            ops =
                                [ AddOp "/items/-" (E.int 42) ]

                            result =
                                applyPatch base ops
                        in
                        D.decodeValue (D.field "items" (D.list D.int)) result
                            |> Expect.equal (Ok [ 42 ])
                ]
            , describe "replace operations"
                [ test "replace existing key" <|
                    \_ ->
                        let
                            base =
                                E.object [ ( "count", E.int 1 ) ]

                            ops =
                                [ ReplaceOp "/count" (E.int 99) ]

                            result =
                                applyPatch base ops
                        in
                        D.decodeValue (D.field "count" D.int) result
                            |> Expect.equal (Ok 99)
                , test "replace root value" <|
                    \_ ->
                        let
                            base =
                                E.object [ ( "old", E.bool True ) ]

                            newRoot =
                                E.object [ ( "new", E.bool False ) ]

                            ops =
                                [ ReplaceOp "" newRoot ]

                            result =
                                applyPatch base ops
                        in
                        D.decodeValue (D.field "new" D.bool) result
                            |> Expect.equal (Ok False)
                ]
            , describe "remove operations"
                [ test "remove a key from object" <|
                    \_ ->
                        let
                            base =
                                E.object
                                    [ ( "keep", E.int 1 )
                                    , ( "remove", E.int 2 )
                                    ]

                            ops =
                                [ RemoveOp "/remove" ]

                            result =
                                applyPatch base ops
                        in
                        Expect.all
                            [ \_ ->
                                D.decodeValue (D.field "keep" D.int) result
                                    |> Expect.equal (Ok 1)
                            , \_ ->
                                case D.decodeValue (D.field "remove" D.int) result of
                                    Err _ ->
                                        Expect.pass

                                    Ok _ ->
                                        Expect.fail "Expected 'remove' key to be gone"
                            ]
                            ()
                , test "remove from array by index" <|
                    \_ ->
                        let
                            base =
                                E.object [ ( "items", E.list E.int [ 10, 20, 30 ] ) ]

                            ops =
                                [ RemoveOp "/items/1" ]

                            result =
                                applyPatch base ops
                        in
                        D.decodeValue (D.field "items" (D.list D.int)) result
                            |> Expect.equal (Ok [ 10, 30 ])
                , test "remove root is a no-op" <|
                    \_ ->
                        let
                            base =
                                E.object [ ( "x", E.int 5 ) ]

                            ops =
                                [ RemoveOp "" ]

                            result =
                                applyPatch base ops
                        in
                        D.decodeValue (D.field "x" D.int) result
                            |> Expect.equal (Ok 5)
                ]
            , describe "deeply nested operations"
                [ test "add deeply nested field" <|
                    \_ ->
                        let
                            base =
                                E.object
                                    [ ( "a"
                                      , E.object
                                            [ ( "b", E.object [] ) ]
                                      )
                                    ]

                            ops =
                                [ AddOp "/a/b/c" (E.string "deep") ]

                            result =
                                applyPatch base ops
                        in
                        D.decodeValue
                            (D.at [ "a", "b", "c" ] D.string)
                            result
                            |> Expect.equal (Ok "deep")
                , test "replace deeply nested field" <|
                    \_ ->
                        let
                            base =
                                E.object
                                    [ ( "user"
                                      , E.object
                                            [ ( "profile"
                                              , E.object [ ( "name", E.string "Alice" ) ]
                                              )
                                            ]
                                      )
                                    ]

                            ops =
                                [ ReplaceOp "/user/profile/name" (E.string "Bob") ]

                            result =
                                applyPatch base ops
                        in
                        D.decodeValue
                            (D.at [ "user", "profile", "name" ] D.string)
                            result
                            |> Expect.equal (Ok "Bob")
                , test "remove deeply nested field" <|
                    \_ ->
                        let
                            base =
                                E.object
                                    [ ( "data"
                                      , E.object
                                            [ ( "keep", E.int 1 )
                                            , ( "drop", E.int 2 )
                                            ]
                                      )
                                    ]

                            ops =
                                [ RemoveOp "/data/drop" ]

                            result =
                                applyPatch base ops
                        in
                        Expect.all
                            [ \_ ->
                                D.decodeValue (D.at [ "data", "keep" ] D.int) result
                                    |> Expect.equal (Ok 1)
                            , \_ ->
                                case D.decodeValue (D.at [ "data", "drop" ] D.int) result of
                                    Err _ ->
                                        Expect.pass

                                    Ok _ ->
                                        Expect.fail "Expected 'drop' to be removed"
                            ]
                            ()
                ]
            , describe "multiple operations applied in sequence"
                [ test "add then replace" <|
                    \_ ->
                        let
                            base =
                                E.object []

                            ops =
                                [ AddOp "/x" (E.int 1)
                                , ReplaceOp "/x" (E.int 2)
                                ]

                            result =
                                applyPatch base ops
                        in
                        D.decodeValue (D.field "x" D.int) result
                            |> Expect.equal (Ok 2)
                , test "add then remove" <|
                    \_ ->
                        let
                            base =
                                E.object []

                            ops =
                                [ AddOp "/y" (E.string "temp")
                                , RemoveOp "/y"
                                ]

                            result =
                                applyPatch base ops
                        in
                        case D.decodeValue (D.field "y" D.string) result of
                            Err _ ->
                                Expect.pass

                            Ok _ ->
                                Expect.fail "Expected 'y' to be removed"
                , test "empty patch list returns base unchanged" <|
                    \_ ->
                        let
                            base =
                                E.object [ ( "val", E.int 7 ) ]

                            result =
                                applyPatch base []
                        in
                        D.decodeValue (D.field "val" D.int) result
                            |> Expect.equal (Ok 7)
                ]
            , describe "RFC 6901 pointer escaping"
                [ test "~1 is unescaped to /" <|
                    \_ ->
                        let
                            base =
                                E.object []

                            ops =
                                [ AddOp "/a~1b" (E.int 1) ]

                            result =
                                applyPatch base ops
                        in
                        D.decodeValue (D.field "a/b" D.int) result
                            |> Expect.equal (Ok 1)
                , test "~0 is unescaped to ~" <|
                    \_ ->
                        let
                            base =
                                E.object []

                            ops =
                                [ AddOp "/a~0b" (E.string "tilde") ]

                            result =
                                applyPatch base ops
                        in
                        D.decodeValue (D.field "a~b" D.string) result
                            |> Expect.equal (Ok "tilde")
                ]
            ]
        ]
