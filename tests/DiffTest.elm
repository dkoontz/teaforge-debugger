module DiffTest exposing (suite)

{-| Unit tests for the Diff module.

Tests cover:
- Correctly identifying added fields
- Correctly identifying removed fields
- Correctly identifying modified values
- Detecting type changes (e.g., string to object)
- Handling deeply nested structures
- Returning empty result for identical states

-}

import Dict
import Diff exposing (..)
import Expect
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
        ]
