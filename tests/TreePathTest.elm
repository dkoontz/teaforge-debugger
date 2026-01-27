module TreePathTest exposing (suite)

{-| Unit tests for TreePath navigation and manipulation.

Tests cover:
- Correctly navigating to nested object fields
- Correctly navigating to array indices
- Converting path to string correctly ("user.profile.name")
- Handling empty paths
- Handling deeply nested paths (20+ levels)

Note: TreePath is defined in Types.elm as `type alias TreePath = List String`
These tests verify the behavior of path-related operations in Diff and Search modules.

-}

import Diff
import Expect
import Json.Encode as E
import Search
import Test exposing (..)


suite : Test
suite =
    describe "TreePath"
        [ describe "path representation"
            [ test "empty path is empty list" <|
                \_ ->
                    let
                        path =
                            []
                    in
                    Expect.equal 0 (List.length path)
            , test "single level path" <|
                \_ ->
                    let
                        path =
                            [ "name" ]
                    in
                    Expect.equal 1 (List.length path)
            , test "nested object path" <|
                \_ ->
                    let
                        path =
                            [ "user", "profile", "name" ]
                    in
                    Expect.all
                        [ \_ -> Expect.equal 3 (List.length path)
                        , \_ -> Expect.equal (Just "user") (List.head path)
                        , \_ -> Expect.equal (Just "name") (List.head (List.reverse path))
                        ]
                        ()
            , test "array index path" <|
                \_ ->
                    let
                        path =
                            [ "items", "0", "name" ]
                    in
                    Expect.all
                        [ \_ -> Expect.equal 3 (List.length path)
                        , \_ -> Expect.equal (Just "0") (List.head (List.drop 1 path))
                        ]
                        ()
            ]
        , describe "pathToString via Diff module"
            [ test "converts nested path to dot notation" <|
                \_ ->
                    Diff.pathToString [ "user", "profile", "name" ]
                        |> Expect.equal "user.profile.name"
            , test "handles empty path" <|
                \_ ->
                    Diff.pathToString []
                        |> Expect.equal ""
            , test "handles single segment" <|
                \_ ->
                    Diff.pathToString [ "name" ]
                        |> Expect.equal "name"
            , test "handles array indices" <|
                \_ ->
                    Diff.pathToString [ "items", "5", "value" ]
                        |> Expect.equal "items.5.value"
            ]
        , describe "pathToString via Search module"
            [ test "converts path to dot notation" <|
                \_ ->
                    Search.pathToString [ "settings", "theme", "color" ]
                        |> Expect.equal "settings.theme.color"
            , test "handles deep nesting" <|
                \_ ->
                    let
                        deepPath =
                            [ "a", "b", "c", "d", "e", "f", "g", "h", "i", "j" ]
                    in
                    Search.pathToString deepPath
                        |> Expect.equal "a.b.c.d.e.f.g.h.i.j"
            ]
        , describe "deeply nested paths (20+ levels)"
            [ test "handles 20 levels of nesting" <|
                \_ ->
                    let
                        deepPath =
                            List.range 1 20
                                |> List.map (\n -> "level" ++ String.fromInt n)
                    in
                    Expect.all
                        [ \_ -> Expect.equal 20 (List.length deepPath)
                        , \_ ->
                            Diff.pathToString deepPath
                                |> String.contains "level1"
                                |> Expect.equal True
                        , \_ ->
                            Diff.pathToString deepPath
                                |> String.contains "level20"
                                |> Expect.equal True
                        ]
                        ()
            , test "handles 25 levels of nesting" <|
                \_ ->
                    let
                        deepPath =
                            List.range 1 25
                                |> List.map (\n -> "l" ++ String.fromInt n)
                    in
                    Expect.all
                        [ \_ -> Expect.equal 25 (List.length deepPath)
                        , \_ ->
                            Search.pathToString deepPath
                                |> String.split "."
                                |> List.length
                                |> Expect.equal 25
                        ]
                        ()
            ]
        , describe "path navigation in nested structures"
            [ test "diff correctly identifies deeply nested change" <|
                \_ ->
                    let
                        buildNested depth value =
                            if depth <= 0 then
                                E.string value

                            else
                                E.object [ ( "nested", buildNested (depth - 1) value ) ]

                        before =
                            buildNested 5 "old"

                        after =
                            buildNested 5 "new"

                        changedPaths =
                            Diff.compareStates before after
                    in
                    Expect.equal 1 (List.length changedPaths)
            , test "search finds value in deeply nested structure" <|
                \_ ->
                    let
                        deepJson =
                            E.object
                                [ ( "level1"
                                  , E.object
                                        [ ( "level2"
                                          , E.object
                                                [ ( "level3"
                                                  , E.object
                                                        [ ( "level4"
                                                          , E.object
                                                                [ ( "target", E.string "findme" )
                                                                ]
                                                          )
                                                        ]
                                                  )
                                                ]
                                          )
                                        ]
                                  )
                                ]

                        result =
                            Search.search "findme" deepJson
                    in
                    Expect.all
                        [ \_ -> Expect.equal 1 result.matchCount
                        , \_ ->
                            case List.head result.matches of
                                Just path ->
                                    Expect.equal 5 (List.length path)

                                Nothing ->
                                    Expect.fail "Expected a match"
                        ]
                        ()
            ]
        , describe "path construction for arrays"
            [ test "diff correctly tracks array element paths" <|
                \_ ->
                    let
                        before =
                            E.object
                                [ ( "users"
                                  , E.list identity
                                        [ E.object [ ( "name", E.string "Alice" ) ]
                                        , E.object [ ( "name", E.string "Bob" ) ]
                                        ]
                                  )
                                ]

                        after =
                            E.object
                                [ ( "users"
                                  , E.list identity
                                        [ E.object [ ( "name", E.string "Alice" ) ]
                                        , E.object [ ( "name", E.string "Charlie" ) ]
                                        ]
                                  )
                                ]

                        changedPaths =
                            Diff.compareStates before after
                    in
                    Expect.equal [ [ "users", "1", "name" ] ] changedPaths
            , test "search returns correct array paths" <|
                \_ ->
                    let
                        json =
                            E.object
                                [ ( "items"
                                  , E.list identity
                                        [ E.object [ ( "id", E.int 1 ) ]
                                        , E.object [ ( "id", E.int 2 ) ]
                                        , E.object [ ( "id", E.int 42 ) ]
                                        ]
                                  )
                                ]

                        result =
                            Search.search "42" json
                    in
                    case List.head result.matches of
                        Just path ->
                            Expect.equal [ "items", "2", "id" ] path

                        Nothing ->
                            Expect.fail "Expected a match"
            ]
        , describe "mixed object and array paths"
            [ test "handles mixed object and array nesting" <|
                \_ ->
                    let
                        json =
                            E.object
                                [ ( "users"
                                  , E.list identity
                                        [ E.object
                                            [ ( "addresses"
                                              , E.list identity
                                                    [ E.object [ ( "city", E.string "Tokyo" ) ]
                                                    , E.object [ ( "city", E.string "Paris" ) ]
                                                    ]
                                              )
                                            ]
                                        ]
                                  )
                                ]

                        result =
                            Search.search "Paris" json
                    in
                    case List.head result.matches of
                        Just path ->
                            Expect.equal [ "users", "0", "addresses", "1", "city" ] path

                        Nothing ->
                            Expect.fail "Expected a match"
            ]
        ]
