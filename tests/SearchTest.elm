module SearchTest exposing (suite)

{-| Unit tests for the Search module.

Tests cover:
- Finding matches in field names (case-insensitive)
- Finding matches in string values
- Finding matches in number values (converted to string)
- Finding matches in boolean values
- Returning empty result for no matches
- Correctly building visible paths for parent nodes

-}

import Expect
import Json.Encode as E
import Search exposing (..)
import Set
import Test exposing (..)


suite : Test
suite =
    describe "Search"
        [ describe "search"
            [ test "returns empty result for empty query" <|
                \_ ->
                    let
                        json =
                            E.object [ ( "name", E.string "test" ) ]

                        result =
                            search "" json
                    in
                    Expect.all
                        [ \_ -> Expect.equal [] result.matches
                        , \_ -> Expect.equal 0 result.matchCount
                        ]
                        ()
            , test "returns empty result for whitespace-only query" <|
                \_ ->
                    let
                        json =
                            E.object [ ( "name", E.string "test" ) ]

                        result =
                            search "   " json
                    in
                    Expect.equal 0 result.matchCount
            , test "finds match in field name" <|
                \_ ->
                    let
                        json =
                            E.object [ ( "userName", E.string "alice" ) ]

                        result =
                            search "user" json
                    in
                    Expect.equal 1 result.matchCount
            , test "finds match in string value" <|
                \_ ->
                    let
                        json =
                            E.object [ ( "greeting", E.string "Hello World" ) ]

                        result =
                            search "world" json
                    in
                    Expect.equal 1 result.matchCount
            , test "search is case-insensitive for field names" <|
                \_ ->
                    let
                        json =
                            E.object [ ( "UserName", E.string "test" ) ]

                        result =
                            search "username" json
                    in
                    Expect.equal 1 result.matchCount
            , test "search is case-insensitive for values" <|
                \_ ->
                    let
                        json =
                            E.object [ ( "message", E.string "HELLO" ) ]

                        result =
                            search "hello" json
                    in
                    Expect.equal 1 result.matchCount
            ]
        , describe "numeric and boolean values"
            [ test "finds match in number value" <|
                \_ ->
                    let
                        json =
                            E.object [ ( "count", E.int 42 ) ]

                        result =
                            search "42" json
                    in
                    Expect.equal 1 result.matchCount
            , test "finds match in float value" <|
                \_ ->
                    let
                        json =
                            E.object [ ( "price", E.float 19.99 ) ]

                        result =
                            search "19.99" json
                    in
                    Expect.equal 1 result.matchCount
            , test "finds match in boolean true" <|
                \_ ->
                    let
                        json =
                            E.object [ ( "active", E.bool True ) ]

                        result =
                            search "true" json
                    in
                    Expect.equal 1 result.matchCount
            , test "finds match in boolean false" <|
                \_ ->
                    let
                        json =
                            E.object [ ( "active", E.bool False ) ]

                        result =
                            search "false" json
                    in
                    Expect.equal 1 result.matchCount
            , test "finds match in null value" <|
                \_ ->
                    let
                        json =
                            E.object [ ( "data", E.null ) ]

                        result =
                            search "null" json
                    in
                    Expect.equal 1 result.matchCount
            ]
        , describe "no matches"
            [ test "returns empty for no matches" <|
                \_ ->
                    let
                        json =
                            E.object
                                [ ( "name", E.string "alice" )
                                , ( "age", E.int 25 )
                                ]

                        result =
                            search "xyz" json
                    in
                    Expect.all
                        [ \_ -> Expect.equal [] result.matches
                        , \_ -> Expect.equal 0 result.matchCount
                        , \_ -> Expect.equal True (Set.isEmpty result.pathsWithMatches)
                        ]
                        ()
            ]
        , describe "nested structures"
            [ test "finds match in nested object field name" <|
                \_ ->
                    let
                        json =
                            E.object
                                [ ( "user"
                                  , E.object [ ( "profileName", E.string "test" ) ]
                                  )
                                ]

                        result =
                            search "profile" json
                    in
                    Expect.equal 1 result.matchCount
            , test "finds match in nested object value" <|
                \_ ->
                    let
                        json =
                            E.object
                                [ ( "user"
                                  , E.object [ ( "name", E.string "alice" ) ]
                                  )
                                ]

                        result =
                            search "alice" json
                    in
                    Expect.equal 1 result.matchCount
            , test "returns correct path for nested match" <|
                \_ ->
                    let
                        json =
                            E.object
                                [ ( "user"
                                  , E.object
                                        [ ( "profile"
                                          , E.object [ ( "displayName", E.string "test" ) ]
                                          )
                                        ]
                                  )
                                ]

                        result =
                            search "displayName" json
                    in
                    case List.head result.matches of
                        Just path ->
                            Expect.equal [ "user", "profile", "displayName" ] path

                        Nothing ->
                            Expect.fail "Expected at least one match"
            , test "finds multiple matches across structure" <|
                \_ ->
                    let
                        json =
                            E.object
                                [ ( "userName", E.string "test" )
                                , ( "data"
                                  , E.object [ ( "userEmail", E.string "test@example.com" ) ]
                                  )
                                ]

                        result =
                            search "user" json
                    in
                    Expect.equal 2 result.matchCount
            ]
        , describe "array handling"
            [ test "finds match in array element" <|
                \_ ->
                    let
                        json =
                            E.object
                                [ ( "items", E.list E.string [ "apple", "banana", "cherry" ] )
                                ]

                        result =
                            search "banana" json
                    in
                    Expect.equal 1 result.matchCount
            , test "returns correct path for array match" <|
                \_ ->
                    let
                        json =
                            E.object
                                [ ( "items", E.list E.string [ "apple", "banana" ] )
                                ]

                        result =
                            search "banana" json
                    in
                    case List.head result.matches of
                        Just path ->
                            Expect.equal [ "items", "1" ] path

                        Nothing ->
                            Expect.fail "Expected at least one match"
            ]
        , describe "matchesQuery"
            [ test "returns true for substring match" <|
                \_ ->
                    matchesQuery "user" "currentUser"
                        |> Expect.equal True
            , test "returns true for prefix match" <|
                \_ ->
                    matchesQuery "first" "firstName"
                        |> Expect.equal True
            , test "returns false for no match" <|
                \_ ->
                    matchesQuery "email" "username"
                        |> Expect.equal False
            , test "is case-insensitive" <|
                \_ ->
                    matchesQuery "USER" "userName"
                        |> Expect.equal True
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
            , test "handles array indices in path" <|
                \_ ->
                    pathToString [ "items", "0", "name" ]
                        |> Expect.equal "items.0.name"
            ]
        , describe "buildVisiblePaths"
            [ test "includes all ancestors of matching paths" <|
                \_ ->
                    let
                        matches =
                            [ [ "user", "profile", "name" ] ]

                        visible =
                            buildVisiblePaths matches
                    in
                    Expect.all
                        [ \_ -> Expect.equal True (Set.member "" visible)
                        , \_ -> Expect.equal True (Set.member "user" visible)
                        , \_ -> Expect.equal True (Set.member "user.profile" visible)
                        , \_ -> Expect.equal True (Set.member "user.profile.name" visible)
                        ]
                        ()
            , test "handles multiple matching paths" <|
                \_ ->
                    let
                        matches =
                            [ [ "user", "name" ]
                            , [ "settings", "theme" ]
                            ]

                        visible =
                            buildVisiblePaths matches
                    in
                    Expect.all
                        [ \_ -> Expect.equal True (Set.member "user" visible)
                        , \_ -> Expect.equal True (Set.member "user.name" visible)
                        , \_ -> Expect.equal True (Set.member "settings" visible)
                        , \_ -> Expect.equal True (Set.member "settings.theme" visible)
                        ]
                        ()
            , test "returns empty set for no matches" <|
                \_ ->
                    buildVisiblePaths []
                        |> Set.isEmpty
                        |> Expect.equal True
            ]
        ]
