module FilterTest exposing (suite)

{-| Unit tests for the Filter module.

Tests cover:

  - fuzzyMatch: empty query, empty target, case-insensitive, spec examples
  - matchFieldValue: dot-path navigation, wildcard `*` key
  - deepValueSearch: nested JSON structures
  - matchesEntry: multiple filter AND logic, ErrorEntry/InitEntry always pass

-}

import Expect
import Filter
    exposing
        ( ActiveFilter
        , Filter(..)
        , FilterStatus(..)
        , deepValueSearch
        , fuzzyMatch
        , matchesEntry
        )
import Json.Encode as E
import Test exposing (..)
import Types
    exposing
        ( LogEntry(..)
        )


suite : Test
suite =
    describe "Filter"
        [ fuzzyMatchSuite
        , matchFieldValueSuite
        , deepValueSearchSuite
        , matchesEntrySuite
        , primitiveValueToStringSuite
        ]



-- FUZZY MATCH TESTS


fuzzyMatchSuite : Test
fuzzyMatchSuite =
    describe "fuzzyMatch"
        [ test "empty query matches any target" <|
            \_ ->
                fuzzyMatch "" "anything"
                    |> Expect.equal True
        , test "empty query matches empty target" <|
            \_ ->
                fuzzyMatch "" ""
                    |> Expect.equal True
        , test "non-empty query does not match empty target" <|
            \_ ->
                fuzzyMatch "a" ""
                    |> Expect.equal False
        , test "exact match works" <|
            \_ ->
                fuzzyMatch "hello" "hello"
                    |> Expect.equal True
        , test "case-insensitive match" <|
            \_ ->
                fuzzyMatch "abc" "ABC"
                    |> Expect.equal True
        , test "case-insensitive match (mixed case query)" <|
            \_ ->
                fuzzyMatch "AbC" "abc"
                    |> Expect.equal True
        , test "spec example: SysDr matches InputSubsystem.Message.DriverControl" <|
            \_ ->
                fuzzyMatch "SysDr" "InputSubsystem.Message.DriverControl"
                    |> Expect.equal True
        , test "spec example: smdc matches InputSubsystem.Message.DriverControl" <|
            \_ ->
                fuzzyMatch "smdc" "InputSubsystem.Message.DriverControl"
                    |> Expect.equal True
        , test "characters must appear in order" <|
            \_ ->
                fuzzyMatch "ba" "abc"
                    |> Expect.equal False
        , test "substring match works" <|
            \_ ->
                fuzzyMatch "sub" "InputSubsystem"
                    |> Expect.equal True
        , test "single character match" <|
            \_ ->
                fuzzyMatch "x" "example"
                    |> Expect.equal True
        , test "no match when character not present" <|
            \_ ->
                fuzzyMatch "z" "hello"
                    |> Expect.equal False
        , test "repeated characters in query" <|
            \_ ->
                fuzzyMatch "aa" "abac"
                    |> Expect.equal True
        , test "repeated characters not enough in target" <|
            \_ ->
                fuzzyMatch "aa" "abc"
                    |> Expect.equal False
        ]



-- MATCH FIELD VALUE TESTS


matchFieldValueSuite : Test
matchFieldValueSuite =
    describe "matchFieldValue (via matchesEntry with MessageFieldFilter)"
        [ test "matches simple field by key and value" <|
            \_ ->
                let
                    entry =
                        makeUpdateEntry "TestMsg"
                            (E.object [ ( "name", E.string "hello" ) ])
                            E.null
                            E.null
                in
                matchesEntry [ MessageFieldFilter { key = "name", value = "hello" } ] entry
                    |> Expect.equal True
        , test "does not match when value differs" <|
            \_ ->
                let
                    entry =
                        makeUpdateEntry "TestMsg"
                            (E.object [ ( "name", E.string "world" ) ])
                            E.null
                            E.null
                in
                matchesEntry [ MessageFieldFilter { key = "name", value = "hello" } ] entry
                    |> Expect.equal False
        , test "matches dot-path navigation" <|
            \_ ->
                let
                    payload =
                        E.object
                            [ ( "user"
                              , E.object
                                    [ ( "name", E.string "Alice" ) ]
                              )
                            ]

                    entry =
                        makeUpdateEntry "TestMsg" payload E.null E.null
                in
                matchesEntry [ MessageFieldFilter { key = "user.name", value = "Alice" } ] entry
                    |> Expect.equal True
        , test "dot-path does not match when path does not exist" <|
            \_ ->
                let
                    payload =
                        E.object
                            [ ( "user"
                              , E.object
                                    [ ( "name", E.string "Alice" ) ]
                              )
                            ]

                    entry =
                        makeUpdateEntry "TestMsg" payload E.null E.null
                in
                matchesEntry [ MessageFieldFilter { key = "user.email", value = "Alice" } ] entry
                    |> Expect.equal False
        , test "wildcard * key searches all fields" <|
            \_ ->
                let
                    payload =
                        E.object
                            [ ( "id", E.int 42 )
                            , ( "name", E.string "test" )
                            ]

                    entry =
                        makeUpdateEntry "TestMsg" payload E.null E.null
                in
                matchesEntry [ MessageFieldFilter { key = "*", value = "test" } ] entry
                    |> Expect.equal True
        , test "wildcard * key deep searches nested values" <|
            \_ ->
                let
                    payload =
                        E.object
                            [ ( "outer"
                              , E.object
                                    [ ( "inner", E.string "deep-value" ) ]
                              )
                            ]

                    entry =
                        makeUpdateEntry "TestMsg" payload E.null E.null
                in
                matchesEntry [ MessageFieldFilter { key = "*", value = "deep-value" } ] entry
                    |> Expect.equal True
        , test "value matching is case-insensitive" <|
            \_ ->
                let
                    entry =
                        makeUpdateEntry "TestMsg"
                            (E.object [ ( "name", E.string "Hello" ) ])
                            E.null
                            E.null
                in
                matchesEntry [ MessageFieldFilter { key = "name", value = "hello" } ] entry
                    |> Expect.equal True
        , test "matches numeric value as substring" <|
            \_ ->
                let
                    entry =
                        makeUpdateEntry "TestMsg"
                            (E.object [ ( "count", E.int 42 ) ])
                            E.null
                            E.null
                in
                matchesEntry [ MessageFieldFilter { key = "count", value = "42" } ] entry
                    |> Expect.equal True
        ]



-- DEEP VALUE SEARCH TESTS


deepValueSearchSuite : Test
deepValueSearchSuite =
    describe "deepValueSearch"
        [ test "finds string value at top level" <|
            \_ ->
                let
                    json =
                        E.object [ ( "name", E.string "hello" ) ]
                in
                deepValueSearch "hello" json
                    |> Expect.equal True
        , test "finds string value in nested object" <|
            \_ ->
                let
                    json =
                        E.object
                            [ ( "level1"
                              , E.object
                                    [ ( "level2"
                                      , E.object
                                            [ ( "target", E.string "found-it" ) ]
                                      )
                                    ]
                              )
                            ]
                in
                deepValueSearch "found-it" json
                    |> Expect.equal True
        , test "finds value in array" <|
            \_ ->
                let
                    json =
                        E.object
                            [ ( "items", E.list E.string [ "alpha", "beta", "gamma" ] ) ]
                in
                deepValueSearch "beta" json
                    |> Expect.equal True
        , test "finds numeric value" <|
            \_ ->
                let
                    json =
                        E.object [ ( "count", E.int 99 ) ]
                in
                deepValueSearch "99" json
                    |> Expect.equal True
        , test "finds boolean value" <|
            \_ ->
                let
                    json =
                        E.object [ ( "active", E.bool True ) ]
                in
                deepValueSearch "true" json
                    |> Expect.equal True
        , test "finds null value" <|
            \_ ->
                let
                    json =
                        E.object [ ( "value", E.null ) ]
                in
                deepValueSearch "null" json
                    |> Expect.equal True
        , test "case-insensitive search" <|
            \_ ->
                let
                    json =
                        E.object [ ( "name", E.string "Hello World" ) ]
                in
                deepValueSearch "hello" json
                    |> Expect.equal True
        , test "returns false when value not found" <|
            \_ ->
                let
                    json =
                        E.object
                            [ ( "name", E.string "alice" )
                            , ( "age", E.int 25 )
                            ]
                in
                deepValueSearch "bob" json
                    |> Expect.equal False
        , test "finds value in deeply nested array within object" <|
            \_ ->
                let
                    json =
                        E.object
                            [ ( "data"
                              , E.object
                                    [ ( "records"
                                      , E.list identity
                                            [ E.object [ ( "id", E.int 1 ), ( "value", E.string "needle" ) ]
                                            , E.object [ ( "id", E.int 2 ), ( "value", E.string "other" ) ]
                                            ]
                                      )
                                    ]
                              )
                            ]
                in
                deepValueSearch "needle" json
                    |> Expect.equal True
        , test "substring match works" <|
            \_ ->
                let
                    json =
                        E.object [ ( "message", E.string "error: something went wrong" ) ]
                in
                deepValueSearch "something" json
                    |> Expect.equal True
        ]



-- MATCHES ENTRY TESTS


matchesEntrySuite : Test
matchesEntrySuite =
    describe "matchesEntry"
        [ describe "ErrorEntry always passes"
            [ test "ErrorEntry passes with any filters" <|
                \_ ->
                    let
                        entry =
                            ErrorEntry { lineNumber = 1, rawText = "bad", error = "parse error" }
                    in
                    matchesEntry [ MessageNameFilter { query = "xyz" } ] entry
                        |> Expect.equal True
            ]
        , describe "InitEntry always passes"
            [ test "InitEntry passes with any filters" <|
                \_ ->
                    let
                        entry =
                            InitEntry { timestamp = 0, model = E.null, effects = [] }
                    in
                    matchesEntry [ MessageNameFilter { query = "xyz" }, HasEffectsFilter ] entry
                        |> Expect.equal True
            ]
        , describe "MessageNameFilter"
            [ test "matches by fuzzy name" <|
                \_ ->
                    let
                        entry =
                            makeUpdateEntry "InputSubsystem.Message.DriverControl" E.null E.null E.null
                    in
                    matchesEntry [ MessageNameFilter { query = "SysDr" } ] entry
                        |> Expect.equal True
            , test "does not match non-matching name" <|
                \_ ->
                    let
                        entry =
                            makeUpdateEntry "SomeOtherMessage" E.null E.null E.null
                    in
                    matchesEntry [ MessageNameFilter { query = "xyz" } ] entry
                        |> Expect.equal False
            ]
        , describe "ModelChangedFilter"
            [ test "matches when model changed" <|
                \_ ->
                    let
                        before =
                            E.object [ ( "count", E.int 1 ) ]

                        after =
                            E.object [ ( "count", E.int 2 ) ]

                        entry =
                            makeUpdateEntry "Msg" E.null before after
                    in
                    matchesEntry [ ModelChangedFilter ] entry
                        |> Expect.equal True
            , test "does not match when model unchanged" <|
                \_ ->
                    let
                        model =
                            E.object [ ( "count", E.int 1 ) ]

                        entry =
                            makeUpdateEntry "Msg" E.null model model
                    in
                    matchesEntry [ ModelChangedFilter ] entry
                        |> Expect.equal False
            ]
        , describe "HasEffectsFilter"
            [ test "matches when effects exist" <|
                \_ ->
                    let
                        entry =
                            UpdateEntry
                                { timestamp = 0
                                , message = { name = "Msg", payload = E.null }
                                , modelBefore = E.null
                                , modelAfter = E.null
                                , effects = [ { name = "HttpRequest", data = E.null } ]
                                }
                    in
                    matchesEntry [ HasEffectsFilter ] entry
                        |> Expect.equal True
            , test "does not match when no effects" <|
                \_ ->
                    let
                        entry =
                            makeUpdateEntry "Msg" E.null E.null E.null
                    in
                    matchesEntry [ HasEffectsFilter ] entry
                        |> Expect.equal False
            ]
        , describe "ModelValueFilter"
            [ test "matches model value with specific key" <|
                \_ ->
                    let
                        modelAfter =
                            E.object [ ( "status", E.string "active" ) ]

                        entry =
                            makeUpdateEntry "Msg" E.null E.null modelAfter
                    in
                    matchesEntry [ ModelValueFilter { key = "status", value = "active" } ] entry
                        |> Expect.equal True
            , test "matches model value with wildcard key" <|
                \_ ->
                    let
                        modelAfter =
                            E.object
                                [ ( "nested"
                                  , E.object [ ( "deep", E.string "target" ) ]
                                  )
                                ]

                        entry =
                            makeUpdateEntry "Msg" E.null E.null modelAfter
                    in
                    matchesEntry [ ModelValueFilter { key = "*", value = "target" } ] entry
                        |> Expect.equal True
            ]
        , describe "EffectNameFilter"
            [ test "matches effect by fuzzy name" <|
                \_ ->
                    let
                        entry =
                            UpdateEntry
                                { timestamp = 0
                                , message = { name = "Msg", payload = E.null }
                                , modelBefore = E.null
                                , modelAfter = E.null
                                , effects = [ { name = "HttpRequest.Send", data = E.null } ]
                                }
                    in
                    matchesEntry [ EffectNameFilter { query = "Http" } ] entry
                        |> Expect.equal True
            , test "does not match wrong effect name" <|
                \_ ->
                    let
                        entry =
                            UpdateEntry
                                { timestamp = 0
                                , message = { name = "Msg", payload = E.null }
                                , modelBefore = E.null
                                , modelAfter = E.null
                                , effects = [ { name = "Navigation.Push", data = E.null } ]
                                }
                    in
                    matchesEntry [ EffectNameFilter { query = "Http" } ] entry
                        |> Expect.equal False
            ]
        , describe "AND logic with multiple filters"
            [ test "all filters must match (both match)" <|
                \_ ->
                    let
                        before =
                            E.object [ ( "count", E.int 1 ) ]

                        after =
                            E.object [ ( "count", E.int 2 ) ]

                        entry =
                            makeUpdateEntry "Increment" E.null before after
                    in
                    matchesEntry
                        [ MessageNameFilter { query = "Inc" }
                        , ModelChangedFilter
                        ]
                        entry
                        |> Expect.equal True
            , test "all filters must match (one does not match)" <|
                \_ ->
                    let
                        model =
                            E.object [ ( "count", E.int 1 ) ]

                        entry =
                            makeUpdateEntry "Increment" E.null model model
                    in
                    matchesEntry
                        [ MessageNameFilter { query = "Inc" }
                        , ModelChangedFilter
                        ]
                        entry
                        |> Expect.equal False
            , test "empty filter list matches everything" <|
                \_ ->
                    let
                        entry =
                            makeUpdateEntry "AnyMessage" E.null E.null E.null
                    in
                    matchesEntry [] entry
                        |> Expect.equal True
            , test "three filters AND logic" <|
                \_ ->
                    let
                        before =
                            E.object [ ( "count", E.int 1 ) ]

                        after =
                            E.object [ ( "count", E.int 2 ) ]

                        entry =
                            UpdateEntry
                                { timestamp = 0
                                , message =
                                    { name = "Increment"
                                    , payload = E.object [ ( "step", E.int 1 ) ]
                                    }
                                , modelBefore = before
                                , modelAfter = after
                                , effects = [ { name = "Log", data = E.null } ]
                                }
                    in
                    matchesEntry
                        [ MessageNameFilter { query = "Inc" }
                        , ModelChangedFilter
                        , HasEffectsFilter
                        ]
                        entry
                        |> Expect.equal True
            ]
        , describe "SubscriptionChangeEntry handling"
            [ test "message name filter excludes subscription entries" <|
                \_ ->
                    let
                        entry =
                            SubscriptionChangeEntry
                                { timestamp = 0
                                , started = [ E.object [ ( "name", E.string "Timer" ) ] ]
                                , stopped = []
                                }
                    in
                    matchesEntry [ MessageNameFilter { query = "Timer" } ] entry
                        |> Expect.equal False
            , test "subscription name filter matches subscription entries" <|
                \_ ->
                    let
                        entry =
                            SubscriptionChangeEntry
                                { timestamp = 0
                                , started = [ E.object [ ( "name", E.string "Timer.Tick" ) ] ]
                                , stopped = []
                                }
                    in
                    matchesEntry [ SubscriptionNameFilter { query = "Timer" } ] entry
                        |> Expect.equal True
            ]
        , describe "ModelFieldChangedFilter"
            [ test "matches when specific field changed" <|
                \_ ->
                    let
                        before =
                            E.object [ ( "user", E.object [ ( "name", E.string "Alice" ) ] ) ]

                        after =
                            E.object [ ( "user", E.object [ ( "name", E.string "Bob" ) ] ) ]

                        entry =
                            makeUpdateEntry "Msg" E.null before after
                    in
                    matchesEntry [ ModelFieldChangedFilter { fieldPath = "user.name" } ] entry
                        |> Expect.equal True
            , test "matches when parent field specified and child changed" <|
                \_ ->
                    let
                        before =
                            E.object [ ( "user", E.object [ ( "name", E.string "Alice" ) ] ) ]

                        after =
                            E.object [ ( "user", E.object [ ( "name", E.string "Bob" ) ] ) ]

                        entry =
                            makeUpdateEntry "Msg" E.null before after
                    in
                    matchesEntry [ ModelFieldChangedFilter { fieldPath = "user" } ] entry
                        |> Expect.equal True
            , test "does not match when different field changed" <|
                \_ ->
                    let
                        before =
                            E.object
                                [ ( "user", E.object [ ( "name", E.string "Alice" ) ] )
                                , ( "count", E.int 1 )
                                ]

                        after =
                            E.object
                                [ ( "user", E.object [ ( "name", E.string "Alice" ) ] )
                                , ( "count", E.int 2 )
                                ]

                        entry =
                            makeUpdateEntry "Msg" E.null before after
                    in
                    matchesEntry [ ModelFieldChangedFilter { fieldPath = "user" } ] entry
                        |> Expect.equal False
            ]
        ]



-- PRIMITIVE VALUE TO STRING TESTS


primitiveValueToStringSuite : Test
primitiveValueToStringSuite =
    describe "primitiveValueToString"
        [ test "converts string value" <|
            \_ ->
                Filter.primitiveValueToString (E.string "hello")
                    |> Expect.equal (Just "hello")
        , test "converts int value" <|
            \_ ->
                Filter.primitiveValueToString (E.int 42)
                    |> Expect.equal (Just "42")
        , test "converts float value" <|
            \_ ->
                Filter.primitiveValueToString (E.float 3.14)
                    |> Expect.equal (Just "3.14")
        , test "converts true" <|
            \_ ->
                Filter.primitiveValueToString (E.bool True)
                    |> Expect.equal (Just "true")
        , test "converts false" <|
            \_ ->
                Filter.primitiveValueToString (E.bool False)
                    |> Expect.equal (Just "false")
        , test "converts null" <|
            \_ ->
                Filter.primitiveValueToString E.null
                    |> Expect.equal (Just "null")
        , test "returns Nothing for object" <|
            \_ ->
                Filter.primitiveValueToString (E.object [ ( "a", E.int 1 ) ])
                    |> Expect.equal Nothing
        , test "returns Nothing for array" <|
            \_ ->
                Filter.primitiveValueToString (E.list E.int [ 1, 2 ])
                    |> Expect.equal Nothing
        ]



-- TEST HELPERS


{-| Create a simple UpdateEntry for testing.
-}
makeUpdateEntry : String -> E.Value -> E.Value -> E.Value -> LogEntry
makeUpdateEntry msgName payload modelBefore modelAfter =
    UpdateEntry
        { timestamp = 0
        , message = { name = msgName, payload = payload }
        , modelBefore = modelBefore
        , modelAfter = modelAfter
        , effects = []
        }
