module LogParserTest exposing (suite)

{-| Unit tests for the LogParser module.

Tests cover:
- Parsing valid log entry JSON
- Handling malformed JSON gracefully
- Returning EmptyFile error for empty content
- Handling alternative field names (modelBefore/stateBefore/before)
- Correctly counting skipped malformed entries
- Parsing both `{"entries": [...]}` and `[...]` formats

-}

import Expect
import Json.Encode as E
import LogParser exposing (..)
import Test exposing (..)


suite : Test
suite =
    describe "LogParser"
        [ describe "parseLogFile"
            [ test "returns EmptyFile error for empty string" <|
                \_ ->
                    parseLogFile ""
                        |> Expect.equal (Err EmptyFile)
            , test "returns EmptyFile error for whitespace only" <|
                \_ ->
                    parseLogFile "   \n  "
                        |> Expect.equal (Err EmptyFile)
            , test "parses valid entries format" <|
                \_ ->
                    let
                        json =
                            """{"entries": [{"timestamp": 123, "message": {"name": "Test", "payload": null}, "modelBefore": null, "modelAfter": {}, "effects": []}]}"""
                    in
                    case parseLogFile json of
                        Ok ( entries, skipped ) ->
                            Expect.all
                                [ \_ -> Expect.equal 1 (List.length entries)
                                , \_ -> Expect.equal 0 skipped
                                ]
                                ()

                        Err _ ->
                            Expect.fail "Expected successful parse"
            , test "parses array format" <|
                \_ ->
                    let
                        json =
                            """[{"timestamp": 123, "message": {"name": "Test", "payload": null}, "modelBefore": null, "modelAfter": {}, "effects": []}]"""
                    in
                    case parseLogFile json of
                        Ok ( entries, _ ) ->
                            Expect.equal 1 (List.length entries)

                        Err _ ->
                            Expect.fail "Expected successful parse"
            , test "skips malformed entries and counts them" <|
                \_ ->
                    let
                        json =
                            """{"entries": [{"timestamp": 123, "message": {"name": "Valid"}, "modelAfter": {}}, {"invalid": true}]}"""
                    in
                    case parseLogFile json of
                        Ok ( entries, skipped ) ->
                            Expect.all
                                [ \_ -> Expect.equal 1 (List.length entries)
                                , \_ -> Expect.equal 1 skipped
                                ]
                                ()

                        Err _ ->
                            Expect.fail "Expected successful parse with skipped entries"
            , test "returns InvalidJson for completely invalid JSON" <|
                \_ ->
                    case parseLogFile "not json at all" of
                        Err (InvalidJson _) ->
                            Expect.pass

                        _ ->
                            Expect.fail "Expected InvalidJson error"
            , test "parses multiple valid entries" <|
                \_ ->
                    let
                        json =
                            """{"entries": [
                                {"timestamp": 100, "message": {"name": "First"}, "modelAfter": {}},
                                {"timestamp": 200, "message": {"name": "Second"}, "modelAfter": {}},
                                {"timestamp": 300, "message": {"name": "Third"}, "modelAfter": {}}
                            ]}"""
                    in
                    case parseLogFile json of
                        Ok ( entries, skipped ) ->
                            Expect.all
                                [ \_ -> Expect.equal 3 (List.length entries)
                                , \_ -> Expect.equal 0 skipped
                                ]
                                ()

                        Err _ ->
                            Expect.fail "Expected successful parse of multiple entries"
            ]
        , describe "alternative field names"
            [ test "parses stateBefore instead of modelBefore" <|
                \_ ->
                    let
                        json =
                            """{"entries": [{"timestamp": 123, "message": {"name": "Test"}, "stateBefore": {"old": true}, "modelAfter": {}}]}"""
                    in
                    case parseLogFile json of
                        Ok ( entries, _ ) ->
                            Expect.equal 1 (List.length entries)

                        Err _ ->
                            Expect.fail "Expected successful parse with stateBefore"
            , test "parses 'before' field" <|
                \_ ->
                    let
                        json =
                            """{"entries": [{"timestamp": 123, "message": {"name": "Test"}, "before": {"old": true}, "after": {}}]}"""
                    in
                    case parseLogFile json of
                        Ok ( entries, _ ) ->
                            Expect.equal 1 (List.length entries)

                        Err _ ->
                            Expect.fail "Expected successful parse with before/after"
            , test "parses message with type field instead of name" <|
                \_ ->
                    let
                        json =
                            """{"entries": [{"timestamp": 123, "message": {"type": "ButtonClicked"}, "modelAfter": {}}]}"""
                    in
                    case parseLogFile json of
                        Ok ( entries, _ ) ->
                            case List.head entries of
                                Just entry ->
                                    Expect.equal "ButtonClicked" entry.message.name

                                Nothing ->
                                    Expect.fail "Expected at least one entry"

                        Err _ ->
                            Expect.fail "Expected successful parse with type field"
            ]
        , describe "effects parsing"
            [ test "parses effects list correctly" <|
                \_ ->
                    let
                        json =
                            """{"entries": [{"timestamp": 123, "message": {"name": "Test"}, "modelAfter": {}, "effects": [{"name": "Http", "data": {"url": "test"}}]}]}"""
                    in
                    case parseLogFile json of
                        Ok ( entries, _ ) ->
                            case List.head entries of
                                Just entry ->
                                    Expect.equal 1 (List.length entry.effects)

                                Nothing ->
                                    Expect.fail "Expected at least one entry"

                        Err _ ->
                            Expect.fail "Expected successful parse with effects"
            , test "handles empty effects list" <|
                \_ ->
                    let
                        json =
                            """{"entries": [{"timestamp": 123, "message": {"name": "Test"}, "modelAfter": {}, "effects": []}]}"""
                    in
                    case parseLogFile json of
                        Ok ( entries, _ ) ->
                            case List.head entries of
                                Just entry ->
                                    Expect.equal 0 (List.length entry.effects)

                                Nothing ->
                                    Expect.fail "Expected at least one entry"

                        Err _ ->
                            Expect.fail "Expected successful parse"
            , test "handles missing effects field" <|
                \_ ->
                    let
                        json =
                            """{"entries": [{"timestamp": 123, "message": {"name": "Test"}, "modelAfter": {}}]}"""
                    in
                    case parseLogFile json of
                        Ok ( entries, _ ) ->
                            case List.head entries of
                                Just entry ->
                                    Expect.equal 0 (List.length entry.effects)

                                Nothing ->
                                    Expect.fail "Expected at least one entry"

                        Err _ ->
                            Expect.fail "Expected successful parse"
            ]
        ]
