module CompressionDictTest exposing (suite)

{-| Unit tests for the CompressionDict module.

Tests cover:

  - decompressString with Disabled returns unchanged
  - decompressString with Enabled looks up @N references
  - decompressString passes through non-matching strings
  - decompressString passes through unknown references
  - merge builds dictionary correctly
  - merge accumulates across multiple stringDict entries
  - decompressValue decompresses object keys and string values
  - decompressValue handles nested structures

-}

import CompressionDict exposing (Compression(..), decompressString, decompressValue, empty, merge)
import Dict
import Expect
import Json.Decode as D
import Json.Encode as E
import Test exposing (..)


suite : Test
suite =
    describe "CompressionDict"
        [ describe "empty"
            [ test "returns Disabled" <|
                \_ ->
                    Expect.equal Disabled empty
            ]
        , describe "decompressString"
            [ test "with Disabled returns string unchanged" <|
                \_ ->
                    decompressString Disabled "@0"
                        |> Expect.equal "@0"
            , test "with Disabled returns regular string unchanged" <|
                \_ ->
                    decompressString Disabled "hello"
                        |> Expect.equal "hello"
            , test "with Enabled looks up @N reference" <|
                \_ ->
                    let
                        dict =
                            Dict.fromList [ ( "0", "model" ), ( "1", "Message.Click" ) ]
                    in
                    decompressString (Enabled dict) "@0"
                        |> Expect.equal "model"
            , test "with Enabled looks up multi-digit reference" <|
                \_ ->
                    let
                        dict =
                            Dict.fromList [ ( "123", "longKey" ) ]
                    in
                    decompressString (Enabled dict) "@123"
                        |> Expect.equal "longKey"
            , test "passes through non-matching strings" <|
                \_ ->
                    let
                        dict =
                            Dict.fromList [ ( "0", "model" ) ]
                    in
                    decompressString (Enabled dict) "hello"
                        |> Expect.equal "hello"
            , test "passes through partial @ patterns" <|
                \_ ->
                    let
                        dict =
                            Dict.fromList [ ( "0", "model" ) ]
                    in
                    decompressString (Enabled dict) "@abc"
                        |> Expect.equal "@abc"
            , test "passes through @ without number" <|
                \_ ->
                    let
                        dict =
                            Dict.fromList [ ( "0", "model" ) ]
                    in
                    decompressString (Enabled dict) "@"
                        |> Expect.equal "@"
            , test "passes through unknown references" <|
                \_ ->
                    let
                        dict =
                            Dict.fromList [ ( "0", "model" ) ]
                    in
                    decompressString (Enabled dict) "@99"
                        |> Expect.equal "@99"
            , test "handles @0 reference correctly" <|
                \_ ->
                    let
                        dict =
                            Dict.fromList [ ( "0", "firstKey" ) ]
                    in
                    decompressString (Enabled dict) "@0"
                        |> Expect.equal "firstKey"
            ]
        , describe "merge"
            [ test "builds dictionary from stringDict entry" <|
                \_ ->
                    let
                        value =
                            E.object
                                [ ( "type", E.string "stringDict" )
                                , ( "strings"
                                  , E.object
                                        [ ( "0", E.string "model" )
                                        , ( "1", E.string "Message.Click" )
                                        ]
                                  )
                                ]

                        result =
                            merge value Disabled
                    in
                    case result of
                        Enabled dict ->
                            Expect.all
                                [ \_ -> Dict.get "0" dict |> Expect.equal (Just "model")
                                , \_ -> Dict.get "1" dict |> Expect.equal (Just "Message.Click")
                                ]
                                ()

                        Disabled ->
                            Expect.fail "Expected Enabled"
            , test "accumulates across multiple merges" <|
                \_ ->
                    let
                        value1 =
                            E.object
                                [ ( "strings"
                                  , E.object [ ( "0", E.string "first" ) ]
                                  )
                                ]

                        value2 =
                            E.object
                                [ ( "strings"
                                  , E.object [ ( "1", E.string "second" ) ]
                                  )
                                ]

                        result =
                            Disabled
                                |> merge value1
                                |> merge value2
                    in
                    case result of
                        Enabled dict ->
                            Expect.all
                                [ \_ -> Dict.get "0" dict |> Expect.equal (Just "first")
                                , \_ -> Dict.get "1" dict |> Expect.equal (Just "second")
                                ]
                                ()

                        Disabled ->
                            Expect.fail "Expected Enabled"
            , test "handles missing strings field gracefully" <|
                \_ ->
                    let
                        value =
                            E.object [ ( "type", E.string "stringDict" ) ]

                        result =
                            merge value Disabled
                    in
                    Expect.equal Disabled result
            , test "merges into existing Enabled state" <|
                \_ ->
                    let
                        existingDict =
                            Dict.fromList [ ( "0", "existing" ) ]

                        value =
                            E.object
                                [ ( "strings"
                                  , E.object [ ( "1", E.string "new" ) ]
                                  )
                                ]

                        result =
                            merge value (Enabled existingDict)
                    in
                    case result of
                        Enabled dict ->
                            Expect.all
                                [ \_ -> Dict.get "0" dict |> Expect.equal (Just "existing")
                                , \_ -> Dict.get "1" dict |> Expect.equal (Just "new")
                                ]
                                ()

                        Disabled ->
                            Expect.fail "Expected Enabled"
            ]
        , describe "decompressValue"
            [ test "with Disabled returns value unchanged" <|
                \_ ->
                    let
                        value =
                            E.object [ ( "@0", E.string "@1" ) ]

                        result =
                            decompressValue Disabled value
                    in
                    Expect.equal (E.encode 0 value) (E.encode 0 result)
            , test "decompresses object keys" <|
                \_ ->
                    let
                        dict =
                            Dict.fromList [ ( "0", "model" ) ]

                        value =
                            E.object [ ( "@0", E.int 42 ) ]

                        result =
                            decompressValue (Enabled dict) value

                        decoded =
                            D.decodeValue (D.field "model" D.int) result
                    in
                    Expect.equal (Ok 42) decoded
            , test "decompresses string values" <|
                \_ ->
                    let
                        dict =
                            Dict.fromList [ ( "0", "hello" ) ]

                        value =
                            E.object [ ( "key", E.string "@0" ) ]

                        result =
                            decompressValue (Enabled dict) value

                        decoded =
                            D.decodeValue (D.field "key" D.string) result
                    in
                    Expect.equal (Ok "hello") decoded
            , test "decompresses nested objects" <|
                \_ ->
                    let
                        dict =
                            Dict.fromList [ ( "0", "outer" ), ( "1", "inner" ) ]

                        value =
                            E.object
                                [ ( "@0"
                                  , E.object [ ( "@1", E.int 123 ) ]
                                  )
                                ]

                        result =
                            decompressValue (Enabled dict) value

                        decoded =
                            D.decodeValue
                                (D.field "outer" (D.field "inner" D.int))
                                result
                    in
                    Expect.equal (Ok 123) decoded
            , test "decompresses array elements" <|
                \_ ->
                    let
                        dict =
                            Dict.fromList [ ( "0", "first" ), ( "1", "second" ) ]

                        value =
                            E.list E.string [ "@0", "@1", "literal" ]

                        result =
                            decompressValue (Enabled dict) value

                        decoded =
                            D.decodeValue (D.list D.string) result
                    in
                    Expect.equal (Ok [ "first", "second", "literal" ]) decoded
            , test "preserves non-string primitives" <|
                \_ ->
                    let
                        dict =
                            Dict.fromList [ ( "0", "key" ) ]

                        value =
                            E.object
                                [ ( "@0", E.int 42 )
                                , ( "bool", E.bool True )
                                , ( "null", E.null )
                                , ( "float", E.float 3.14 )
                                ]

                        result =
                            decompressValue (Enabled dict) value

                        keyDecoded =
                            D.decodeValue (D.field "key" D.int) result

                        boolDecoded =
                            D.decodeValue (D.field "bool" D.bool) result

                        nullDecoded =
                            D.decodeValue (D.field "null" (D.null ())) result

                        floatDecoded =
                            D.decodeValue (D.field "float" D.float) result
                    in
                    Expect.all
                        [ \_ -> Expect.equal (Ok 42) keyDecoded
                        , \_ -> Expect.equal (Ok True) boolDecoded
                        , \_ -> Expect.equal (Ok ()) nullDecoded
                        , \_ -> Expect.equal (Ok 3.14) floatDecoded
                        ]
                        ()
            ]
        ]
