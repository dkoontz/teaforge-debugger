module LogParserTest exposing (suite)

{-| Unit tests for the LogParser module.

Tests cover:

  - Entry type detection
  - Header decoding (with and without compression)
  - Init entry decoding (v2 format with modelDiff)
  - Update entry decoding (v2 format with modelDiff)
  - Subscription change entry decoding
  - Effect decoding (v2 formats)

-}

import CompressionDict
import Expect
import Json.Decode as D
import Json.Encode as E
import LogParser
import Test exposing (..)
import Types exposing (Effect, MessageData)


suite : Test
suite =
    describe "LogParser"
        [ entryTypeDecoderTests
        , headerDecoderTests
        , initDataDecoderTests
        , updateDataDecoderTests
        , subscriptionChangeDataDecoderTests
        , effectDecoderTests
        ]


entryTypeDecoderTests : Test
entryTypeDecoderTests =
    describe "entryTypeDecoder"
        [ test "decodes type field" <|
            \_ ->
                let
                    json =
                        E.object [ ( "type", E.string "update" ) ]
                in
                D.decodeValue LogParser.entryTypeDecoder json
                    |> Expect.equal (Ok "update")
        , test "decodes header type" <|
            \_ ->
                let
                    json =
                        E.object [ ( "type", E.string "header" ) ]
                in
                D.decodeValue LogParser.entryTypeDecoder json
                    |> Expect.equal (Ok "header")
        , test "decodes init type" <|
            \_ ->
                let
                    json =
                        E.object [ ( "type", E.string "init" ) ]
                in
                D.decodeValue LogParser.entryTypeDecoder json
                    |> Expect.equal (Ok "init")
        , test "fails without type field" <|
            \_ ->
                let
                    json =
                        E.object [ ( "notType", E.string "update" ) ]
                in
                case D.decodeValue LogParser.entryTypeDecoder json of
                    Err _ ->
                        Expect.pass

                    Ok _ ->
                        Expect.fail "Expected decoder to fail"
        ]


headerDecoderTests : Test
headerDecoderTests =
    describe "headerDecoder"
        [ test "decodes header with version" <|
            \_ ->
                let
                    json =
                        E.object
                            [ ( "type", E.string "header" )
                            , ( "version", E.int 1 )
                            ]
                in
                case D.decodeValue LogParser.headerDecoder json of
                    Ok header ->
                        Expect.all
                            [ \_ -> Expect.equal 1 header.version
                            , \_ -> Expect.equal Nothing header.compression
                            ]
                            ()

                    Err e ->
                        Expect.fail (D.errorToString e)
        , test "decodes header with stringDict compression" <|
            \_ ->
                let
                    json =
                        E.object
                            [ ( "type", E.string "header" )
                            , ( "version", E.int 1 )
                            , ( "compression", E.string "stringDict" )
                            ]
                in
                case D.decodeValue LogParser.headerDecoder json of
                    Ok header ->
                        Expect.all
                            [ \_ -> Expect.equal 1 header.version
                            , \_ -> Expect.equal (Just "stringDict") header.compression
                            ]
                            ()

                    Err e ->
                        Expect.fail (D.errorToString e)
        , test "defaults version to 1 if missing" <|
            \_ ->
                let
                    json =
                        E.object [ ( "type", E.string "header" ) ]
                in
                case D.decodeValue LogParser.headerDecoder json of
                    Ok header ->
                        Expect.equal 1 header.version

                    Err e ->
                        Expect.fail (D.errorToString e)
        ]


initDataDecoderTests : Test
initDataDecoderTests =
    describe "initDataDecoder"
        [ test "decodes init entry with all fields" <|
            \_ ->
                let
                    json =
                        E.object
                            [ ( "type", E.string "init" )
                            , ( "timestamp", E.int 12345 )
                            , ( "modelDiff", E.list identity [] )
                            , ( "effects", E.list identity [] )
                            ]
                in
                case D.decodeValue (LogParser.initDataDecoder CompressionDict.empty) json of
                    Ok data ->
                        Expect.equal 12345 data.timestamp

                    Err e ->
                        Expect.fail (D.errorToString e)
        , test "defaults timestamp to 0 if missing" <|
            \_ ->
                let
                    json =
                        E.object
                            [ ( "type", E.string "init" )
                            , ( "modelDiff", E.list identity [] )
                            ]
                in
                case D.decodeValue (LogParser.initDataDecoder CompressionDict.empty) json of
                    Ok data ->
                        Expect.equal 0 data.timestamp

                    Err e ->
                        Expect.fail (D.errorToString e)
        , test "defaults effects to empty list if missing" <|
            \_ ->
                let
                    json =
                        E.object
                            [ ( "type", E.string "init" )
                            , ( "modelDiff", E.list identity [] )
                            ]
                in
                case D.decodeValue (LogParser.initDataDecoder CompressionDict.empty) json of
                    Ok data ->
                        Expect.equal [] data.effects

                    Err e ->
                        Expect.fail (D.errorToString e)
        , test "parses modelDiff list" <|
            \_ ->
                let
                    json =
                        E.object
                            [ ( "type", E.string "init" )
                            , ( "modelDiff"
                              , E.list identity
                                    [ E.object
                                        [ ( "op", E.string "add" )
                                        , ( "path", E.string "/counter" )
                                        , ( "value", E.int 0 )
                                        ]
                                    ]
                              )
                            ]
                in
                case D.decodeValue (LogParser.initDataDecoder CompressionDict.empty) json of
                    Ok data ->
                        Expect.equal 1 (List.length data.modelDiff)

                    Err e ->
                        Expect.fail (D.errorToString e)
        , test "parses effects list" <|
            \_ ->
                let
                    json =
                        E.object
                            [ ( "type", E.string "init" )
                            , ( "modelDiff", E.list identity [] )
                            , ( "effects"
                              , E.list identity
                                    [ E.object
                                        [ ( "_type", E.string "Http" )
                                        ]
                                    ]
                              )
                            ]
                in
                case D.decodeValue (LogParser.initDataDecoder CompressionDict.empty) json of
                    Ok data ->
                        Expect.equal 1 (List.length data.effects)

                    Err e ->
                        Expect.fail (D.errorToString e)
        ]


updateDataDecoderTests : Test
updateDataDecoderTests =
    describe "updateDataDecoder"
        [ test "decodes update entry with all fields" <|
            \_ ->
                let
                    json =
                        E.object
                            [ ( "type", E.string "update" )
                            , ( "timestamp", E.int 12345 )
                            , ( "message"
                              , E.object [ ( "_type", E.string "ButtonClicked" ) ]
                              )
                            , ( "modelDiff", E.list identity [] )
                            , ( "effects", E.list identity [] )
                            ]
                in
                case D.decodeValue (LogParser.updateDataDecoder CompressionDict.empty) json of
                    Ok data ->
                        Expect.all
                            [ \_ -> Expect.equal 12345 data.timestamp
                            , \_ -> Expect.equal "ButtonClicked" data.message.name
                            ]
                            ()

                    Err e ->
                        Expect.fail (D.errorToString e)
        , test "decodes message with _type field" <|
            \_ ->
                let
                    json =
                        E.object
                            [ ( "type", E.string "update" )
                            , ( "message"
                              , E.object [ ( "_type", E.string "Increment" ) ]
                              )
                            , ( "modelDiff", E.list identity [] )
                            ]
                in
                case D.decodeValue (LogParser.updateDataDecoder CompressionDict.empty) json of
                    Ok data ->
                        Expect.equal "Increment" data.message.name

                    Err e ->
                        Expect.fail (D.errorToString e)
        , test "decodes message with _inner field" <|
            \_ ->
                let
                    json =
                        E.object
                            [ ( "type", E.string "update" )
                            , ( "message"
                              , E.object
                                    [ ( "_type", E.string "Wrapper" )
                                    , ( "_inner"
                                      , E.object [ ( "_type", E.string "InnerMsg" ) ]
                                      )
                                    ]
                              )
                            , ( "modelDiff", E.list identity [] )
                            ]
                in
                case D.decodeValue (LogParser.updateDataDecoder CompressionDict.empty) json of
                    Ok data ->
                        Expect.equal "InnerMsg" data.message.name

                    Err e ->
                        Expect.fail (D.errorToString e)
        , test "defaults timestamp to 0 if missing" <|
            \_ ->
                let
                    json =
                        E.object
                            [ ( "type", E.string "update" )
                            , ( "message", E.object [ ( "_type", E.string "Test" ) ] )
                            , ( "modelDiff", E.list identity [] )
                            ]
                in
                case D.decodeValue (LogParser.updateDataDecoder CompressionDict.empty) json of
                    Ok data ->
                        Expect.equal 0 data.timestamp

                    Err e ->
                        Expect.fail (D.errorToString e)
        , test "defaults effects to empty list if missing" <|
            \_ ->
                let
                    json =
                        E.object
                            [ ( "type", E.string "update" )
                            , ( "message", E.object [ ( "_type", E.string "Test" ) ] )
                            , ( "modelDiff", E.list identity [] )
                            ]
                in
                case D.decodeValue (LogParser.updateDataDecoder CompressionDict.empty) json of
                    Ok data ->
                        Expect.equal [] data.effects

                    Err e ->
                        Expect.fail (D.errorToString e)
        ]


subscriptionChangeDataDecoderTests : Test
subscriptionChangeDataDecoderTests =
    describe "subscriptionChangeDataDecoder"
        [ test "decodes subscription change with started and stopped" <|
            \_ ->
                let
                    json =
                        E.object
                            [ ( "type", E.string "subscriptionChange" )
                            , ( "timestamp", E.int 12345 )
                            , ( "started", E.list identity [ E.string "sub1" ] )
                            , ( "stopped", E.list identity [ E.string "sub2" ] )
                            ]
                in
                case D.decodeValue (LogParser.subscriptionChangeDataDecoder CompressionDict.empty) json of
                    Ok data ->
                        Expect.all
                            [ \_ -> Expect.equal 12345 data.timestamp
                            , \_ -> Expect.equal 1 (List.length data.started)
                            , \_ -> Expect.equal 1 (List.length data.stopped)
                            ]
                            ()

                    Err e ->
                        Expect.fail (D.errorToString e)
        , test "defaults started and stopped to empty lists" <|
            \_ ->
                let
                    json =
                        E.object
                            [ ( "type", E.string "subscriptionChange" )
                            , ( "timestamp", E.int 100 )
                            ]
                in
                case D.decodeValue (LogParser.subscriptionChangeDataDecoder CompressionDict.empty) json of
                    Ok data ->
                        Expect.all
                            [ \_ -> Expect.equal [] data.started
                            , \_ -> Expect.equal [] data.stopped
                            ]
                            ()

                    Err e ->
                        Expect.fail (D.errorToString e)
        ]


effectDecoderTests : Test
effectDecoderTests =
    describe "effectDecoder"
        [ test "decodes effect with _type field" <|
            \_ ->
                let
                    json =
                        E.object
                            [ ( "_type", E.string "Effect.Http.Request" )
                            , ( "method", E.string "GET" )
                            , ( "url", E.string "/api/data" )
                            ]
                in
                case D.decodeValue LogParser.effectDecoder json of
                    Ok effect ->
                        Expect.equal "Effect.Http.Request" effect.name

                    Err e ->
                        Expect.fail (D.errorToString e)
        , test "decodes string-only effect" <|
            \_ ->
                let
                    json =
                        E.string "Cmd.none"
                in
                case D.decodeValue LogParser.effectDecoder json of
                    Ok effect ->
                        Expect.equal "Cmd.none" effect.name

                    Err e ->
                        Expect.fail (D.errorToString e)
        ]
