module CompressionDict exposing
    ( Compression(..)
    , empty
    , merge
    , decompressString
    , decompressValue
    )

{-| String dictionary compression support for TeaForge log files.

This module handles LZ78-style string dictionary compression where repeated
strings are replaced with `@N` references (where N is an integer).

## Compression State

@docs Compression, empty

## Operations

@docs merge, decompressString, decompressValue

-}

import Dict exposing (Dict)
import Json.Decode as D
import Json.Encode as E
import Regex


{-| A lookup dictionary mapping string keys to their decompressed values.
-}
type alias LookupDict =
    Dict String String


{-| Compression state.

  - `Disabled` - No compression; strings pass through unchanged
  - `Enabled dict` - Active compression with the given dictionary

-}
type Compression
    = Disabled
    | Enabled LookupDict


{-| Initial empty compression state (disabled).
-}
empty : Compression
empty =
    Disabled


{-| Regex pattern to match @N references where N is a non-negative integer.
-}
refPattern : Regex.Regex
refPattern =
    Regex.fromString "^@(\\d+)$"
        |> Maybe.withDefault Regex.never


{-| Check if a string is an @N reference and extract the numeric part.
-}
extractRef : String -> Maybe String
extractRef str =
    case Regex.find refPattern str of
        [ match ] ->
            match.submatches
                |> List.head
                |> Maybe.andThen identity

        _ ->
            Nothing


{-| Decompress a single string if it's an @N reference.

Returns the original string if:

  - Compression is `Disabled`
  - The string doesn't match the `@N` pattern
  - The reference is not found in the dictionary

-}
decompressString : Compression -> String -> String
decompressString compression str =
    case compression of
        Disabled ->
            str

        Enabled dict ->
            case extractRef str of
                Just refNum ->
                    Dict.get refNum dict
                        |> Maybe.withDefault str

                Nothing ->
                    str


{-| Merge strings from a stringDict entry into compression state.

The value should be a JSON object with a "strings" field containing
a dictionary of string references:

    {"type": "stringDict", "strings": {"0": "model", "1": "Message.Click"}}

If compression is `Disabled`, this enables it with the new strings.
If compression is already `Enabled`, the new strings are merged in.

-}
merge : D.Value -> Compression -> Compression
merge value compression =
    let
        stringsDecoder =
            D.field "strings" (D.dict D.string)
    in
    case D.decodeValue stringsDecoder value of
        Ok newStrings ->
            case compression of
                Disabled ->
                    Enabled newStrings

                Enabled existingDict ->
                    Enabled (Dict.union newStrings existingDict)

        Err _ ->
            compression


{-| Recursively decompress a JSON value.

This walks the D.Value structure and:

  - Objects: Decompresses keys and recursively processes values
  - Arrays: Recursively processes elements
  - Strings: Decompresses if @N reference
  - Other primitives: Pass through unchanged

-}
decompressValue : Compression -> D.Value -> D.Value
decompressValue compression value =
    case compression of
        Disabled ->
            value

        Enabled _ ->
            decompressValueHelp compression value


{-| Helper to recursively decompress a JSON value.
-}
decompressValueHelp : Compression -> D.Value -> D.Value
decompressValueHelp compression value =
    -- Try to decode as different JSON types and handle accordingly
    case D.decodeValue (D.keyValuePairs D.value) value of
        Ok pairs ->
            -- It's an object - decompress keys and recurse on values
            pairs
                |> List.map
                    (\( key, childValue ) ->
                        ( decompressString compression key
                        , decompressValueHelp compression childValue
                        )
                    )
                |> E.object

        Err _ ->
            -- Not an object, try array
            case D.decodeValue (D.list D.value) value of
                Ok items ->
                    items
                        |> List.map (decompressValueHelp compression)
                        |> E.list identity

                Err _ ->
                    -- Not an array, try string
                    case D.decodeValue D.string value of
                        Ok str ->
                            E.string (decompressString compression str)

                        Err _ ->
                            -- Other primitive (number, bool, null) - pass through
                            value
