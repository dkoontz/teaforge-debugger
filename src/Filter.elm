module Filter exposing
    ( Filter(..)
    , FilterStatus(..)
    , ActiveFilter
    , FilterCategory(..)
    , EditingFilter(..)
    , filterCategory
    , editingFilterCategory
    , matchesEntry
    , fuzzyMatch
    , enabledFilters
    , filtersByCategory
    , enabledFilterCount
    , emptyEditingFilter
    , editingFilterFromActive
    , editingFilterToFilter
    , filterDisplayName
    , filterDisplayDetail
    , categoryName
    , categoryIcon
    , deepValueSearch
    , primitiveValueToString
    )

{-| Filter types and evaluation logic for the advanced filtering system.

This module defines the core filter types and provides functions to evaluate
whether log entries match a set of active filters. Filters are composable
and combine with AND logic -- all enabled filters must match for an entry
to be included.

@docs Filter, FilterStatus, ActiveFilter, FilterCategory
@docs filterCategory, matchesEntry, fuzzyMatch
@docs enabledFilters, filtersByCategory, enabledFilterCount

-}

import Array exposing (Array)
import Dict exposing (Dict)
import Diff
import Json.Decode as D
import Json.Encode as E
import Types
    exposing
        ( Effect
        , LogEntry(..)
        , TreePath
        )


{-| Whether a filter is currently applied or just stored.
-}
type FilterStatus
    = Enabled
    | Disabled


{-| A filter wrapped with its enabled/disabled state.

The separation means Filter defines _what_ to match, while
ActiveFilter wraps it with _whether_ it is currently applied.

-}
type alias ActiveFilter =
    { status : FilterStatus
    , filter : Filter
    }


{-| Core filter specification. Each variant is self-contained with only
the fields relevant to that filter type.
-}
type Filter
    = MessageNameFilter { query : String }
    | MessageFieldFilter { key : String, value : String }
    | ModelChangedFilter
    | ModelFieldChangedFilter { fieldPath : String }
    | ModelValueFilter { key : String, value : String }
    | HasEffectsFilter
    | EffectNameFilter { query : String }
    | EffectFieldFilter { key : String, value : String }
    | SubscriptionNameFilter { query : String }
    | SubscriptionFieldFilter { key : String, value : String }


{-| Filter categories for grouping in the sidebar.
-}
type FilterCategory
    = MessageCategory
    | ModelCategory
    | EffectsCategory
    | SubscriptionsCategory


{-| State for a filter being edited or created in the sidebar.
-}
type EditingFilter
    = EditingMessageName { query : String }
    | EditingMessageField { key : String, value : String }
    | EditingModelChanged
    | EditingModelFieldChanged { fieldPath : String }
    | EditingModelValue { key : String, value : String }
    | EditingHasEffects
    | EditingEffectName { query : String }
    | EditingEffectField { key : String, value : String }
    | EditingSubscriptionName { query : String }
    | EditingSubscriptionField { key : String, value : String }


{-| Derive the category from a filter variant.
-}
filterCategory : Filter -> FilterCategory
filterCategory filter =
    case filter of
        MessageNameFilter _ ->
            MessageCategory

        MessageFieldFilter _ ->
            MessageCategory

        ModelChangedFilter ->
            ModelCategory

        ModelFieldChangedFilter _ ->
            ModelCategory

        ModelValueFilter _ ->
            ModelCategory

        HasEffectsFilter ->
            EffectsCategory

        EffectNameFilter _ ->
            EffectsCategory

        EffectFieldFilter _ ->
            EffectsCategory

        SubscriptionNameFilter _ ->
            SubscriptionsCategory

        SubscriptionFieldFilter _ ->
            SubscriptionsCategory


{-| Derive the category from an editing filter variant.
-}
editingFilterCategory : EditingFilter -> FilterCategory
editingFilterCategory ef =
    case ef of
        EditingMessageName _ ->
            MessageCategory

        EditingMessageField _ ->
            MessageCategory

        EditingModelChanged ->
            ModelCategory

        EditingModelFieldChanged _ ->
            ModelCategory

        EditingModelValue _ ->
            ModelCategory

        EditingHasEffects ->
            EffectsCategory

        EditingEffectName _ ->
            EffectsCategory

        EditingEffectField _ ->
            EffectsCategory

        EditingSubscriptionName _ ->
            SubscriptionsCategory

        EditingSubscriptionField _ ->
            SubscriptionsCategory


{-| Get a human-readable name for a category.
-}
categoryName : FilterCategory -> String
categoryName category =
    case category of
        MessageCategory ->
            "Message"

        ModelCategory ->
            "Model"

        EffectsCategory ->
            "Effects"

        SubscriptionsCategory ->
            "Subscriptions"


{-| Get the FontAwesome icon class for a category.
-}
categoryIcon : FilterCategory -> String
categoryIcon category =
    case category of
        MessageCategory ->
            "fa-solid fa-envelope"

        ModelCategory ->
            "fa-solid fa-database"

        EffectsCategory ->
            "fa-solid fa-bolt"

        SubscriptionsCategory ->
            "fa-solid fa-arrows-rotate"


{-| Get a display name for a filter.
-}
filterDisplayName : Filter -> String
filterDisplayName filter =
    case filter of
        MessageNameFilter _ ->
            "Message Name"

        MessageFieldFilter _ ->
            "Message Field"

        ModelChangedFilter ->
            "Model Changed"

        ModelFieldChangedFilter _ ->
            "Model Field Changed"

        ModelValueFilter _ ->
            "Model Value"

        HasEffectsFilter ->
            "Has Effects"

        EffectNameFilter _ ->
            "Effect Name"

        EffectFieldFilter _ ->
            "Effect Field"

        SubscriptionNameFilter _ ->
            "Subscription Name"

        SubscriptionFieldFilter _ ->
            "Subscription Field"


{-| Get the display detail for a filter (the criteria values).
-}
filterDisplayDetail : Filter -> String
filterDisplayDetail filter =
    case filter of
        MessageNameFilter { query } ->
            "~ " ++ query

        MessageFieldFilter { key, value } ->
            key ++ " = " ++ value

        ModelChangedFilter ->
            "any change"

        ModelFieldChangedFilter { fieldPath } ->
            fieldPath ++ " changed"

        ModelValueFilter { key, value } ->
            key ++ " = " ++ value

        HasEffectsFilter ->
            "at least 1 effect"

        EffectNameFilter { query } ->
            "~ " ++ query

        EffectFieldFilter { key, value } ->
            key ++ " = " ++ value

        SubscriptionNameFilter { query } ->
            "~ " ++ query

        SubscriptionFieldFilter { key, value } ->
            key ++ " = " ++ value


{-| Create a default empty editing filter for a given category.
-}
emptyEditingFilter : FilterCategory -> EditingFilter
emptyEditingFilter category =
    case category of
        MessageCategory ->
            EditingMessageName { query = "" }

        ModelCategory ->
            EditingModelChanged

        EffectsCategory ->
            EditingHasEffects

        SubscriptionsCategory ->
            EditingSubscriptionName { query = "" }


{-| Convert an ActiveFilter to an EditingFilter for inline editing.
-}
editingFilterFromActive : ActiveFilter -> EditingFilter
editingFilterFromActive activeFilter =
    case activeFilter.filter of
        MessageNameFilter r ->
            EditingMessageName r

        MessageFieldFilter r ->
            EditingMessageField r

        ModelChangedFilter ->
            EditingModelChanged

        ModelFieldChangedFilter r ->
            EditingModelFieldChanged r

        ModelValueFilter r ->
            EditingModelValue r

        HasEffectsFilter ->
            EditingHasEffects

        EffectNameFilter r ->
            EditingEffectName r

        EffectFieldFilter r ->
            EditingEffectField r

        SubscriptionNameFilter r ->
            EditingSubscriptionName r

        SubscriptionFieldFilter r ->
            EditingSubscriptionField r


{-| Convert an EditingFilter back to a Filter. Returns Nothing if invalid.
-}
editingFilterToFilter : EditingFilter -> Maybe Filter
editingFilterToFilter editing =
    case editing of
        EditingMessageName { query } ->
            if String.isEmpty (String.trim query) then
                Nothing

            else
                Just (MessageNameFilter { query = String.trim query })

        EditingMessageField { key, value } ->
            if String.isEmpty (String.trim value) then
                Nothing

            else
                Just (MessageFieldFilter { key = String.trim key, value = String.trim value })

        EditingModelChanged ->
            Just ModelChangedFilter

        EditingModelFieldChanged { fieldPath } ->
            if String.isEmpty (String.trim fieldPath) then
                Nothing

            else
                Just (ModelFieldChangedFilter { fieldPath = String.trim fieldPath })

        EditingModelValue { key, value } ->
            if String.isEmpty (String.trim value) then
                Nothing

            else
                Just (ModelValueFilter { key = String.trim key, value = String.trim value })

        EditingHasEffects ->
            Just HasEffectsFilter

        EditingEffectName { query } ->
            if String.isEmpty (String.trim query) then
                Nothing

            else
                Just (EffectNameFilter { query = String.trim query })

        EditingEffectField { key, value } ->
            if String.isEmpty (String.trim value) then
                Nothing

            else
                Just (EffectFieldFilter { key = String.trim key, value = String.trim value })

        EditingSubscriptionName { query } ->
            if String.isEmpty (String.trim query) then
                Nothing

            else
                Just (SubscriptionNameFilter { query = String.trim query })

        EditingSubscriptionField { key, value } ->
            if String.isEmpty (String.trim value) then
                Nothing

            else
                Just (SubscriptionFieldFilter { key = String.trim key, value = String.trim value })



-- QUERYING


{-| Get only enabled filters from a list.
-}
enabledFilters : List ActiveFilter -> List Filter
enabledFilters =
    List.filterMap
        (\af ->
            case af.status of
                Enabled ->
                    Just af.filter

                Disabled ->
                    Nothing
        )


{-| Count enabled filters.
-}
enabledFilterCount : List ActiveFilter -> Int
enabledFilterCount =
    enabledFilters >> List.length


{-| Get filters grouped by category.
-}
filtersByCategory : List ActiveFilter -> List ( FilterCategory, List ( Int, ActiveFilter ) )
filtersByCategory filters =
    let
        indexed =
            List.indexedMap Tuple.pair filters

        categories =
            [ MessageCategory, ModelCategory, EffectsCategory, SubscriptionsCategory ]
    in
    List.filterMap
        (\cat ->
            let
                matching =
                    List.filter (\( _, af ) -> filterCategory af.filter == cat) indexed
            in
            if List.isEmpty matching then
                Nothing

            else
                Just ( cat, matching )
        )
        categories



-- EVALUATION


{-| Check if a log entry matches ALL enabled filters (AND logic).

ErrorEntry entries always match (are never filtered out).
InitEntry entries always match.
SubscriptionChangeEntry entries are excluded by message/model filters but
included by subscription filters.

-}
matchesEntry : List Filter -> LogEntry -> Bool
matchesEntry filters entry =
    case entry of
        ErrorEntry _ ->
            True

        InitEntry _ ->
            True

        _ ->
            List.all (\f -> matchesSingleFilter f entry) filters


{-| Check if a log entry matches a single filter.
-}
matchesSingleFilter : Filter -> LogEntry -> Bool
matchesSingleFilter filter entry =
    case filter of
        MessageNameFilter { query } ->
            matchMessageName query entry

        MessageFieldFilter { key, value } ->
            matchMessageField key value entry

        ModelChangedFilter ->
            matchModelChanged entry

        ModelFieldChangedFilter { fieldPath } ->
            matchModelFieldChanged fieldPath entry

        ModelValueFilter { key, value } ->
            matchModelValue key value entry

        HasEffectsFilter ->
            matchHasEffects entry

        EffectNameFilter { query } ->
            matchEffectName query entry

        EffectFieldFilter { key, value } ->
            matchEffectField key value entry

        SubscriptionNameFilter { query } ->
            matchSubscriptionName query entry

        SubscriptionFieldFilter { key, value } ->
            matchSubscriptionField key value entry


{-| Fuzzy match: each character in the query must appear in order in the target.
Case-insensitive.
-}
fuzzyMatch : String -> String -> Bool
fuzzyMatch query target =
    let
        queryChars =
            String.toLower query |> String.toList

        targetChars =
            String.toLower target |> String.toList
    in
    fuzzyMatchHelper queryChars targetChars


fuzzyMatchHelper : List Char -> List Char -> Bool
fuzzyMatchHelper queryChars targetChars =
    case queryChars of
        [] ->
            True

        qc :: restQuery ->
            case targetChars of
                [] ->
                    False

                tc :: restTarget ->
                    if qc == tc then
                        fuzzyMatchHelper restQuery restTarget

                    else
                        fuzzyMatchHelper (qc :: restQuery) restTarget



-- MATCH HELPERS


matchMessageName : String -> LogEntry -> Bool
matchMessageName query entry =
    case entry of
        UpdateEntry data ->
            fuzzyMatch query data.message.name

        SubscriptionChangeEntry _ ->
            -- SubscriptionChangeEntry has no message name, exclude
            False

        _ ->
            False


matchMessageField : String -> String -> LogEntry -> Bool
matchMessageField key value entry =
    case entry of
        UpdateEntry data ->
            if key == "*" then
                deepValueSearch value data.message.payload

            else
                matchFieldValue key value data.message.payload

        _ ->
            False


matchModelChanged : LogEntry -> Bool
matchModelChanged entry =
    case entry of
        UpdateEntry data ->
            let
                diffResult =
                    Diff.findChangedPaths data.modelBefore data.modelAfter
            in
            not (List.isEmpty diffResult.changedPaths)

        _ ->
            False


matchModelFieldChanged : String -> LogEntry -> Bool
matchModelFieldChanged fieldPath entry =
    case entry of
        UpdateEntry data ->
            let
                diffResult =
                    Diff.findChangedPaths data.modelBefore data.modelAfter
            in
            List.any
                (\changedPath ->
                    let
                        changedStr =
                            String.join "." changedPath
                    in
                    -- Match if the changed path starts with or equals the field path
                    changedStr == fieldPath || String.startsWith (fieldPath ++ ".") changedStr
                )
                diffResult.changedPaths

        _ ->
            False


matchModelValue : String -> String -> LogEntry -> Bool
matchModelValue key value entry =
    case entry of
        UpdateEntry data ->
            if key == "*" then
                deepValueSearch value data.modelAfter

            else
                matchFieldValue key value data.modelAfter

        _ ->
            False


matchHasEffects : LogEntry -> Bool
matchHasEffects entry =
    case entry of
        UpdateEntry data ->
            not (List.isEmpty data.effects)

        InitEntry data ->
            not (List.isEmpty data.effects)

        _ ->
            False


matchEffectName : String -> LogEntry -> Bool
matchEffectName query entry =
    let
        effects =
            getEntryEffects entry
    in
    List.any (\e -> fuzzyMatch query e.name) effects


matchEffectField : String -> String -> LogEntry -> Bool
matchEffectField key value entry =
    let
        effects =
            getEntryEffects entry
    in
    List.any
        (\e ->
            if key == "*" then
                deepValueSearch value e.data

            else
                matchFieldValue key value e.data
        )
        effects


matchSubscriptionName : String -> LogEntry -> Bool
matchSubscriptionName query entry =
    case entry of
        SubscriptionChangeEntry data ->
            let
                allSubs =
                    data.started ++ data.stopped

                subNames =
                    List.filterMap extractSubName allSubs
            in
            List.any (fuzzyMatch query) subNames

        _ ->
            False


matchSubscriptionField : String -> String -> LogEntry -> Bool
matchSubscriptionField key value entry =
    case entry of
        SubscriptionChangeEntry data ->
            let
                allSubs =
                    data.started ++ data.stopped
            in
            List.any
                (\sub ->
                    if key == "*" then
                        deepValueSearch value sub

                    else
                        matchFieldValue key value sub
                )
                allSubs

        _ ->
            False



-- JSON HELPERS


getEntryEffects : LogEntry -> List Effect
getEntryEffects entry =
    case entry of
        UpdateEntry data ->
            data.effects

        InitEntry data ->
            data.effects

        _ ->
            []


{-| Extract a subscription name from a subscription JSON value.
Tries to get a "name" or "_type" field, or uses JSON encoding.
-}
extractSubName : D.Value -> Maybe String
extractSubName value =
    case D.decodeValue (D.field "name" D.string) value of
        Ok name ->
            Just name

        Err _ ->
            case D.decodeValue (D.field "_type" D.string) value of
                Ok typeName ->
                    Just typeName

                Err _ ->
                    case D.decodeValue D.string value of
                        Ok str ->
                            Just str

                        Err _ ->
                            Nothing


{-| Deep search for a value across all fields of a JSON value.
Returns True if any primitive value (as a string) contains the search value
as a case-insensitive substring.
-}
deepValueSearch : String -> D.Value -> Bool
deepValueSearch searchValue jsonValue =
    let
        lowerSearch =
            String.toLower searchValue
    in
    deepValueSearchHelper lowerSearch jsonValue


deepValueSearchHelper : String -> D.Value -> Bool
deepValueSearchHelper lowerSearch jsonValue =
    -- Check if this is a primitive that matches
    if primitiveContains lowerSearch jsonValue then
        True

    else
        -- Check object fields
        case D.decodeValue (D.keyValuePairs D.value) jsonValue of
            Ok pairs ->
                List.any (\( _, v ) -> deepValueSearchHelper lowerSearch v) pairs

            Err _ ->
                -- Check array items
                case D.decodeValue (D.list D.value) jsonValue of
                    Ok items ->
                        List.any (deepValueSearchHelper lowerSearch) items

                    Err _ ->
                        False


{-| Convert a primitive JSON value to its string representation.

Returns Nothing for non-primitive values (objects, arrays).

-}
primitiveValueToString : D.Value -> Maybe String
primitiveValueToString jsonValue =
    case D.decodeValue D.string jsonValue of
        Ok str ->
            Just str

        Err _ ->
            case D.decodeValue D.float jsonValue of
                Ok num ->
                    Just (String.fromFloat num)

                Err _ ->
                    case D.decodeValue D.bool jsonValue of
                        Ok b ->
                            Just
                                (if b then
                                    "true"

                                 else
                                    "false"
                                )

                        Err _ ->
                            case D.decodeValue (D.null ()) jsonValue of
                                Ok _ ->
                                    Just "null"

                                Err _ ->
                                    Nothing


{-| Check if a primitive JSON value contains the search string.
-}
primitiveContains : String -> D.Value -> Bool
primitiveContains lowerSearch jsonValue =
    case primitiveValueToString jsonValue of
        Just str ->
            String.contains lowerSearch (String.toLower str)

        Nothing ->
            False


{-| Match a specific field key to a value in a JSON object.
The key can be a dot-separated path (e.g., "user.name").
-}
matchFieldValue : String -> String -> D.Value -> Bool
matchFieldValue key value jsonValue =
    let
        pathParts =
            String.split "." key

        lowerValue =
            String.toLower value
    in
    matchFieldValueAtPath pathParts lowerValue jsonValue


matchFieldValueAtPath : List String -> String -> D.Value -> Bool
matchFieldValueAtPath pathParts lowerValue jsonValue =
    case pathParts of
        [] ->
            -- At the target: check if value matches
            primitiveContains lowerValue jsonValue
                || (case D.decodeValue (D.keyValuePairs D.value) jsonValue of
                        Ok pairs ->
                            List.any (\( _, v ) -> primitiveContains lowerValue v) pairs

                        Err _ ->
                            case D.decodeValue (D.list D.value) jsonValue of
                                Ok items ->
                                    List.any (primitiveContains lowerValue) items

                                Err _ ->
                                    False
                   )

        segment :: rest ->
            case D.decodeValue (D.field segment D.value) jsonValue of
                Ok childValue ->
                    matchFieldValueAtPath rest lowerValue childValue

                Err _ ->
                    False
