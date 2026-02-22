module FilterSidebar exposing
    ( Msg(..)
    , Config
    , view
    , viewToggleButton
    )

{-| Filter sidebar UI component for the advanced filtering system.

This module provides the view for the right-hand filter sidebar,
including filter CRUD operations, category sections, and toggle controls.

@docs Msg, Config, view, viewToggleButton

-}

import Filter
    exposing
        ( ActiveFilter
        , EditingFilter(..)
        , Filter(..)
        , FilterCategory(..)
        , FilterStatus(..)
        , categoryIcon
        , categoryName
        , editingFilterCategory
        , editingFilterToFilter
        , enabledFilterCount
        , filterCategory
        , filterDisplayDetail
        , filterDisplayName
        )
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)


{-| Messages produced by the filter sidebar.
-}
type Msg
    = ToggleSidebar
    | ToggleGlobalFilters
    | ToggleCategoryFilters FilterCategory
    | ToggleFilter Int
    | DeleteFilter Int
    | StartAddFilter FilterCategory
    | StartEditFilter Int
    | CancelEdit
    | SaveEdit
    | UpdateEditingFilter EditingFilter
    | ChangeEditingType EditingFilter


{-| Configuration for the filter sidebar view.
-}
type alias Config =
    { filters : List ActiveFilter
    , isOpen : Bool
    , globalEnabled : Bool
    , editing : Maybe ( Maybe Int, EditingFilter )

    -- Maybe Int: Nothing = adding new, Just idx = editing existing
    , totalEntries : Int
    , visibleEntries : Int
    }


{-| Render the toggle button for the filter sidebar.
Shows the filter icon and active filter count badge.
-}
viewToggleButton : Config -> Html Msg
viewToggleButton config =
    let
        activeCount =
            enabledFilterCount config.filters

        hasFilters =
            not (List.isEmpty config.filters)
    in
    button
        [ class "btn btn-ghost btn-sm gap-1"
        , onClick ToggleSidebar
        , title "Toggle filter sidebar"
        ]
        [ i [ class "fa-solid fa-filter" ] []
        , if hasFilters && activeCount > 0 && config.globalEnabled then
            span [ class "badge badge-primary badge-xs" ]
                [ text (String.fromInt activeCount) ]

          else
            text ""
        ]


{-| Render the filter sidebar.
-}
view : Config -> Html Msg
view config =
    div
        [ class "flex flex-col h-full bg-base-200 border-l border-base-300 overflow-hidden"
        , style "width" "280px"
        , style "flex-shrink" "0"
        ]
        [ viewHeader config
        , viewFilterCount config
        , div [ class "flex-1 overflow-y-auto p-2" ]
            [ viewFilterList config
            , viewEditingForm config
            ]
        ]


{-| Render the sidebar header with global toggle.
-}
viewHeader : Config -> Html Msg
viewHeader config =
    div [ class "flex items-center justify-between p-3 border-b border-base-300 bg-base-100" ]
        [ div [ class "flex items-center gap-2" ]
            [ i [ class "fa-solid fa-filter text-primary" ] []
            , span [ class "font-semibold text-sm" ] [ text "Filters" ]
            ]
        , div [ class "flex items-center gap-2" ]
            [ if not (List.isEmpty config.filters) then
                label [ class "label cursor-pointer gap-1" ]
                    [ span [ class "label-text text-xs" ] [ text "All" ]
                    , input
                        [ type_ "checkbox"
                        , class "toggle toggle-xs toggle-primary"
                        , checked config.globalEnabled
                        , onClick ToggleGlobalFilters
                        ]
                        []
                    ]

              else
                text ""
            , button
                [ class "btn btn-ghost btn-xs"
                , onClick ToggleSidebar
                , title "Close filter sidebar"
                ]
                [ i [ class "fa-solid fa-xmark" ] [] ]
            ]
        ]


{-| Render the message count indicator (visible/total).
-}
viewFilterCount : Config -> Html Msg
viewFilterCount config =
    let
        hasActiveFilters =
            config.globalEnabled && enabledFilterCount config.filters > 0
    in
    if hasActiveFilters then
        div [ class "px-3 py-1.5 border-b border-base-300 bg-base-100/50" ]
            [ span [ class "text-xs text-base-content/60" ]
                [ text
                    (String.fromInt config.visibleEntries
                        ++ " of "
                        ++ String.fromInt config.totalEntries
                        ++ " messages"
                    )
                ]
            ]

    else
        text ""


{-| Render the list of filters grouped by category.
-}
viewFilterList : Config -> Html Msg
viewFilterList config =
    let
        allCategories =
            [ MessageCategory, ModelCategory, EffectsCategory, SubscriptionsCategory ]

        indexed =
            List.indexedMap Tuple.pair config.filters
    in
    div [ class "space-y-1" ]
        (List.map
            (\cat ->
                let
                    categoryFilters =
                        List.filter (\( _, af ) -> filterCategory af.filter == cat) indexed
                in
                viewCategorySection config cat categoryFilters
            )
            allCategories
        )


{-| Render a category section with its filters and add button.
-}
viewCategorySection : Config -> FilterCategory -> List ( Int, ActiveFilter ) -> Html Msg
viewCategorySection config category categoryFilters =
    div [ class "mb-1" ]
        [ div [ class "flex items-center justify-between px-2 py-1.5" ]
            [ div [ class "flex items-center gap-1.5" ]
                [ i [ class (categoryIcon category ++ " text-xs text-base-content/50") ] []
                , span [ class "text-xs font-semibold uppercase tracking-wider text-base-content/60" ]
                    [ text (categoryName category) ]
                , if not (List.isEmpty categoryFilters) then
                    label [ class "cursor-pointer" ]
                        [ input
                            [ type_ "checkbox"
                            , class "toggle toggle-xs"
                            , checked (List.all (\( _, af ) -> af.status == Enabled) categoryFilters)
                            , onClick (ToggleCategoryFilters category)
                            ]
                            []
                        ]

                  else
                    text ""
                ]
            , button
                [ class "btn btn-ghost btn-xs px-1"
                , onClick (StartAddFilter category)
                , title ("Add " ++ categoryName category ++ " filter")
                ]
                [ i [ class "fa-solid fa-plus text-xs" ] [] ]
            ]
        , if List.isEmpty categoryFilters then
            text ""

          else
            div [ class "space-y-0.5 px-1" ]
                (List.map (\( idx, af ) -> viewFilterItem config idx af) categoryFilters)
        ]


{-| Render a single filter item.
-}
viewFilterItem : Config -> Int -> ActiveFilter -> Html Msg
viewFilterItem config index activeFilter =
    let
        isDisabled =
            activeFilter.status == Disabled || not config.globalEnabled

        opacityClass =
            if isDisabled then
                " opacity-40"

            else
                ""
    in
    div
        [ class ("flex items-center gap-1.5 px-2 py-1.5 rounded-lg bg-base-100 border border-base-300 text-xs" ++ opacityClass)
        ]
        [ input
            [ type_ "checkbox"
            , class "checkbox checkbox-xs"
            , checked (activeFilter.status == Enabled)
            , onClick (ToggleFilter index)
            ]
            []
        , div [ class "flex-1 min-w-0" ]
            [ div [ class "font-medium truncate" ]
                [ text (filterDisplayName activeFilter.filter) ]
            , div [ class "text-base-content/60 truncate" ]
                [ text (filterDisplayDetail activeFilter.filter) ]
            ]
        , button
            [ class "btn btn-ghost btn-xs px-1"
            , onClick (StartEditFilter index)
            , title "Edit filter"
            ]
            [ i [ class "fa-solid fa-pen text-xs" ] [] ]
        , button
            [ class "btn btn-ghost btn-xs px-1 text-error"
            , onClick (DeleteFilter index)
            , title "Delete filter"
            ]
            [ i [ class "fa-solid fa-trash text-xs" ] [] ]
        ]


{-| Render the editing/adding form if active.
-}
viewEditingForm : Config -> Html Msg
viewEditingForm config =
    case config.editing of
        Nothing ->
            text ""

        Just ( maybeIdx, editingFilter ) ->
            let
                title_ =
                    case maybeIdx of
                        Nothing ->
                            "Add Filter"

                        Just _ ->
                            "Edit Filter"

                isValid =
                    editingFilterToFilter editingFilter /= Nothing
            in
            div [ class "mx-1 mt-2 p-3 rounded-lg bg-base-100 border border-primary/30" ]
                [ div [ class "flex items-center justify-between mb-2" ]
                    [ span [ class "text-sm font-semibold" ] [ text title_ ]
                    ]
                , viewFilterTypeSelector editingFilter
                , viewEditingFields editingFilter
                , div [ class "flex justify-end gap-2 mt-3" ]
                    [ button
                        [ class "btn btn-ghost btn-xs"
                        , onClick CancelEdit
                        ]
                        [ text "Cancel" ]
                    , button
                        [ class "btn btn-primary btn-xs"
                        , onClick SaveEdit
                        , disabled (not isValid)
                        ]
                        [ text
                            (case maybeIdx of
                                Nothing ->
                                    "Add"

                                Just _ ->
                                    "Save"
                            )
                        ]
                    ]
                ]


{-| Render the filter type selector dropdown.
-}
viewFilterTypeSelector : EditingFilter -> Html Msg
viewFilterTypeSelector editingFilter =
    let
        currentValue =
            editingFilterTypeValue editingFilter

        category =
            editingFilterCategory editingFilter
    in
    div [ class "mb-2" ]
        [ select
            [ class "select select-bordered select-xs w-full"
            , onInput (\v -> ChangeEditingType (typeValueToEditing category v))
            , value currentValue
            ]
            (typeOptionsForCategory category)
        ]


{-| Get the select value for an editing filter.
-}
editingFilterTypeValue : EditingFilter -> String
editingFilterTypeValue ef =
    case ef of
        EditingMessageName _ ->
            "message-name"

        EditingMessageField _ ->
            "message-field"

        EditingModelChanged ->
            "model-changed"

        EditingModelFieldChanged _ ->
            "model-field-changed"

        EditingModelValue _ ->
            "model-value"

        EditingHasEffects ->
            "has-effects"

        EditingEffectName _ ->
            "effect-name"

        EditingEffectField _ ->
            "effect-field"

        EditingSubscriptionName _ ->
            "subscription-name"

        EditingSubscriptionField _ ->
            "subscription-field"


{-| Get the type options for a category.
-}
typeOptionsForCategory : FilterCategory -> List (Html Msg)
typeOptionsForCategory category =
    case category of
        MessageCategory ->
            [ option [ value "message-name" ] [ text "Name (fuzzy)" ]
            , option [ value "message-field" ] [ text "Field = Value" ]
            ]

        ModelCategory ->
            [ option [ value "model-changed" ] [ text "Any Change" ]
            , option [ value "model-field-changed" ] [ text "Field Changed" ]
            , option [ value "model-value" ] [ text "Field = Value" ]
            ]

        EffectsCategory ->
            [ option [ value "has-effects" ] [ text "Has Effects" ]
            , option [ value "effect-name" ] [ text "Name (fuzzy)" ]
            , option [ value "effect-field" ] [ text "Field = Value" ]
            ]

        SubscriptionsCategory ->
            [ option [ value "subscription-name" ] [ text "Name (fuzzy)" ]
            , option [ value "subscription-field" ] [ text "Field = Value" ]
            ]


{-| Convert a select value back to an editing filter.
-}
typeValueToEditing : FilterCategory -> String -> EditingFilter
typeValueToEditing category typeValue =
    case typeValue of
        "message-name" ->
            EditingMessageName { query = "" }

        "message-field" ->
            EditingMessageField { key = "*", value = "" }

        "model-changed" ->
            EditingModelChanged

        "model-field-changed" ->
            EditingModelFieldChanged { fieldPath = "" }

        "model-value" ->
            EditingModelValue { key = "*", value = "" }

        "has-effects" ->
            EditingHasEffects

        "effect-name" ->
            EditingEffectName { query = "" }

        "effect-field" ->
            EditingEffectField { key = "*", value = "" }

        "subscription-name" ->
            EditingSubscriptionName { query = "" }

        "subscription-field" ->
            EditingSubscriptionField { key = "*", value = "" }

        _ ->
            Filter.emptyEditingFilter category


{-| Render the editing fields for the current filter type.
-}
viewEditingFields : EditingFilter -> Html Msg
viewEditingFields editingFilter =
    case editingFilter of
        EditingMessageName { query } ->
            div []
                [ viewInputField "Name pattern" query (\v -> UpdateEditingFilter (EditingMessageName { query = v }))
                , viewHint "Fuzzy match: characters must appear in order"
                ]

        EditingMessageField { key, value } ->
            div [ class "space-y-1.5" ]
                [ viewInputField "Field (or * for any)" key (\v -> UpdateEditingFilter (EditingMessageField { key = v, value = value }))
                , viewInputField "Value" value (\v -> UpdateEditingFilter (EditingMessageField { key = key, value = v }))
                ]

        EditingModelChanged ->
            div []
                [ viewHint "Matches messages that changed the model" ]

        EditingModelFieldChanged { fieldPath } ->
            div []
                [ viewInputField "Field path (e.g. user.name)" fieldPath (\v -> UpdateEditingFilter (EditingModelFieldChanged { fieldPath = v }))
                ]

        EditingModelValue { key, value } ->
            div [ class "space-y-1.5" ]
                [ viewInputField "Field (or * for any)" key (\v -> UpdateEditingFilter (EditingModelValue { key = v, value = value }))
                , viewInputField "Value" value (\v -> UpdateEditingFilter (EditingModelValue { key = key, value = v }))
                ]

        EditingHasEffects ->
            div []
                [ viewHint "Matches messages that produced at least 1 effect" ]

        EditingEffectName { query } ->
            div []
                [ viewInputField "Effect name pattern" query (\v -> UpdateEditingFilter (EditingEffectName { query = v }))
                , viewHint "Fuzzy match: characters must appear in order"
                ]

        EditingEffectField { key, value } ->
            div [ class "space-y-1.5" ]
                [ viewInputField "Field (or * for any)" key (\v -> UpdateEditingFilter (EditingEffectField { key = v, value = value }))
                , viewInputField "Value" value (\v -> UpdateEditingFilter (EditingEffectField { key = key, value = v }))
                ]

        EditingSubscriptionName { query } ->
            div []
                [ viewInputField "Subscription name pattern" query (\v -> UpdateEditingFilter (EditingSubscriptionName { query = v }))
                , viewHint "Fuzzy match: characters must appear in order"
                ]

        EditingSubscriptionField { key, value } ->
            div [ class "space-y-1.5" ]
                [ viewInputField "Field (or * for any)" key (\v -> UpdateEditingFilter (EditingSubscriptionField { key = v, value = value }))
                , viewInputField "Value" value (\v -> UpdateEditingFilter (EditingSubscriptionField { key = key, value = v }))
                ]


viewInputField : String -> String -> (String -> Msg) -> Html Msg
viewInputField placeholder_ currentValue onInputMsg =
    input
        [ type_ "text"
        , class "input input-bordered input-xs w-full"
        , placeholder placeholder_
        , value currentValue
        , onInput onInputMsg
        ]
        []


viewHint : String -> Html Msg
viewHint hintText =
    p [ class "text-xs text-base-content/50 mt-1" ] [ text hintText ]
