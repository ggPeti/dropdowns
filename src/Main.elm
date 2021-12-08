module Main exposing (..)

import Browser
import Bulma.CDN exposing (stylesheet)
import Bulma.Classes exposing (isFull, isHalf, isVariable)
import Bulma.Columns exposing (Display(..), Gap(..), column, columnModifiers, columns)
import Bulma.Components exposing (dropdown, dropdownItem, dropdownMenu, dropdownTrigger)
import Bulma.Elements exposing (easyRoundedTag)
import Bulma.Form exposing (controlCheckBox)
import Bulma.Layout exposing (container)
import Bulma.Modifiers exposing (Color(..), HorizontalAlignment(..), Size(..), VerticalDirection(..), Width(..))
import Dict
import Helpers exposing (..)
import Html exposing (Html, div, main_, p, text)
import Html.Attributes exposing (checked, class, style)
import Html.Events exposing (onClick)
import Model exposing (..)


main : Program () Model Msg
main =
    Browser.element
        { init = always ( initModel, Cmd.none )
        , update = update
        , subscriptions = always Sub.none
        , view = view
        }


view : Model -> Html Msg
view ({ overall, open } as model) =
    main_
        [ onClick CloseMenus
        , style "background-color" "#eff1f6"
        , style "padding" "32px"
        , style "font-family" "Verdana"
        , style "font-weight" "600"
        , style "font-size" "0.8em"
        ]
        [ stylesheet
        , container [ style "border-radius" "16px", style "background-color" "#e5e7ec", style "padding" "16px", style "max-width" "800px" ]
            [ viewComponent Overall (open == Just Overall) overall [ style "border-radius" "16px", style "box-shadow" "0px 2px 4px #aaa", style "margin" "-16px -16px 0 -16px", style "minHeight" "360px" ]
            , viewBreakdownHeader
            , viewCategories model
            ]
        ]


viewComponent : ComponentId -> Bool -> Component -> List (Html.Attribute Msg) -> Html Msg
viewComponent compId menuOpen component attrs =
    div ([ style "padding" "16px", style "background-color" "#fff" ] ++ attrs)
        [ div [ style "display" "flex", style "justify-content" "space-between", style "align-items" "flex-start" ]
            [ p [ style "text-transform" "uppercase" ] [ text (compIdToText compId) ]
            , dropdown menuOpen
                { horizontalAlignment = Right, verticalDirection = Down }
                [ onClickStopProp Nop ]
                [ dropdownTrigger []
                    [ easyRoundedTag { size = Medium, color = Link, isLink = True }
                        [ onClick (ToggleMenu compId), style "text-decoration" "none", style "background-color" (fi menuOpen "#192d5b" "#e0e4ed"), style "color" (fi menuOpen "#fff" "#000"), style "font-size" ".8em" ]
                        "COMPARE ▼"
                    ]
                , dropdownMenu [ style "min-width" "8rem" ]
                    [ style "box-shadow" "0px 4px 8px #aaa", style "max-height" "200px", style "overflow" "auto" ]
                    [ dropdownItem False [] (component |> Dict.map (viewOption compId) |> Dict.values) ]
                ]
            ]
        , p [ style "color" "#aaa" ] [ text (componentToString component) ]
        ]



viewBreakdownHeader : Html msg
viewBreakdownHeader =
    div [ style "margin" "24px 0 16px 0" ]
        [ p [] [ text "BREAKDOWN" ]
        , p [ style "color" "#aaa" ] [ text "Select the options from dropdown menu." ]
        ]


viewCategories : Model -> Html Msg
viewCategories { open, cat1, cat2, cat3 } =
    columns { multiline = True, gap = Gap2, display = DesktopAndBeyond, centered = True }
        [ class isVariable, style "display" "flex" ]
        [ column columnModifiers
            [ class isFull, style "padding" "var(--columnGap)" ]
            [ viewComponent Cat1 (open == Just Cat1) cat1 [ style "border-radius" "12px", style "min-height" "160px" ]
            ]
        , column columnModifiers
            [ class isHalf, style "padding" "var(--columnGap)" ]
            [ viewComponent Cat2 (open == Just Cat2) cat2 [ style "border-radius" "12px", style "min-height" "320px" ]
            ]
        , column columnModifiers
            [ class isHalf, style "padding" "var(--columnGap)" ]
            [ viewComponent Cat3 (open == Just Cat3) cat3 [ style "border-radius" "12px", style "min-height" "320px" ]
            ]
        ]


viewOption : ComponentId -> String -> Bool -> Html Msg
viewOption compId name state =
    controlCheckBox False
        [ style "padding-bottom" "16px", style "white-space" "nowrap" ]
        [ style "font-size" "0.8em" ]
        [ style "margin-right" "8px", checked state, onClick (Set compId name (not state)) ]
        [ text name ]


update : Msg -> Model -> ( Model, Cmd Msg )
update msg ({ open, overall, cat1, cat2, cat3 } as model) =
    case msg of
        ToggleMenu compId ->
            ( { model | open = fi (open == Just compId) Nothing (Just compId) }, Cmd.none )

        CloseMenus ->
            ( { model | open = Nothing }, Cmd.none )

        Set Overall name state ->
            ( { model | overall = overall |> Dict.insert name state }, Cmd.none )

        Set Cat1 name state ->
            ( { model | cat1 = cat1 |> Dict.insert name state }, Cmd.none )

        Set Cat2 name state ->
            ( { model | cat2 = cat2 |> Dict.insert name state }, Cmd.none )

        Set Cat3 name state ->
            ( { model | cat3 = cat3 |> Dict.insert name state }, Cmd.none )

        Nop ->
            ( model, Cmd.none )
