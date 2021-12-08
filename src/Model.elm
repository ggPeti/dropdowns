module Model exposing (..)
import Dict exposing (Dict)

type alias Component =
    Dict String Bool


emptyComponent : Component
emptyComponent =
    Dict.fromList (List.range 1 9 |> List.map (\n -> ( "Option " ++ String.fromInt n, False )))


componentToString : Component -> String
componentToString component =
    if Dict.values component |> List.all ((==) False) then
        ""

    else
        "Selected: " ++ (Dict.filter (always identity) component |> Dict.keys |> String.join ", ")


type alias Model =
    { open : Maybe ComponentId, overall : Component, cat1 : Component, cat2 : Component, cat3 : Component }


initModel : Model
initModel =
    { open = Nothing, overall = emptyComponent, cat1 = emptyComponent, cat2 = emptyComponent, cat3 = emptyComponent }


type ComponentId
    = Overall
    | Cat1
    | Cat2
    | Cat3


compIdToText : ComponentId -> String
compIdToText compId =
    case compId of
        Overall ->
            "Overall"

        Cat1 ->
            "Category 1"

        Cat2 ->
            "Category 2"

        Cat3 ->
            "Category 3"


type Msg
    = ToggleMenu ComponentId
    | CloseMenus
    | Set ComponentId String Bool
    | Nop
