module Helpers exposing (..)
import Json.Decode
import Html
import Html.Events exposing (stopPropagationOn)


{-|
Utility function to inline `if`s.
-}
fi : Bool -> a -> a -> a
fi i t e =
    if i then
        t

    else
        e



{-|
`onClick` but with stopPropagation.
-}
onClickStopProp : a -> Html.Attribute a
onClickStopProp msg =
    stopPropagationOn "click" (Json.Decode.succeed ( msg, True ))
