module CustomElements exposing (number)

import Html.Styled exposing (Attribute, Html, input)
import Html.Styled.Attributes exposing (type_, value)

number : Int -> List (Attribute msg) -> List (Html msg) -> Html msg
number val attr content =
    input (type_ "number" :: (value <| String.fromInt val) :: attr) content
