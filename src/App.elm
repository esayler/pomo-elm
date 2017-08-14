module App exposing (main)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)


type alias Model =
    { time : Int }


type Msg
    = Start


model : Model
model =
    { time = 10 }


view : Model -> Html Msg
view model =
    div []
        [ h1 [ class "title" ] [ text "Pomo Elm" ]
        , h2 [ class "time" ] [ text (toString model.time) ]
        , button
            [ class "start-btn"
            , name "start"
            , onClick Start
            ]
            [ text "start" ]
        ]


update : Msg -> Model -> Model
update msg model =
    model


main : Program Never Model Msg
main =
    Html.beginnerProgram
        { model = model
        , view = view
        , update = update
        }
