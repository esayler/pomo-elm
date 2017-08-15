module App exposing (main)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Time exposing (..)


startTime : Int
startTime =
    30


type alias Model =
    { startTime : Int
    , currentTime : Int
    , running : Bool
    }


type Msg
    = Start
    | Reset
    | Tick Time


model : Model
model =
    { startTime = startTime
    , currentTime = startTime
    , running = False
    }


view : Model -> Html Msg
view model =
    div []
        [ h1 [ class "title" ] [ text "pomo-elm" ]
        , div [ class "time-display" ]
            [ span [ class "time" ] [ text (toString model.currentTime) ]
            , span [ class "time-label" ] [ text "s" ]
            ]
        , button
            [ class "start-btn"
            , name "start"
            , onClick Start
            ]
            [ text "start" ]
        , button
            [ class "reset-btn"
            , name "reset"
            , onClick Reset
            ]
            [ text "reset" ]
        ]


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Start ->
            ( { model | running = True }, Cmd.none )

        Reset ->
            ( { model | currentTime = startTime, running = False }, Cmd.none )

        Tick time ->
            let
                running =
                    if model.currentTime == 0 then
                        False
                    else
                        True

                newTime =
                    if running then
                        model.currentTime - 1
                    else
                        model.currentTime
            in
            ( { model | currentTime = newTime, running = running }, Cmd.none )


subscriptions : Model -> Sub Msg
subscriptions model =
    if model.running then
        Time.every second Tick
    else
        Sub.none


main : Program Never Model Msg
main =
    Html.program
        { init = ( model, Cmd.none )
        , view = view
        , update = update
        , subscriptions = subscriptions
        }
