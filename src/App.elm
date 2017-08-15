module App exposing (main)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Time exposing (..)


startTime : Int
startTime =
    11


type alias Model =
    { startTime : Int
    , currentTime : Int
    , running : Bool
    }


type Msg
    = Start
    | Pause
    | Reset
    | Tick Time
    | Change String


model : Model
model =
    { startTime = startTime
    , currentTime = startTime
    , running = False
    }


view : Model -> Html Msg
view model =
    div []
        [ h1
            [ class "title" ]
            [ text "pomo-elm" ]
        , div
            [ class "form-container" ]
            [ input
                [ classList
                    [ ( "time", True )
                    , ( "time-input", True )
                    , ( "warning", model.running && model.currentTime < 10 && model.currentTime /= 0 )
                    , ( "done", model.currentTime == 0 )
                    ]
                , type_ "number"
                , defaultValue (toString model.startTime)
                , value (toString model.currentTime)
                , onInput Change
                , disabled model.running
                ]
                []
            ]
        , div
            [ class "edit-time-btn-group" ]
            [ button
                [ class "btn edit-second-up-btn"
                , name "second-up"
                , onClick (Change (toString (model.currentTime + 1)))
                , disabled model.running
                ]
                [ text "up" ]
            , button
                [ class "btn edit-second-down-btn"
                , name "second-down"
                , onClick (Change (toString (model.currentTime - 1)))
                , disabled model.running
                ]
                [ text "down" ]
            ]
        , div
            [ class "btn-group" ]
            [ button
                [ class "btn start-btn"
                , name "start"
                , onClick Start
                , disabled (model.running || model.currentTime == 0)
                ]
                [ text "start" ]
            , button
                [ class "btn pause-btn"
                , name "pause"
                , onClick Pause
                , disabled (not model.running)
                ]
                [ text "pause" ]
            , button
                [ class "btn reset-btn"
                , name "reset"
                , onClick Reset
                , disabled (model.currentTime == model.startTime)
                ]
                [ text "reset" ]
            ]
        ]


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Start ->
            ( { model | running = True }, Cmd.none )

        Reset ->
            ( { model | currentTime = model.startTime, running = False }, Cmd.none )

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

        Pause ->
            ( { model | running = False }, Cmd.none )

        Change inputString ->
            let
                newValue =
                    Result.withDefault 0 (String.toInt inputString)
            in
            ( { model | startTime = newValue, currentTime = newValue }, Cmd.none )


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
