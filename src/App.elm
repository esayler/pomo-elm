module App exposing (main)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Time exposing (..)


initialTime : Int
initialTime =
    60 * 25


defaultWorkTime : Int
defaultWorkTime =
    60 * 25


defaultShortBreakTime : Int
defaultShortBreakTime =
    60 * 5


defaultLongBreakTime : Int
defaultLongBreakTime =
    60 * 20


type alias Model =
    { initialTime : Int
    , currentTime : Int
    , workTime : Int
    , longBreakTime : Int
    , shortBreakTime : Int
    , running : Bool
    }


type Msg
    = Start
    | Set Int
    | Pause
    | Reset
    | Tick Time
    | Change String


type Direction
    = Up
    | Down


model : Model
model =
    { initialTime = initialTime
    , currentTime = initialTime
    , workTime = defaultWorkTime
    , shortBreakTime = defaultShortBreakTime
    , longBreakTime = defaultLongBreakTime
    , running = False
    }


view : Model -> Html Msg
view model =
    div []
        [ h1
            [ class "title" ]
            [ text "pomo-elm" ]
        , timeSelector model
        , div
            [ class "time-display-container" ]
            (List.concat
                [ timeEditable model
                , timeButton Up "edit-min-up-btn" "min-up" "+1m" 60 model
                , timeButton Up "edit-sec-up-btn" "second-up" "+1s" 1 model
                , displayTime model
                , timeButton Down "edit-min-down-btn" "min-down" "-1m" 60 model
                , timeButton Down "edit-sec-down-btn" "sec-down" "-1s" 1 model
                ]
            )
        , div
            [ class "btn-group controls" ]
            [ button
                [ class "btn btn-large start-btn"
                , name "start"
                , onClick Start
                , disabled (model.running || model.currentTime == 0)
                ]
                [ text "start" ]
            , button
                [ class "btn btn-large pause-btn"
                , name "pause"
                , onClick Pause
                , disabled (not model.running)
                ]
                [ text "pause" ]
            , button
                [ class "btn btn-large reset-btn"
                , name "reset"
                , onClick Reset
                , disabled (model.currentTime == model.initialTime)
                ]
                [ text "reset" ]
            ]
        ]


timeButton : Direction -> String -> String -> String -> Int -> Model -> List (Html Msg)
timeButton direction className nameString textString amt model =
    let
        newTime =
            case direction of
                Up ->
                    model.currentTime + amt

                Down ->
                    model.currentTime - amt
    in
    [ button
        [ class ("btn edit-btn " ++ className)
        , name nameString
        , onClick (Change (toString newTime))
        ]
        [ text textString ]
    ]


timeEditable : Model -> List (Html Msg)
timeEditable model =
    [ input
        [ classList
            [ ( "time", True )
            , ( "time-input", True )
            , ( "warning", model.running && model.currentTime < 10 && model.currentTime /= 0 )
            , ( "done", model.currentTime == 0 )
            ]
        , type_ "number"
        , defaultValue (toString model.initialTime)
        , value (toString model.currentTime)
        , onInput Change
        , disabled model.running
        ]
        []
    ]


timeSelector : Model -> Html Msg
timeSelector model =
    div
        [ class "time-selector" ]
        [ div
            [ class "btn-group" ]
            [ button
                [ class "btn set-time-btn"
                , name "set-work-time"
                , onClick (Set model.workTime)
                ]
                [ text "25m" ]
            , button
                [ class "btn set-time-btn"
                , name "set-short-break-time"
                , onClick (Set model.shortBreakTime)
                ]
                [ text "5m" ]
            , button
                [ class "btn set-time-btn"
                , name "set-long-break-time"
                , onClick (Set model.longBreakTime)
                ]
                [ text "20m" ]
            ]
        ]


displayTime : Model -> List (Html Msg)
displayTime model =
    let
        mins =
            model.currentTime // 60

        remainingSeconds =
            model.currentTime % 60
    in
    [ span
        [ classList
            [ ( "time-display-number time-min", True )
            , ( "warning", model.running && model.currentTime < 10 && model.currentTime /= 0 )
            , ( "done", model.currentTime == 0 )
            ]
        ]
        [ text (toString mins)
        , span
            [ class "time-display-label time-label-min" ]
            [ text "m" ]
        ]
    , span
        [ classList
            [ ( "time-display-number time-sec", True )
            , ( "warning", model.running && model.currentTime < 10 && model.currentTime /= 0 )
            , ( "done", model.currentTime == 0 )
            ]
        ]
        [ text (toString remainingSeconds)
        , span
            [ class "time-display-label time-label-sec" ]
            [ text "s" ]
        ]
    ]


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Start ->
            ( { model | running = True }, Cmd.none )

        Set seconds ->
            ( { model | initialTime = seconds, currentTime = seconds }, Cmd.none )

        Reset ->
            ( { model | currentTime = model.initialTime, running = False }, Cmd.none )

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
            ( { model | initialTime = newValue, currentTime = newValue }, Cmd.none )


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
