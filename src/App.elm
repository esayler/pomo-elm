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
    | Increment TimeUnit
    | Decrement TimeUnit
    | Pause
    | Reset
    | Tick Time
    | Change String


type Direction
    = Up
    | Down


type TimeUnit
    = Min
    | Sec


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
    div [ class "app" ]
        [ h1
            [ class "title" ]
            [ span
                [ class "title-pomo" ]
                [ text "pomo" ]
            , span
                [ class "title-dash" ]
                [ text "." ]
            , span
                [ class "title-elm" ]
                [ text "elm" ]
            ]
        , timeSelector model
        , div
            [ class "time-display-container" ]
            (List.concat
                [ [ timeButton (Increment Min) "edit-min-up-btn" "min-up" "+1m" (model.currentTime >= 999 * 60)
                  , timeButton (Increment Sec) "edit-sec-up-btn" "second-up" "+1s" (model.currentTime >= 1000 * 60)
                  ]
                , displayTime model
                , [ timeButton (Decrement Min) "edit-min-down-btn" "min-down" "-1m" (model.currentTime <= 60)
                  , timeButton (Decrement Sec) "edit-sec-down-btn" "sec-down" "-1s" (model.currentTime <= 0)
                  , timeEditable model
                  ]
                ]
            )
        , div
            [ class "btn-group controls" ]
            [ controlButton Reset (messageToString Reset) (model.currentTime == model.initialTime)
            , controlButton Pause (messageToString Pause) (not model.running)
            , controlButton Start (messageToString Start) (model.running || model.currentTime == 0)
            ]
        ]


messageToString : Msg -> String
messageToString msg =
    case msg of
        Start ->
            "start"

        Pause ->
            "pause"

        Reset ->
            "reset"

        _ ->
            ""


controlButton : Msg -> String -> Bool -> Html Msg
controlButton msg name disable =
    button
        [ class ("btn btn-large " ++ name ++ "-btn")
        , onClick msg
        , disabled disable
        ]
        [ text name ]


timeButton : Msg -> String -> String -> String -> Bool -> Html Msg
timeButton msg className nameString textString disable =
    button
        [ class ("btn edit-btn " ++ className)
        , name nameString
        , onClick msg
        , disabled disable
        ]
        [ text textString ]


timeEditable : Model -> Html Msg
timeEditable model =
    input
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


timeSelector : Model -> Html Msg
timeSelector model =
    div
        [ class "time-selector" ]
        [ div
            [ class "btn-group controls" ]
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
            ( { model | initialTime = model.currentTime, running = True }, Cmd.none )

        Set seconds ->
            ( { model | initialTime = seconds, currentTime = seconds, running = False }, Cmd.none )

        Increment Sec ->
            ( { model | currentTime = model.currentTime + 1 }, Cmd.none )

        Decrement Sec ->
            ( { model | currentTime = model.currentTime - 1 }, Cmd.none )

        Increment Min ->
            ( { model | currentTime = model.currentTime + 60 }, Cmd.none )

        Decrement Min ->
            ( { model | currentTime = model.currentTime - 60 }, Cmd.none )

        Reset ->
            ( { model | currentTime = model.initialTime, running = False }, Cmd.none )

        Tick time ->
            let
                running =
                    model.currentTime /= 0

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
