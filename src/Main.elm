module Main exposing (main, view)

import Browser
import Html exposing (Html, a, br, button, div, footer, h1, h2, header, i, input, label, main_, p, section, span, table, tbody, td, text, th, thead, tr)
import Html.Attributes exposing (class, href, placeholder, type_, value)
import Html.Events exposing (onClick, onInput)
import Time exposing (Month(..), utc)
import Time.Extra as Time
import List.Extra as List


type alias Model =
    { tasks : List Task
    , editingTask : Task
    }

type alias Task =
    { title : String
    , start : Maybe Time.Posix
    , end : Maybe Time.Posix
    , estimate : String
    }


main : Program () Model Msg
main =
    Browser.document
        { init = always init
        , view = view
        , update = update
        , subscriptions = always Sub.none
        }


init : ( Model, Cmd Msg )
init =
    ( { tasks = [ { title = "Wakeup"
                  , start = Just <| Time.partsToPosix utc (Time.Parts 2021 Jan 1 6 0 0 0)
                  , end = Just <| Time.partsToPosix utc (Time.Parts 2021 Jan 1 6 30 0 0)
                  , estimate = ""
                  }
                , { title = "Lunch"
                  , start = Just <| Time.partsToPosix utc (Time.Parts 2021 Jan 1 12 0 0 0)
                  , end = Just <| Time.partsToPosix utc (Time.Parts 2021 Jan 1 13 15 0 0)
                  , estimate = "01:00"
                  }
                , { title = "Dinner"
                  , start = Just <| Time.partsToPosix utc (Time.Parts 2021 Jan 1 19 0 0 0)
                  , end = Nothing
                  , estimate = "01:00"
                  }
                ]
      , editingTask = {title = "Hello", start = Nothing, end = Nothing, estimate = ""}
      }
    , Cmd.none
    )


-- update

type Msg
    = AddTask Int
    | DeleteTask Int
    | SwapTask Int Int
    | ChangeTaskTitle Int String
    | ChangeTaskStart Int String
    | ChangeTaskEnd Int String
    | ChangeTaskEstimate Int String

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        AddTask i ->
            let
                firstHalf = List.take (i+1) model.tasks
                secondHalf = List.drop (i+1) model.tasks
                new = List.append [ (Task "" Nothing Nothing "" ) ] secondHalf
                    |> List.append firstHalf
            in
                ( { model | tasks = new }, Cmd.none )
        DeleteTask i ->
            let
                new = List.removeAt i model.tasks
            in
                ( { model | tasks = new }, Cmd.none )
        SwapTask me to ->
            let
                new = List.swapAt me to model.tasks
            in
                ( { model | tasks = new }, Cmd.none )
        ChangeTaskTitle i value ->
            case List.getAt i model.tasks of
                Just task ->
                    let
                        newTask = { task | title = value }
                        newTasks = List.setAt i newTask model.tasks
                    in
                        ( { model | tasks = newTasks }, Cmd.none )
                Nothing ->
                    ( model, Cmd.none )
        ChangeTaskStart i value ->
            case List.getAt i model.tasks of
                Just task ->
                    let
                        (h, m) = splitTimeStringToHourMinute value
                        today = Time.partsToPosix utc (Time.Parts 2021 Jan 1 h m 0 0)
                        newTask = { task | start = Just today }
                        newTasks = List.setAt i newTask model.tasks
                    in
                        ( { model | tasks = newTasks }, Cmd.none )
                Nothing ->
                    ( model, Cmd.none )
        ChangeTaskEnd i value ->
            case List.getAt i model.tasks of
                Just task ->
                    let
                        (h, m) = splitTimeStringToHourMinute value
                        today = Time.partsToPosix utc (Time.Parts 2021 Jan 1 h m 0 0)
                        newTask = { task | end = Just today }
                        newTasks = List.setAt i newTask model.tasks
                    in
                        ( { model | tasks = newTasks }, Cmd.none )
                Nothing ->
                    ( model, Cmd.none )
        ChangeTaskEstimate i value ->
            case List.getAt i model.tasks of
                Just task ->
                    let
                        newTask = { task | estimate = value}
                        newTasks = List.setAt i newTask model.tasks
                    in
                        ( { model | tasks = newTasks }, Cmd.none )
                Nothing ->
                    ( model, Cmd.none )

-- view

view : Model -> Browser.Document Msg
view m =
    Browser.Document
        "Instant Daily Scheduler"
        [ viewHeader
        , viewMain m
        , viewFooter
        ]

viewHeader : Html msg
viewHeader =
    header [ class "px-4 py-5" ]
           [ ]

viewFooter : Html msg
viewFooter =
    footer [ class "footer"]
           [ div [ class "content has-text-centered" ]
                 [ p []
                     [ text "Development by "
                     , a [ href "https://shootacean.com" ] [ text "shootacean." ]
                     , br [] []
                     , a [ href "https://github.com/shootacean/instant-daily-scheduler"] [ text "Source code" ]
                     ]
                 ]
           ]

viewMain : Model -> Html Msg
viewMain model =
    main_ [ class "px-3 py-3" ]
          [
          -- viewSummary model
          --, viewWorkInProgress
          div [ class "columns" ]
                [ viewSchedule model
                -- , viewScheduleVisual
                ]
          ]

viewSummary : Model -> Html msg
viewSummary model =
    section [ class "section" ]
            [ h2 [] [ text "Summary" ]
            , table [ class "table"]
                    [ tbody []
                            [ tr []
                                 [ th [] [ text "開始時間" ]
                                 , th [] [ text "終了予測時間" ]
                                 ]
                            , tr []
                                 [ td [] [ text "08:00" ]
                                 , td [] [ text "" ]
                                 ]
                            ]
                    ]
            ]

viewWorkInProgress : Html msg
viewWorkInProgress =
    section [ class "section" ]
            [ h2 [] [ text "WIP" ]
            , div [ class "is-flex" ]
                  [  ]
            , label [ class "" ] [ text "Now task..." ]
            , button [ class "button is-small is-success" ] [ text "Finish" ]
            , button [ class "button is-small is-info" ] [ text "Rest" ]
            , button [ class "button is-small is-info" ] [ text "Break" ]
            , button [ class "button is-small is-warning" ] [ text "内部割込" ]
            , button [ class "button is-small is-danger" ] [ text "外部割込" ]
            ]

viewSchedule : Model -> Html Msg
viewSchedule m =
    section [ class "section column" ]
            [ h2 [] [ text "Schedule" ]
            , viewScheduleTable m.tasks
            ]

viewScheduleTable : List Task -> Html Msg
viewScheduleTable tasks =
    table [ class "table" ]
          [ thead []
                  [ tr []
                       [ th [] [ text "" ]
                       , th [] [ text "" ]
                       , th [] [ text "" ]
                       , th [] [ text "Title" ]
                       , th [] [ text "Start" ]
                       , th [] [ text "End" ]
                       , th [] [ text "Estimate" ]
                       , th [] [ text "Actual" ]
                       , th [] [ text "Real" ]
                       , th [] [ text "" ]
                       ]
                  ]
          , tbody [] (viewScheduleRecords tasks)
          ]

viewScheduleRecords : List Task -> List (Html Msg)
viewScheduleRecords tasks =
    List.map2 viewScheduleRecord tasks (List.range 0 <| List.length tasks)

viewScheduleRecord : Task -> Int -> Html Msg
viewScheduleRecord t i =
    tr [ ]
       [ td [] [ button [ class "button is-small", onClick (SwapTask i ( i - 1 )) ] [ viewIcon "fas fa-arrow-up" ] ]
       , td [] [ button [ class "button is-small", onClick (SwapTask i ( i + 1 )) ] [ viewIcon "fas fa-arrow-down" ] ]
       , td [] [ button [ class "button is-small is-primary", onClick (AddTask i)] [ text "Add" ] ]
       , td [] [ input [ class "", type_ "text", onInput (ChangeTaskTitle i), value t.title ] [ ] ]
       , td [] [ input [ class "input", type_ "time", onInput (ChangeTaskStart i), value <| convertPosixToTimeString t.start] [] ]
       , td [] [ input [ class "input", type_ "time", onInput (ChangeTaskEnd i), value <| convertPosixToTimeString t.end ] [] ]
       , td [] [ input [ class "input", type_ "time", onInput (ChangeTaskEstimate i), value t.estimate ] [] ]
       , td [] [ label [  ] [ text <| viewActual t ] ]
       , td [] [ label [  ] [ text <| viewReal t ] ]
       , td [] [ button [ class "delete is-medium", onClick (DeleteTask i) ] [] ]
       ]

viewActual : Task -> String
viewActual t =
    let
        (hour, minute) = calcActual t
    in
        if t.start == Nothing || t.end == Nothing then
            "-"
        else
            hour ++ ":" ++ minute

viewReal : Task -> String
viewReal t =
    let
        (hour, minute) = calcActual t
    in
        if t.end /= Nothing then
            hour ++ ":" ++ minute
        else if t.estimate /= "" then
            t.estimate
        else
            "-"

viewScheduleVisual : Html msg
viewScheduleVisual =
    section [ class "section column" ]
            [ h2 [] [ text "Schedule Visual" ] ]

viewIcon : String -> Html msg
viewIcon iconClass =
    span [ class "icon" ] [ i [ class iconClass ] [] ]


-- Helpers

calcActual : Task -> (String, String)
calcActual t =
    let
        s = t.start |> Maybe.withDefault ( Time.partsToPosix utc ( Time.Parts 2021 Jan 1 0 0 0 0 ) )
        e = t.end |> Maybe.withDefault ( Time.partsToPosix utc ( Time.Parts 2021 Jan 1 0 0 0 0 ) )
        m = Time.diff Time.Minute utc s e
        hour = String.padLeft 2 '0' <| String.fromInt <| m // 60
        minute = String.padLeft 2 '0' <| String.fromInt <| modBy 60 m
    in
        (hour, minute)

splitTimeStringToHourMinute : String -> (Int, Int)
splitTimeStringToHourMinute time =
    let
        s = String.split ":" time
        h = List.getAt 0 s |> Maybe.withDefault "" |> String.toInt |> Maybe.withDefault 0
        m = List.getAt 1 s |> Maybe.withDefault "" |> String.toInt |> Maybe.withDefault 0
    in
          (h, m)

convertPosixToTimeString : Maybe Time.Posix -> String
convertPosixToTimeString maybePosix =
    case maybePosix of
        Just p ->
            let
                part = Time.posixToParts utc p
                h = String.padLeft 2 '0' <| String.fromInt part.hour
                m = String.padLeft 2 '0' <| String.fromInt part.minute
            in
                h ++ ":" ++ m
        Nothing ->
            ""
