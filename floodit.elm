module Main exposing (main)

import Html exposing (div, span, button, text)
import Html.Attributes exposing (class, style)
import Html.Events exposing (onClick)
import Random
import Array exposing (Array)


type alias ColorMatrix =
    List (List Int)


type alias Game =
    { matrix : ColorMatrix
    , stepNumber : Int
    }


type alias Model =
    Maybe Game


type Msg
    = Click Int
    | InitNewGame ColorMatrix
    | NewGame


type alias Point =
    { x : Int, y : Int }


model : Model
model =
    Nothing


getCurrentColor matrix =
    case List.head matrix of
        Just row ->
            case List.head row of
                Just color ->
                    color

                Nothing ->
                    0

        Nothing ->
            0


convertListToArray list =
    Array.fromList (List.map (\list2 -> Array.fromList list2) list)


getX x m =
    case Array.get x m of
        Just x ->
            x

        Nothing ->
            Array.fromList []


getXY x y matrix =
    case Array.get x matrix of
        Just m2 ->
            case Array.get y m2 of
                Just n ->
                    n

                Nothing ->
                    -1

        Nothing ->
            -1


setXY x y value matrix =
    Array.set x (Array.set y value (getX x matrix)) matrix


step currentColor stepColor point matrix =
    let
        xyColor =
            getXY point.x point.y matrix

        stepPrepare =
            step currentColor stepColor

        down =
            stepPrepare { point | x = point.x + 1 }

        right =
            stepPrepare { point | y = point.y + 1 }

        up =
            stepPrepare { point | x = point.x - 1 }

        left =
            stepPrepare { point | y = point.y - 1 }
    in
        if (xyColor == currentColor) then
            (setXY point.x point.y stepColor matrix)
                |> down
                |> right
                |> left
                |> up
        else
            matrix


convertArrayToList a =
    Array.toList (Array.map (\a2 -> Array.toList a2) a)


makeStep stepColor matrix =
    let
        currentColor =
            getCurrentColor matrix

        array =
            step currentColor stepColor (Point 0 0) (convertListToArray matrix)
    in
        convertArrayToList array


colors =
    Array.fromList [ "#e74c3c", "#27ae60", "#9b59b6", "#f1c40f", "#34495e", "#3498db" ]


colorsCount =
    Array.length colors


pickColor number =
    case Array.get number colors of
        Just color ->
            color

        Nothing ->
            "white"


won currentColor matrix =
    not (List.any (\l2 -> List.any (\cellColor -> currentColor /= cellColor) l2) matrix)


cellStyle cellColor =
    style
        [ ( "display", "inline-block" )
        , ( "height", "30px" )
        , ( "vertical-align", "top" )
        , ( "width", "30px" )
        , ( "background-color", pickColor cellColor )
        , ( "transition", "background-color 250ms linear" )
        ]


viewCell cell =
    div [ class "game-cell", onClick (Click cell), cellStyle cell ] []


viewRow row =
    div [ class "game-row" ] (List.map viewCell row)


viewMatrix matrix =
    List.map viewRow matrix


scoreStyle =
    style [ ( "margin", "10px" ), ( "font-size", "16px" ), ( "font-weight", "bold" ) ]


viewScore won stepNumber =
    div
        [ scoreStyle ]
        [ text
            (if won then
                ("You won! Score: " ++ (toString stepNumber))
             else
                ("Score: " ++ (toString stepNumber))
            )
        ]


gameStyle =
    style [ ( "display", "inline-block" ) ]


viewGame model =
    case model of
        Just game ->
            let
                { matrix, stepNumber } =
                    game

                currentColor =
                    getCurrentColor matrix
            in
                div [ gameStyle ]
                    [ div [] (viewMatrix matrix)
                    , viewScore (won currentColor matrix) stepNumber
                    ]

        Nothing ->
            div [] []


panelStyle =
    style [ ( "display", "inline-block" ), ( "padding", "5px" ) ]


viewPanel =
    div [] (List.intersperse (div [ panelStyle ] []) (List.map viewCell (List.range 0 (colorsCount - 1))))


buttonStyle =
    style [ ( "background-color", "#bdc3c7" ), ( "margin", "20px" ) ]


backgroundStyle =
    style
        [ ( "text-align", "center" )
        , ( "background-color", "#ecf0f1" )
        , ( "height", "100vh" )
        ]


view model =
    div [ backgroundStyle ]
        [ div [] [ button [ buttonStyle, onClick NewGame ] [ text "New Game" ] ]
        , viewGame model
        , viewPanel
        ]


update msg model =
    case msg of
        InitNewGame matrix ->
            ( Just (Game matrix 0), Cmd.none )

        Click stepColor ->
            ( case model of
                Just game ->
                    let
                        { matrix, stepNumber } =
                            game

                        currentColor =
                            getCurrentColor matrix
                    in
                        if (stepColor == currentColor || won currentColor matrix) then
                            model
                        else
                            Just (Game (makeStep stepColor matrix) (stepNumber + 1))

                Nothing ->
                    Nothing
            , Cmd.none
            )

        NewGame ->
            ( Nothing, generateColorMatrix )


subscriptions model =
    Sub.none


rows =
    10


columns =
    10


generateColorMatrix =
    Random.generate InitNewGame
        (Random.list rows (Random.list columns (Random.int 0 (colorsCount - 1))))


init : ( Model, Cmd Msg )
init =
    ( model, generateColorMatrix )


main =
    Html.program { init = init, update = update, subscriptions = subscriptions, view = view }
