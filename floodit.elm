module Main exposing (main)

import Html exposing (div, span, button, text)
import Html.Attributes exposing (class, style)
import Html.Events exposing (onClick)
import Random
import Array exposing (Array)


{- Styles -}


scoreStyle : Html.Attribute msg
scoreStyle =
    style [ ( "margin", "10px" ), ( "font-size", "16px" ), ( "font-weight", "bold" ) ]


gameStyle : Html.Attribute msg
gameStyle =
    style [ ( "display", "inline-block" ) ]


panelStyle : Html.Attribute msg
panelStyle =
    style [ ( "display", "inline-block" ), ( "padding", "5px" ) ]


buttonStyle : Html.Attribute msg
buttonStyle =
    style [ ( "background-color", "#bdc3c7" ), ( "margin", "20px" ) ]


backgroundStyle : Html.Attribute msg
backgroundStyle =
    style
        [ ( "text-align", "center" )
        , ( "background-color", "#ecf0f1" )
        , ( "height", "100vh" )
        ]


cellStyle : String -> Html.Attribute msg
cellStyle color =
    style
        [ ( "display", "inline-block" )
        , ( "height", "30px" )
        , ( "vertical-align", "top" )
        , ( "width", "30px" )
        , ( "background-color", color )
        , ( "transition", "background-color 250ms linear" )
        ]


{--}
type alias ColorMatrix =
    List (List Int)


type alias GameConfig =
    { rows : Int
    , columns : Int
    , colors : Array String
    }


type alias Game =
    { matrix : ColorMatrix
    , stepNumber : Int
    }


type alias GameApp =
    { game : Maybe Game
    , config : GameConfig
    }


type alias Model =
    GameApp


type Msg
    = Click Int
    | InitNewGame ColorMatrix
    | NewGame


type alias Point =
    { x : Int, y : Int }


model : Model
model =
    { game = Nothing
    , config =
        { rows = 10
        , columns = 10
        , colors = Array.fromList [ "#e74c3c", "#27ae60", "#9b59b6", "#f1c40f", "#34495e", "#3498db" ]
        }
    }


getCurrentColor : List (List number) -> number
getCurrentColor matrix =
    matrix
        |> List.head
        |> Maybe.withDefault []
        |> List.head
        |> Maybe.withDefault 0


convertListToArray : List (List a) -> Array (Array a)
convertListToArray list =
    Array.fromList (List.map (\list2 -> Array.fromList list2) list)


isWon : Int -> ColorMatrix -> Bool
isWon currentColor matrix =
    not (List.any (\l2 -> List.any (\cellColor -> currentColor /= cellColor) l2) matrix)


viewCell : Array String -> Int -> Html.Html Msg
viewCell colors cell =
    let
        pickColor number =
            Maybe.withDefault "white" (Array.get number colors)
    in
        div [ class "game-cell", onClick (Click cell), cellStyle <| pickColor cell ] []


viewGame :
    { d
        | config : { a | colors : Array String }
        , game : Maybe { c | matrix : List (List Int), stepNumber : b }
    }
    -> Html.Html Msg
viewGame model =
    let
        viewMatrix matrix =
            let
                viewRow row =
                    div [ class "game-row" ] (List.map (viewCell model.config.colors) row)
            in
                List.map viewRow matrix

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
    in
        case model.game of
            Just game ->
                let
                    { matrix, stepNumber } =
                        game

                    currentColor =
                        getCurrentColor matrix
                in
                    div [ gameStyle ]
                        [ div [] (viewMatrix matrix)
                        , viewScore (isWon currentColor matrix) stepNumber
                        ]

            Nothing ->
                div [] []


viewPanel : GameConfig -> Html.Html Msg
viewPanel { colors } =
    div [] (List.intersperse (div [ panelStyle ] []) (List.map (viewCell colors) (List.range 0 ((Array.length colors) - 1))))


view : GameApp -> Html.Html Msg
view model =
    div [ class "background", backgroundStyle ]
        [ div [] [ button [ buttonStyle, onClick NewGame ] [ text "New Game" ] ]
        , viewGame model
        , viewPanel model.config
        ]


update : Msg -> GameApp -> ( GameApp, Cmd Msg )
update msg model =
    let
        step currentColor stepColor point matrix =
            let
                getX x m =
                    Maybe.withDefault (Array.fromList []) (Array.get x m)

                getXY x y matrix =
                    matrix
                        |> Array.get x
                        |> Maybe.withDefault (Array.fromList [])
                        |> Array.get y
                        |> Maybe.withDefault -1

                setXY x y value matrix =
                    Array.set x (Array.set y value <| getX x matrix) matrix

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
            Array.toList <| Array.map (\a2 -> Array.toList a2) a

        makeStep stepColor matrix =
            let
                currentColor =
                    getCurrentColor matrix

                array =
                    step currentColor stepColor (Point 0 0) (convertListToArray matrix)
            in
                convertArrayToList array
    in
        case msg of
            InitNewGame matrix ->
                ( { model | game = Just (Game matrix 0) }, Cmd.none )

            Click stepColor ->
                ( case model.game of
                    Just game ->
                        let
                            { matrix, stepNumber } =
                                game

                            currentColor =
                                getCurrentColor matrix
                        in
                            if (stepColor == currentColor || isWon currentColor matrix) then
                                model
                            else
                                { model | game = Just (Game (makeStep stepColor matrix) (stepNumber + 1)) }

                    Nothing ->
                        model
                , Cmd.none
                )

            NewGame ->
                ( { model | game = Nothing }, generateColorMatrix model.config )


subscriptions : a -> Sub msg
subscriptions model =
    Sub.none


generateColorMatrix : GameConfig -> Cmd Msg
generateColorMatrix { rows, columns, colors } =
    Random.generate InitNewGame
        (Random.list rows (Random.list columns (Random.int 0 ((Array.length colors) - 1))))


init : ( Model, Cmd Msg )
init =
    ( model, generateColorMatrix model.config )


main : Program Never Model Msg
main =
    Html.program { init = init, update = update, subscriptions = subscriptions, view = view }
