import Html exposing (Html, Attribute)
import Html.App
import Html exposing (Html, Attribute, text, div, input, span, p)
import Html.Attributes exposing (id, class)
import Html.Events exposing (onClick)
import Html.App as Html
import Time exposing (Time, second)
import Graphics.Render as Render
import Color exposing (rgb)
import Keyboard exposing (KeyCode)
import Char

type alias Model =
    { time : Time
    , knocks : List Event
    , timeSince : Time
    }

type alias Event =
    { time : Time
    , key : KeyCode
    }


type Msg
    = Tick Time
    | Normal
    | HandleKeyPress Int



init : (Model, Cmd Msg)
init =
    (Model 0 [] 0, Cmd.none)

subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ Keyboard.presses HandleKeyPress
        , Time.every Time.millisecond Tick
        ]


update : Msg -> Model -> (Model, Cmd Msg)
update action model =
    case action of
        Tick time ->
            ({ model | time = time, timeSince = model.timeSince + 1 }, Cmd.none)
        HandleKeyPress key ->
            ({ model | knocks = (Event model.time key) :: (List.take 10 model.knocks), timeSince = 0 }, Cmd.none)
        Normal ->
            (model, Cmd.none)

view : Model -> Html Msg
view model =
    div [centered] [
         div []
             [ timeline model ]
        , text (toString model.time)
        , p [] [text (toString model.timeSince)]
        , p [] [text (toString model.knocks)]
        ]

timeline : Model -> Html Msg
timeline model =
    Render.group
        (List.concat
             [ (List.map (knockShape knockPositionX model.timeSince model.time) model.knocks)
             , (List.map (knockShape knockPositionX' model.timeSince model.time) model.knocks)
             , (List.map (knockShape knockPositionY model.timeSince model.time) model.knocks)
             , (List.map (knockShape knockPositionY' model.timeSince model.time) model.knocks)
             , [redicle]
             ]
        )
        |> Render.svg 1000 1000

redicle : Render.Form Msg
redicle =
    Render.rectangle 10 10
        |> Render.solidFillWithBorder (rgb 255 255 255) 10 (rgb 100 100 100)


knockShape : (Time -> Time -> (Float, Float)) -> Float -> Time -> Event -> Render.Form Msg
knockShape positionfn since time knock =
    (knockShaper knock.key) (knockSize time knock.time) (knockSize time knock.time)
        |> Render.solidFill (rgb (round (since/5)) (round (since/3)) (round (since/2)))
        |> Render.opacity (opacitizer time knock)
        |> Render.rotate (since/100)
        |> Render.position (positionfn time knock.time)



opacitizer : Time -> Event -> Float
opacitizer time knock =
    let
        age = knock.time - time + 3500
    in
        clamp 0 0.5 (age/1000)

knockShaper : KeyCode -> (Float -> Float -> Render.Shape)
knockShaper key =
    case (Char.fromCode key) of
        'f' -> Render.rectangle
        _ -> Render.rectangle


complientCircle : Float -> Float -> Render.Shape
complientCircle x y =
    Render.rectangle y y

knockSize : Time -> Time -> Float
knockSize start knock =
    (start - knock)/10

knockPositionX : Time -> Time -> (Float, Float)
knockPositionX top knock =
        ((knock - top + 2000)/4, 0)

knockPositionX' : Time -> Time -> (Float, Float)
knockPositionX' top knock =
    (-(knock - top + 2000)/4, 0)

knockPositionY : Time -> Time -> (Float, Float)
knockPositionY top knock =
        (0, (knock - top + 2000)/4)

knockPositionY' : Time -> Time -> (Float, Float)
knockPositionY' top knock =
    (0, -(knock - top + 2000)/4)



viewTimes : List Time -> Html Msg
viewTimes knocks =
    div [] (List.map viewTime knocks)

viewTime : Time -> Html Msg
viewTime time = p [] [text (toString time)]

centered : Attribute msg
centered =
    Html.Attributes.style
        [ ("height", "1000px")
        , ("width", "1000px")
        , ("margin", "auto")
        , ("outline", "1px solid #eee")
        ]

main : Program Never
main =
  Html.program
    { init = init
    , update = update
    , view = view
    , subscriptions = subscriptions
    }
