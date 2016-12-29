import Html exposing (Html, div, node, text)
import List exposing (map)
import Collage exposing (Form, collage, rect, filled, move, moveY, group)
import Element exposing (toHtml)
import Color exposing (white, black, darkGrey)
import Time exposing (Time)
import Tuple exposing (first, second)
import Keyboard.Extra as KE
import AnimationFrame


type alias Model =
  { keys : KE.Model
  , player1 : Float
  , player2 : Float
  , ball : (Float, Float)
  , speed : (Float, Float)
  }

type Direction = Up | Down

type WhichPlayer = Player1 | Player2

type Msg = Keys KE.Msg | Tick Time | Nop


paddleSpeed : Float
paddleSpeed = 0.01

w : number
w = 400

h : number
h = 200

init : ( Model, Cmd Msg )
init =
  let
    ( keys, cmd ) = KE.init
  in
    ( { keys = keys
      , player1 = 0 -- float 0 1 -- start player at random y
      , player2 = 0 -- float 0 1
      , ball = (0.3, 0.5) -- (float 0 1, float 0 1)
      , speed = (0.4, 0.1)
    }, Cmd.map Keys cmd)


moveBall : Time -> Model -> Model
moveBall time model =
  { model | ball =
    ( (first model.ball) + (first model.speed) * time
    , (second model.ball) + (second model.speed) * time) }

collide : Model -> Model
collide model =
  let
    (ball_x, ball_y) = model.ball
    (speed_x, speed_y) = model.speed
    dx = (0.5 - 0.46 + 0.02)
  in
    if ball_x <= dx
      && (ball_y - 0.5 <= model.player1 + 0.1 && ball_y - 0.5 >= model.player1 - 0.1) -- ball hit player1
    then
      { model
      | speed = (-1 * speed_x, speed_y)
      , ball = (dx, ball_y) }
    else if ball_x >= 1 - dx
      && (ball_y - 0.5 <= model.player2 + 0.1 && ball_y - 0.5 >= model.player2 - 0.1) -- ball hit player2
    then
      { model
      | speed = (-1 * speed_x, speed_y)
      , ball = (1 - dx, ball_y) }
    else if ball_y < 0 + 0.01 -- bottom
    then
      { model
      | speed = (speed_x, -1 * speed_y)
      , ball = (ball_x, ball_y + 0.01) }
    else if ball_y > 1 - 0.01 -- top
    then
      { model
      | speed = (speed_x, -1 * speed_y)
      , ball = (ball_x, ball_y - 0.01) }
    else model


movePlayer : KE.Model -> KE.Key -> KE.Key -> Time -> Float -> Float
movePlayer keys k1 k2 dt y =
  if KE.isPressed k1 keys && y < 0.4
  then y + paddleSpeed
  else if KE.isPressed k2 keys && y > -0.4
  then y - paddleSpeed
  else y

movePlayers : Time -> Model -> Model
movePlayers dt model =
  { model
  | player1 = movePlayer model.keys KE.CharQ KE.CharA dt model.player1
  , player2 = movePlayer model.keys KE.CharP KE.CharL dt model.player2
  }

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    Keys keyMsg ->
      let ( keys, cmd ) = KE.update keyMsg model.keys
      in ( { model | keys = keys }, Cmd.map Keys cmd )
    Tick dt ->
      (model |> movePlayers dt |> moveBall dt |> collide, Cmd.none)
    Nop ->
      (model, Cmd.none )


ball : ( Float, Float ) -> Form
ball (x, y) =
  rect (w * 0.02) (w * 0.02)
  |> filled white
  |> move ((x - 0.5) * w, (y - 0.5) * h)

player : WhichPlayer -> Float -> Form
player which y =
  let mult = case which of
    Player1 -> -1
    Player2 -> 1
  in rect (w * 0.02) (h * 0.2)
    |> filled white
    |> move (mult * 0.46 * w, y * h)

bg : Form
bg =
  group <|
    [ rect w h |> filled black ]
    ++ (List.range 0 4 |> map (λn ->
      rect (w * 0.02) (h * 0.16)
      |> filled darkGrey
      |> moveY ((toFloat n - 2) * h / 4.0)))

view : Model -> Html Msg
view model =
  div []
    [ node "style" [] [ text "html { background: #111; color: #eee; }" ]
    , toHtml <| collage w h
      [ bg
      , player Player1 model.player1
      , player Player2 model.player2
      , ball model.ball
      ]
    , text "keys: player 1: Q A, player 2: P L"
    ]

subscriptions : Model -> Sub Msg
subscriptions model =
  Sub.batch
    [ AnimationFrame.diffs (λt -> Tick (t / 1000.0))
    , Sub.map Keys KE.subscriptions
    ]

main : Program Never Model Msg
main = Html.program
  { init = init
  , view = view
  , update = update
  , subscriptions = subscriptions
  }
