module SpaceInvaders exposing (..)

import Browser
import Browser.Events exposing (onKeyPress)
import Json.Decode as Decode
import Html exposing (..)
import Array exposing (Array, set)
import Tuple exposing (first, second)
import Html.Attributes
import Debug exposing(log)
import List
import Time
import Set exposing (Set, empty)
import Set

dimensions : (Int, Int)
dimensions = (10, 6)

startPlayerPos : Position
startPlayerPos = ( first dimensions - 1, second dimensions // 2)

main : Program () Model Msg
main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }


-- MODEL

type CellState 
    = Empty
    | Player
    | Enemy
    | Bullet

type alias PlayArea = 
    Array (Array CellState)

type alias Position = 
    (Int, Int)

type alias Model =
    { playArea : PlayArea
    , playerPos : Position
    , enemiesPos : Set Position
    , bulletsPos : Set Position
    , gameTick : Int
    , gameOver : Bool
    , score : Int
    }

createPlayArea : PlayArea
createPlayArea =
    let
        emptyArea = Array.repeat (first dimensions) (Array.repeat (second dimensions) Empty)
    in 
        arraySetSingle Player (Just startPlayerPos) emptyArea

init : () -> (Model, Cmd Msg)
init _ =
    ( Model createPlayArea startPlayerPos spawnEnemies empty 0 False 0
    , Cmd.none
    )

-- UPDATE

type PlayerMove
  = Left
  | Right
  | Other

type Action 
  = Shoot


type Msg
    = KeyPressMove PlayerMove
    | KeyPressAction Action
    | GameTick Time.Posix

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        KeyPressMove move ->
            ( updateOnPlayerMove move model, Cmd.none )

        KeyPressAction action ->
            ( updateOnPlayerAction action model, Cmd.none )

        GameTick _ -> 
            ( updateOnGameTick model, Cmd.none )


updateOnPlayerMove : PlayerMove -> Model -> Model
updateOnPlayerMove move model =
    case move of
        Left -> 
            { model | playerPos = updatePlayerPos Left model.playerPos }
                |> updatePlayArea
        Right -> 
            { model | playerPos = updatePlayerPos Right model.playerPos }
                |> updatePlayArea
        Other -> 
            model



updateOnPlayerAction : Action -> Model -> Model
updateOnPlayerAction action model = 
    case action of 
        Shoot -> 
            { model | bulletsPos = Set.insert (advanceBullet model.playerPos) model.bulletsPos }
                |> updatePlayArea
    

updateOnGameTick : Model -> Model
updateOnGameTick model =
        { model | gameTick = model.gameTick + 1 }
            |> updateEnemyPositions
            |> updateBulletPositions
            |> updatePlayArea
            |> removeBulletsOutsidePlayArea
            |> updateScore
            |> checkGameOver
            


checkGameOver : Model -> Model
checkGameOver model =
    if List.any (\pos -> first pos == first dimensions - 1) (Set.toList model.enemiesPos) then 
        { model | gameOver = True }
    else 
        model



removeBulletsOutsidePlayArea : Model -> Model
removeBulletsOutsidePlayArea model =
    { model | bulletsPos = Set.filter (\pos -> first pos >= 0) model.bulletsPos }


advanceBullet : Position -> Position
advanceBullet startpos = 
    (first startpos - 1, second startpos)


updateBulletPositions : Model -> Model
updateBulletPositions  model = 
    { model | bulletsPos = Set.map advanceBullet model.bulletsPos }


moveEnemies : Int -> Int -> Set Position -> Set Position
moveEnemies xChange yChange set =
    set
        |> Set.map (Tuple.mapSecond (\x -> x + xChange))
        |> Set.map (Tuple.mapFirst (\y -> y + yChange))


updateEnemyPositions : Model -> Model
updateEnemyPositions model =
    let
        tick = model.gameTick
        xChange = if remainderBy 6 tick == 0 && remainderBy 12 tick == 0 then 
                1 
            else if remainderBy 6 tick == 0 then 
                -1
            else    
                0 

        yChange = if remainderBy 9 tick == 0 then 1 else 0 
    in
        { model | enemiesPos = moveEnemies xChange yChange model.enemiesPos }


updateScore : Model -> Model
updateScore model =
    let
        collissions = Set.intersect model.bulletsPos model.enemiesPos
    in
        { model | 
          enemiesPos = Set.diff model.enemiesPos collissions
        , bulletsPos = Set.diff model.bulletsPos collissions
        , score = model.score + Set.size collissions
        }


updatePlayerPos : PlayerMove -> Position -> Position
updatePlayerPos move pos =
    let
        xPos = second pos
    in
        case move of
            Left ->
                if xPos == 0 then
                    pos
                else 
                    (first pos, second pos - 1)
            Right -> 
                if xPos == second dimensions - 1  then
                    pos
                else
                    (first pos, second pos + 1)
            Other ->
                pos

updatePlayArea : Model -> Model
updatePlayArea model =
    let
        updated = Array.repeat (first dimensions) (Array.repeat (second dimensions) Empty)
            |> arraySetSingle Player (Just model.playerPos)
            |> arraySetMultiple Enemy (Set.toList model.enemiesPos)
            |> arraySetMultiple Bullet (Set.toList model.bulletsPos)

    in 

        { model | playArea = updated }



spawnEnemies : Set Position
spawnEnemies =
    let
        xs = List.filter (\x -> remainderBy 2 x == 1) (List.range 0 (second dimensions - 1))
        ys = List.repeat (List.length xs) 0
    in
        List.map2 Tuple.pair ys xs
            |> Set.fromList
    



-- SUBSCRIPTIONS

subscriptions : Model -> Sub Msg
subscriptions model =
    if not model.gameOver then
        Sub.batch 
            [ onKeyPress keyDecoder
            , Time.every 200 GameTick
            ]
    else 
        Sub.none
    
    

keyDecoder : Decode.Decoder Msg
keyDecoder =
  Decode.map keyToMessage (Decode.field "key" Decode.string)

keyToMessage : String -> Msg
keyToMessage keyVal =
    case keyVal of
    "a" ->
      KeyPressMove Left

    "d" ->
      KeyPressMove Right

    "w" ->
      KeyPressAction Shoot

    _ ->
      KeyPressMove Other

-- VIEW

view : Model -> Html Msg
view model =
    div [] [ playAreaToHTML model.playArea
           , displayGameOver model
           ] 


displayGameOver : Model -> Html Msg
displayGameOver model =
    if model.gameOver then
        div [ Html.Attributes.style "text-align" "center"
            , Html.Attributes.style "font-size" "large" ] 
            [ text ("Game Over! " ++ "Final Score: " ++ String.fromInt model.score) ]
    else 
        div [ Html.Attributes.style "text-align" "center"
            , Html.Attributes.style "font-size" "large" 
            ] 
            [ text ("Score: " ++ String.fromInt model.score) ]

playAreaToHTML : PlayArea -> Html Msg
playAreaToHTML playarea =
    div 
    [ Html.Attributes.style "text-align" "center", Html.Attributes.style "font-size" "large" ] 
    (
        playarea
            |> Array.toList
            |> List.map listToString
            |> List.map text
            |> List.map List.singleton
            |> List.map (span 
                [ Html.Attributes.style "border-left" "1px solid black"
                , Html.Attributes.style "border-right" "1px solid black"
                ])
            |> List.map List.singleton
            |> List.map (pre [])
    )


listToString : Array CellState -> String
listToString list = 
    list
        |> Array.map stateToString
        |> Array.foldr (++) ""



stateToString : CellState -> String
stateToString state = 
    case state of
        Empty -> 
            "     "
        Player -> 
            "  A  "
        Enemy -> 
            "  V  "
        Bullet -> 
            "  .  "

-- UTILITY FUNCS

arraySetMultiple : CellState -> List Position -> PlayArea -> PlayArea
arraySetMultiple state positions playarea =
    if List.isEmpty positions then
        playarea
    else 
        arraySetMultiple state (List.drop 1 positions) (arraySetSingle state (List.head positions) playarea)


arraySetSingle : CellState -> Maybe Position -> PlayArea -> PlayArea
arraySetSingle state position playarea =
    case position of
        Just pos ->
            let
                subarray = Array.get (first pos) playarea
            in
                case subarray of
                    Just array ->
                        Array.set (first pos) (Array.set (second pos) state array) playarea
                    Nothing -> 
                        playarea
        Nothing -> 
            playarea