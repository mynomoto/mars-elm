module Main exposing (..)

import Html exposing (Html, div, p)
import Html.CssHelpers exposing (withNamespace)
import Html.Attributes exposing (id, src)
import Svg exposing (..)
import Svg.Attributes exposing (..)
import MainCss
import Assets exposing (url, hero)
import Arithmetic
import List.Extra

type Command = TurnRight | TurnLeft | Move
type Direction = North | East | South | West
type Status = Working | Kabum
type alias Position = { x: Int, y: Int }

type alias Dimension = { x: Int, y: Int }

type alias Rover = { position: Position
                   , direction: Direction
                   , status: Status
                   , commands: List Command
                   }

type alias Mars = { rovers: List Rover
                  , dimension: Dimension
                  }

turnRight : Direction -> Direction
turnRight direction = case direction of
    North -> East
    East -> South
    South -> West
    West -> North

turnLeft : Direction -> Direction
turnLeft direction = case direction of
    North -> West
    West -> South
    South -> East
    East -> North

outOfBounds : Position -> Dimension -> Bool
outOfBounds position dimension = position.x > dimension.x || position.y > dimension.y

updateRover : Rover -> Position -> Dimension -> Rover
updateRover rover position dimension = if outOfBounds position dimension then
        { rover | status = Kabum }
    else
        { rover | position = position }

move : Rover -> Dimension -> Rover
move rover dimension = case rover.direction of
    North -> let
        position = rover.position
        newPosition = { position | y = rover.position.y + 1 }
    in
        updateRover rover newPosition dimension
    East -> let
        position = rover.position
        newPosition = { position | y = rover.position.y + 1 }
    in
            updateRover rover newPosition dimension
    South -> let
        newPosition = { position | y = rover.position.y + 1 }
        position = rover.position
    in
            updateRover rover newPosition dimension
    West -> let
        newPosition = { position | y = rover.position.y + 1 }
        position = rover.position
    in
            updateRover rover newPosition dimension

runCommand : Rover -> Dimension -> Command -> Rover
runCommand rover dimension command = case rover.status of
    Kabum -> rover
    Working -> case command of
   TurnRight -> { rover | direction = turnRight rover.direction }
   TurnLeft -> { rover | direction = turnLeft rover.direction }
   Move -> move rover dimension

parseCharCommands : List Char -> List Command
parseCharCommands input = case input of
  [] -> []
  'M' :: rest -> Move :: parseCharCommands rest
  'R' :: rest -> TurnRight :: parseCharCommands rest
  'L' :: rest -> TurnLeft :: parseCharCommands rest
  _ :: rest -> parseCharCommands rest

parseDimensionInput : List String -> Maybe Dimension
parseDimensionInput input = case input of
   x :: y :: [] -> case String.toInt x of
      Err _ -> Nothing
      Ok xInt -> case String.toInt y of
         Err _ -> Nothing
         Ok yInt -> Just { x = xInt
                         , y = yInt
                         }
   _ -> Nothing

parseDimension : String -> Maybe Dimension
parseDimension input = input |> String.trim |> String.words |> parseDimensionInput

parseProbeInput : List String -> List Command -> Maybe Rover
parseProbeInput input commands = case input of
   x :: y :: d :: [] -> case parseDimensionInput [x, y] of
      Nothing -> Nothing
      Just position -> case String.toUpper d of
         "N" -> Just { position = position
                     , direction = North
                     , status = Working
                     , commands = commands
                     }
         "E" -> Just { position = position
                     , direction = East
                     , status = Working
                     , commands = commands
                     }
         "S" -> Just { position = position
                     , direction = South
                     , status = Working
                     , commands = commands
                     }
         "W" -> Just { position = position
                     , direction = West
                     , status = Working
                     , commands = commands
                     }
         _ -> Nothing

   _ -> Nothing

parseProbe : List String -> Maybe Rover
parseProbe input =
  case input of
    rover :: commandString :: [] -> let
        roverInput = rover |> String.trim |> String.words
        commands = commandString |> String.trim |> String.toUpper |> String.toList |> parseCharCommands
          in
        parseProbeInput roverInput commands
    _ -> Nothing

{ id, class, classList } =
    withNamespace "main"

parseMars : List String -> Maybe Mars
parseMars input = case input of
  dimension :: rest -> if Arithmetic.isEven(List.length(rest)) then
      let
          roversInput = List.Extra.groupsOf 2 rest
          mrovers = List.map parseProbe roversInput
          mdimensions = parseDimension dimension
      in
          case mdimensions of
            Nothing -> Nothing
            Just dimensions -> if List.member Nothing mrovers then
              Nothing
            else
              Just { rovers = List.map (Maybe.withDefault { position = { x = 0, y = 0}
                     , direction = East
                     , status = Kabum
                     , commands = []
                     }) mrovers
                   , dimension = dimensions
                   }
    else
      Nothing
  _ -> Nothing

parseTextArea : String -> Maybe Mars
parseTextArea input = input |> String.lines |> List.map String.trim |> List.filter (not << String.isEmpty) |> parseMars

roundRect : Html msg
roundRect =
    svg
      [ width "120", height "120", viewBox "0 0 120 120" ]
      [ rect [ x "10", y "10", width "100", height "100", rx "15", ry "15" ] [] ]


trianglePoints : String
trianglePoints = "24,12.5 1,5 1,20"

hexagonPoints : String
hexagonPoints = "0,18 0,7 7,0 18,0 25,7 25,18 18,25 7,25"

directionToAngle : Direction -> String
directionToAngle direction = case direction of
  East -> "0"
  South -> "90"
  West -> "180"
  North -> "270"

showRover : Rover -> Svg msg
showRover rover = let
    points_ = case rover.status of
            Kabum -> hexagonPoints
            Working -> trianglePoints
      in
          polygon [ points points_, transform ("translate(" ++ (toString rover.position.x) ++ " " ++ (toString rover.position.y) ++ ") rotate(" ++ (directionToAngle rover.direction) ++ " 12.5 12.5)") ] []

cellSize : Int
cellSize = 25

showMars : Mars -> Svg msg
showMars mars =
  let
      xcells =  (Basics.max mars.dimension.x 0) + 1
      ycells =  (Basics.max mars.dimension.y 0) + 1
  in
  svg
      [ width "120", height "120", Svg.Attributes.style "border: 1px solid red" ]
      []

marsm : Maybe Mars
marsm = Just { rovers = []
            , dimension  = { x = 3
                           , y = 5 }}

main : Html a
main =
    div []
        [ div [ id MainCss.Page ] [ Html.text "Mars explorer" ]
        , div []
            [ case marsm of
              Nothing -> Html.text "No planet to render"
              Just mars -> showMars mars]
            ]
