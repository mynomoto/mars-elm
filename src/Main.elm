module Main exposing (..)

import Html exposing (Html, div, p)
import Html.CssHelpers exposing (withNamespace)
import Html.Attributes exposing (id, src)
import MainCss
import Assets exposing (url, hero)

type Command = TurnRight | TurnLeft | Move
type Direction = North | East | South | West
type Status = Working | Kabum
type alias Position = { x: Int, y: Int }

type alias Dimension = { x: Int, y: Int }

type alias Rover = { position: Position
                   , direction: Direction
                   , status: Status
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

parseCommand : String -> List Command
parseCommand input = input |> String.trim |> String.toUpper |> String.toList |> parseCharCommands

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

parseProbeInput : List String -> Maybe Rover
parseProbeInput input = case input of
   x :: y :: d :: [] -> case parseDimensionInput [x, y] of
      Nothing -> Nothing
      Just position -> case String.toUpper d of
         "N" -> Just { position = position
                     , direction = North
                     , status = Working
                     }
         "E" -> Just { position = position
                     , direction = East
                     , status = Working
                     }
         "S" -> Just { position = position
                     , direction = South
                     , status = Working
                     }
         "W" -> Just { position = position
                     , direction = West
                     , status = Working
                     }
         _ -> Nothing

   _ -> Nothing

parseProbe : String -> Maybe Rover
parseProbe input = input |> String.trim |> String.words |> parseProbeInput

{ id, class, classList } =
    withNamespace "main"

-- Continue from here
parseMars : List String -> Maybe Mars
parseMars input = Nothing

parseTextArea : String -> Maybe Mars
parseTextArea input = input |> String.lines |> List.map String.trim |> List.filter String.isEmpty |> parseMars

main : Html a
main =
    div []
        [ div [ id MainCss.Page ] [ Html.text "Mars explorer" ]
        , div []
            [ p [] [ Html.text "is a paragraph" ]
            , p [] [ Html.text (toString West)]
            ]
        ]
