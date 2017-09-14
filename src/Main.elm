module Main exposing (..)

import Html exposing (Html, div, p)
import Html.CssHelpers exposing (withNamespace)
import Html.Attributes exposing (id, src)
import MainCss
import Assets exposing (url, hero)


type Command = TurnRight | TurnLeft | Move
type Direction = North | East | South | West
type Status = Ok | Kabum
type alias Position = { x: Int, y: Int }

type alias Dimension = { x: Int, y: Int }

type alias Rover = { position: Position
                   , direction: Direction
                   , status: Status}

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
    Ok -> case command of
   TurnRight -> { rover | direction = turnRight rover.direction }
   TurnLeft -> { rover | direction = turnLeft rover.direction }
   Move -> move rover dimension

{ id, class, classList } =
    withNamespace "main"

main : Html a
main =
    div []
        [ div [ id MainCss.Page ] [ Html.text "Mars explorer" ]
        , div []
            [ p [] [ Html.text "is a paragraph" ]
            , p [] [ Html.text (toString West)]
            ]
        ]
