module MarsRovers
( Direction (North, East, West, South)
, Rotation (CounterClockWise, ClockWise)
, Position
, Rover
, Plateau
, Command
, Commands
, move
, turn
, command
, isOut
, isValidCommands
, doAllCommands
, runRover
) where

import           Data.Maybe

data Direction = North | East | West | South deriving(Show)
data Rotation = CounterClockWise | ClockWise deriving(Show)
type Position = (Int, Int)
type Rover = (Position, Direction)
type Command = Char
type Commands = [Command]
type Plateau = Position

move :: Rover -> Rover
move ((x, y), North) = ((x, y + 1), North)
move ((x, y), East)  = ((x + 1, y), East)
move ((x, y), West)  = ((x - 1, y), West)
move ((x, y), South) = ((x, y - 1), South)

isOut :: Rover -> Plateau -> Bool
isOut ((x, y), _) (px, py)
    | (x > px) || (y > py) = True
    | (x < 0) || (y < 0) = True
    | otherwise = False

turn :: Rotation -> Rover -> Rover
turn ClockWise        ((x, y), North) = ((x, y), East)
turn CounterClockWise ((x, y), North) = ((x, y), West)
turn ClockWise        ((x, y), East)  = ((x, y), South)
turn CounterClockWise ((x, y), East)  = ((x, y), North)
turn ClockWise        ((x, y), West)  = ((x, y), North)
turn CounterClockWise ((x, y), West)  = ((x, y), South)
turn ClockWise        ((x, y), South) = ((x, y), West)
turn CounterClockWise ((x, y), South) = ((x, y), East)

isValidCommands :: Commands -> Bool
isValidCommands ""              = True
isValidCommands (x:xs)
    | x `elem` ['l', 'm', 'r']  = isValidCommands xs
    | otherwise                 = False

command :: Command -> Rover -> Rover
command 'm' = move
command 'l' = turn CounterClockWise
command 'r' = turn ClockWise

doAllCommands :: Rover -> Commands -> Plateau -> Maybe Rover
doAllCommands r "" p
    | isOut r p = Nothing
    | otherwise = Just r
doAllCommands r (x:xs) p
    | isOut r p = Nothing
    | otherwise = doAllCommands (command x r) xs p

runRover :: Rover -> Plateau -> Commands -> String
runRover r p c =
    if isValidCommands c
        then case doAllCommands r c p of
            Nothing -> "out of plateau"
            Just r  -> show r
        else "invalid command"
