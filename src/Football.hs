module Football
( reactToGoal
, teamScored
, Team(TeamA, TeamB)
, Spectator(Fan, Reporter)
) where

data Team = TeamA | TeamB deriving(Show, Eq)
data Spectator = Fan Team | Reporter deriving(Show)

reactToGoal :: Team -> Spectator -> String
reactToGoal t (Fan ft)
    | t == ft = "Yay!"
    | otherwise = "Boo!"
reactToGoal t Reporter
    | t == TeamA = "Goal By Team A"
    | t == TeamB = "Goal By Team B"

teamScored :: Team -> [Spectator] -> [String]
teamScored = map . reactToGoal