module Data.Cell where

data State = Alive | Dead
 deriving (Eq, Show)

data Cell = Cell State (Int, Int)
 deriving (Eq)

instance Show Cell where
 show (Cell s ind) = (show s) ++ " " ++ (show ind)

isAlive (Cell s _) = s == Alive
isDead = not . isAlive
revState (Cell s xy) = case s of
 Alive -> Cell Dead xy
 Dead  -> Cell Alive xy

birth (Cell _ xy) = Cell Alive xy
death = revState . birth
