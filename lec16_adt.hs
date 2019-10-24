data Contest = Rock | Scissor | Paper

winner :: Contest -> Contest -> String
winner Scissor Paper = "Player One Won!"
winner Scissor Rock = "Player Two Won!"
winner Rock Scissor = "Player One Won!"
winner Rock Paper = "Player Two Won!"
winner Paper Rock = "Palery One Won!"
winner Paper Scissor = "Player Two Won!"
winner _ _ = "Tie! Try again!"

data Velocity = MPS Double | FPS Double 

{-
class Eq a where  
    (==) :: a -> a -> Bool  
    (/=) :: a -> a -> Bool  
    x == y = not (x /= y)  
    x /= y = not (x == y)
-}

instance Eq Velocity where
  (==) vel1 vel2 = inMPS vel1 == inMPS vel2

instance Show Velocity where
  show (MPS x) = (show x) ++ " m/s"
  show (FPS x) = (show x) ++ " f/s"

inMPS :: Velocity -> Double
inMPS (MPS x) = x
inMPS (FPS x) = x / 3.28

readVelocity str = 
  case words str of
    [x, "m/s"] -> MPS (read x)
    [x, "f/s"] -> FPS (read x)

type Point = (Double, Double)
data Shape = Circle Point Double 
           | Rectangle Point Point
           deriving (Show, Eq)

area :: Shape -> Double
area (Circle center r) = pi*r^2
area (Rectangle (x1,y1) (x2,y2)) = abs $ (y2-y1) * (x2-x1)

