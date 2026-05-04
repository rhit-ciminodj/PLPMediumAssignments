module Main(main) where

import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game

width, height, offset :: Int
width = 1000
height = 1000
offset = 100

window :: Display
window = InWindow "Pong" (width, height) (offset, offset)

background :: Color
background = black

data KaelinGame = Game
    { kaelinLoc :: (Float, Float),
      kaelinVel :: (Float, Float),
      } deriving Show

render :: KaelinGame -> Picture
render game = 
    pictures [kaelin]
    where
        kaelin = uncurry translate (kaelinLoc game) $ circleSolid 10


initialState :: PongGame
initialState = Game
    { ballLoc = (0, 200),
      ballVel = (0, 0),
    }

moveBall :: Float -> PongGame -> PongGame
moveBall seconds game = game { ballLoc = (x', y') }
    where
        (x, y) = ballLoc game
        (vx, vy) = ballVel game

        x' = x + vx * seconds
        y' = y + vy * seconds

fps :: Int
fps = 60

wallBounce :: PongGame -> PongGame
paddleBounce :: PongGame -> PongGame

type Radius = Float
type Position = (Float, Float)

wallCollision :: Position -> Radius -> Bool
wallCollision (_, y) radius = topCollision || bottomCollision
    where
        topBoundary = fromIntegral height / 2
        bottomBoundary = negate topBoundary

        topCollision = y + radius >= topBoundary
        bottomCollision = y - radius <= bottomBoundary

wallBounce game = game { ballVel = (vx, vy') }
    where
        radius = 10
        (vx, vy) = ballVel game
        
        vy' = if wallCollision (ballLoc game) radius
            then
                -vy
            else
                vy

paddleBounce game
    | hitPaddle = game { ballVel = (-vx, vy) }
    | otherwise  = game
  where
    radius = 10
    (vx, vy) = ballVel game
    (x, y) = ballLoc game

    paddleX = 120
    paddleHalfWidth = 13
    paddleHalfHeight = 43

    hitPaddle =
      (abs (x - paddleX) <= paddleHalfWidth + radius && abs (y - player1 game) <= paddleHalfHeight + radius) ||
      (abs (x + paddleX) <= paddleHalfWidth + radius && abs (y - player2 game) <= paddleHalfHeight + radius)
      
paddleSpeed :: Float
paddleSpeed = 20

handleKeys :: Event -> PongGame -> PongGame

handleKeys (EventKey (Char 'r') _ _ _) game =
    game { ballLoc = (0, 0) }

handleKeys (EventKey (Char 'p') Down _ _) game = 
    game { paused = not (paused game) }

handleKeys (EventKey (Char 'w') Down _ _) game =
    game { player2 = (player2 game) + paddleSpeed }

handleKeys (EventKey (Char 's') Down _ _) game =
    game { player2 = (player2 game) - paddleSpeed }

handleKeys (EventKey (SpecialKey KeyUp) Down _ _) game =
    game { player1 = (player1 game) + paddleSpeed }

handleKeys (EventKey (SpecialKey KeyDown) Down _ _) game =
    game { player1 = (player1 game) - paddleSpeed }

handleKeys _ game = game

main :: IO ()
main = play window background fps initialState render handleKeys update

update :: Float -> PongGame -> PongGame
update seconds game = if paused game then game else paddleBounce $ wallBounce $ moveBall seconds game