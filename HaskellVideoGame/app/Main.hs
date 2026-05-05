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
            kaelinVelX :: Float,
            kaelinVelY :: Float
    } deriving Show

render :: KaelinGame -> Picture
render game = 
    pictures [kaelin]
    where
        kaelin = color white $ uncurry translate (kaelinLoc game) $ circleSolid 10


initialState :: KaelinGame
initialState = Game
    { kaelinLoc = (0, -200),
            kaelinVelX = 0,
            kaelinVelY = 0
    }

moveKaelin :: Float -> KaelinGame -> KaelinGame
moveKaelin seconds game = game { kaelinLoc = (x', y') }
    where
        (x, y) = kaelinLoc game
        vx = kaelinVelX game
        vy = kaelinVelY game

        x' = x + vx * seconds
        y' = y + vy * seconds

fps :: Int
fps = 60

handleKeys :: Event -> KaelinGame -> KaelinGame

handleKeys (EventKey (Char 'r') _ _ _) game =
    game { kaelinLoc = (0, 0) }

handleKeys (EventKey (Char 'w') Down _ _) game =
    game { kaelinVelY = 250}

handleKeys (EventKey (Char 's') Down _ _) game =
    game { kaelinVelY = -250}

handleKeys (EventKey (Char 'a') Down _ _) game =
    game { kaelinVelX = -250}

handleKeys (EventKey (Char 'd') Down _ _) game =
    game { kaelinVelX = 250}

handleKeys (EventKey (Char 'w') Up _ _) game =
    game { kaelinVelY = 0}

handleKeys (EventKey (Char 's') Up _ _) game =
    game { kaelinVelY = 0}

handleKeys (EventKey (Char 'a') Up _ _) game =
    game { kaelinVelX = 0}

handleKeys (EventKey (Char 'd') Up _ _) game =
    game { kaelinVelX = 0}

handleKeys _ game = game

main :: IO ()
main = play window background fps initialState render handleKeys update

update :: Float -> KaelinGame -> KaelinGame
update seconds game = moveKaelin seconds game