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
            kaelinVelY :: Float,
        spawnHeld :: Bool,
        spawnTimer :: Float,
        friendlyProjectiles :: [(Float, Float)],
        projectileVelocity :: Float,
        enemies :: [(Float, Float)],
        enemySpeed :: Float,
        enemyProjectiles :: [(Float, Float)]
    } deriving Show

render :: KaelinGame -> Picture
render game =
        pictures (kaelin : (enemyPics ++ projectilePics ++ enemyProjectilePics))
    where
        kaelin = color white $ uncurry translate (kaelinLoc game) $ circleSolid 10
        enemyPics = map (\t -> uncurry translate t $ color orange $ circleSolid 10) (enemies game)
        projectilePics = map (\t -> uncurry translate t $ color green $ circleSolid 3) (friendlyProjectiles game)
        enemyProjectilePics = map (\t -> uncurry translate t $ color red $ circleSolid 3) (enemyProjectiles game)


initialState :: KaelinGame
initialState = Game
    { kaelinLoc = (0, -200),
            kaelinVelX = 0,
            kaelinVelY = 0,
            spawnHeld = False,
            spawnTimer = 0,
        friendlyProjectiles = [],
        projectileVelocity = 300,
        enemies = [],
        enemySpeed = 100,
        enemyProjectiles = []
    }

moveKaelin :: Float -> KaelinGame -> KaelinGame
moveKaelin seconds game = game { kaelinLoc = (x', y') }
    where
        (x, y) = kaelinLoc game
        vx = kaelinVelX game
        vy = kaelinVelY game

        x' = x + vx * seconds
        y' = y + vy * seconds

moveProjectile :: Float -> KaelinGame ->  KaelinGame
moveProjectile seconds game = game { friendlyProjectiles = filter inBounds moved }
    where
        moved = map moveOne (friendlyProjectiles game)
        moveOne (x, y) = (x, y + projectileVelocity game * seconds)
        inBounds (_, y) = y >= (-fromIntegral height / 2) && y <= fromIntegral height / 2

moveEnemyProjectile :: Float -> KaelinGame -> KaelinGame
moveEnemyProjectile seconds game = game { enemyProjectiles = filter inBounds moved }
    where
        moved = map moveOne (enemyProjectiles game)
        moveOne (x, y) = (x, y - projectileVelocity game * seconds)
        inBounds (_, y) = y >= (-fromIntegral height / 2) && y <= fromIntegral height / 2

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

handleKeys (EventKey (SpecialKey KeySpace) Down _ _) game =
    game { spawnHeld = True }

handleKeys (EventKey (SpecialKey KeySpace) Up _ _) game =
    game { spawnHeld = False }

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
update seconds game =
    let moved = moveProjectile seconds (moveKaelin seconds game)
        timer = spawnTimer moved - seconds
        cooldown = 0.5
    in if spawnHeld moved && timer <= 0
       then moved { friendlyProjectiles = kaelinLoc moved : friendlyProjectiles moved
                  , spawnTimer = cooldown }
       else moved { spawnTimer = max 0 timer }