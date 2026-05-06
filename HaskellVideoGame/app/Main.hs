module Main(main) where

import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game

width, height, offset :: Int
width = 1000
height = 1000
offset = 100

window :: Display
window = InWindow "Kaelin Game" (width, height) (offset, offset)

background :: Color
background = black

data KaelinGame = Game
    { kaelinLoc :: (Float, Float),
            kaelinVelX :: Float,
            kaelinVelY :: Float,
            keysPressed :: (Bool, Bool, Bool, Bool),
        spawnHeld :: Bool,
        spawnTimer :: Float,
        friendlyProjectiles :: [(Float, Float)],
        projectileVelocity :: Float,
        enemies :: [(Float, Float)],
        enemyVelX :: Float,
        enemyVelY :: Float,
        enemyProjectiles :: [(Float, Float)],
        enemySpawnTimer :: Float,
        enemySpawnCooldown :: Float,
        enemyFireTimer :: Float,
        enemyFireCooldown :: Float,
        loseScreen :: Float
    } deriving Show

render :: KaelinGame -> Picture
render game =
        pictures (overlayPics ++ worldPics)
    where
        kaelin = color white $ uncurry translate (kaelinLoc game) $ circleSolid 10
        enemyPics = map (\t -> uncurry translate t $ color orange $ circleSolid 10) (enemies game)
        projectilePics = map (\t -> uncurry translate t $ color green $ circleSolid 3) (friendlyProjectiles game)
        enemyProjectilePics = map (\t -> uncurry translate t $ color red $ circleSolid 3) (enemyProjectiles game)
        worldPics = kaelin : (enemyPics ++ projectilePics ++ enemyProjectilePics)
        gameOverPic = pictures
            [  translate (-230) 40 $ scale 0.5 0.5 $ color red $ text "GAME OVER"
            , translate (-260) (-40) $ scale 0.2 0.2 $ color white $ text "Press R to restart"
            ]
        overlayPics = if loseScreen game > 0 then [gameOverPic] else []


initialState :: KaelinGame
initialState = Game
    {   kaelinLoc = (0, -200),
        kaelinVelX = 0,
        kaelinVelY = 0,
        keysPressed = (False, False, False, False),
        spawnHeld = False,
        spawnTimer = 0,
        friendlyProjectiles = [],
        projectileVelocity = 300,
        enemies = [],
        enemyVelX = 0,
        enemyVelY = -100,
        enemyProjectiles = [],
        enemySpawnTimer = 0,
        enemySpawnCooldown = 2.0,
        enemyFireTimer = 1.0,
        enemyFireCooldown = 1.5,
        loseScreen = 0
    }

readInput :: KaelinGame -> KaelinGame
readInput game = game { kaelinVelX = vx, kaelinVelY = vy }
    where
        (u, d, l, r) = keysPressed game
        vy0 = if u then 250 else 0
        vy1 = if d then vy0 - 250 else vy0
        vx0 = if r then 250 else 0
        vx1 = if l then vx0 - 250 else vx0
        normalize = (u /= d) && (l /= r)
        vx = if normalize then 0.707 * vx1 else vx1
        vy = if normalize then 0.707 * vy1 else vy1

moveKaelin :: Float -> KaelinGame -> KaelinGame
moveKaelin seconds game = game { kaelinLoc = (x', y') }
    where
        (x, y) = kaelinLoc game
        vx = kaelinVelX game
        vy = kaelinVelY game

        x' = x + vx * seconds
        y' = y + vy * seconds

moveEnemy :: Float -> KaelinGame -> KaelinGame
moveEnemy seconds game = game { enemies = moveAndFilter seconds moveOne (enemies game) }
    where
        moveOne (x, y) = (x + enemyVelX game * seconds, y + enemyVelY game * seconds)
                    

moveProjectile :: Float -> KaelinGame ->  KaelinGame
moveProjectile seconds game = game { friendlyProjectiles = moveAndFilter seconds moveOne (friendlyProjectiles game) }
    where
        moveOne (x, y) = (x, y + projectileVelocity game * seconds)

moveEnemyProjectile :: Float -> KaelinGame -> KaelinGame
moveEnemyProjectile seconds game = game { enemyProjectiles = moveAndFilter seconds moveOne (enemyProjectiles game) }
    where
        moveOne (x, y) = (x, y - projectileVelocity game * seconds)

moveAndFilter :: Float -> ((Float, Float) -> (Float, Float)) -> [(Float, Float)] -> [(Float, Float)]
moveAndFilter seconds f lst = filter inBounds (map f lst)
    where
        inBounds (_, y) = y >= (-fromIntegral height / 2) && y <= fromIntegral height / 2

type Pos = (Float, Float)

collides :: Pos -> Float -> Pos -> Float -> Bool
collides (x1, y1) r1 (x2, y2) r2 =
    let dx = x1 - x2
        dy = y1 - y2
        rs = r1 + r2
    in dx*dx + dy*dy <= rs*rs

enemyCollision :: KaelinGame -> KaelinGame
enemyCollision game = game { enemies = survivors}
    where
        bullets = friendlyProjectiles game
        bulletRadius = 3
        enemyRadius = 10

        isHit enemy = any (\bullet -> collides bullet bulletRadius enemy enemyRadius) bullets
        survivors = filter (not . isHit) (enemies game)

selfCollision :: KaelinGame -> KaelinGame
selfCollision game
    | hitByEnemyProjectile || hitByEnemyBody = game { loseScreen = 1 }
    | otherwise = game
    where
        playerPos = kaelinLoc game
        playerRadius = 10
        bulletRadius = 3
        enemyRadius = 10

        hitByEnemyProjectile = any (\bullet -> collides bullet bulletRadius playerPos playerRadius) (enemyProjectiles game)
        hitByEnemyBody = any (\enemy -> collides enemy enemyRadius playerPos playerRadius) (enemies game)




    

fps :: Int
fps = 60

handleKeys :: Event -> KaelinGame -> KaelinGame

handleKeys (EventKey (Char 'r') _ _ _) game =
    initialState

handleKeys (EventKey (Char 'w') Down _ _) game =
    game { keysPressed = (True, d, l, r) }
    where
        (u, d, l, r) = keysPressed game

handleKeys (EventKey (Char 's') Down _ _) game =
    game { keysPressed = (u, True, l, r) }
    where
        (u, d, l, r) = keysPressed game

handleKeys (EventKey (Char 'a') Down _ _) game =
    game { keysPressed = (u, d, True, r) }
    where
        (u, d, l, r) = keysPressed game

handleKeys (EventKey (Char 'd') Down _ _) game =
    game { keysPressed = (u, d, l, True) }
    where
        (u, d, l, r) = keysPressed game

handleKeys (EventKey (Char 'w') Up _ _) game =
    game { keysPressed = (False, d, l, r) }
    where
        (u, d, l, r) = keysPressed game

handleKeys (EventKey (Char 's') Up _ _) game =
    game { keysPressed = (u, False, l, r) }
    where
        (u, d, l, r) = keysPressed game

handleKeys (EventKey (Char 'a') Up _ _) game =
    game { keysPressed = (u, d, False, r) }
    where
        (u, d, l, r) = keysPressed game

handleKeys (EventKey (Char 'd') Up _ _) game =
    game { keysPressed = (u, d, l, False) }
    where
        (u, d, l, r) = keysPressed game

handleKeys (EventKey (SpecialKey KeySpace) Down _ _) game =
    game { spawnHeld = True }

handleKeys (EventKey (SpecialKey KeySpace) Up _ _) game =
    game { spawnHeld = False }


handleKeys _ game = game

main :: IO ()
main = play window background fps initialState render handleKeys update

update :: Float -> KaelinGame -> KaelinGame
update _ game | loseScreen game > 0 = game
update seconds game =
    let moved = (selfCollision . enemyCollision . moveEnemyProjectile seconds . moveProjectile seconds . moveEnemy seconds . moveKaelin seconds . readInput) game
        timer = spawnTimer moved - seconds
        enemyTimer = enemySpawnTimer moved - seconds
        fireTimer = enemyFireTimer moved - seconds
        cooldown = 0.5
        afterEnemySpawn = if enemyTimer <= 0
                          then moved { enemies = (0, 200) : enemies moved
                                     , enemySpawnTimer = enemySpawnCooldown moved }
                          else moved { enemySpawnTimer = max 0 enemyTimer }
        afterEnemyFire = if fireTimer <= 0 && not (null (enemies afterEnemySpawn))
                         then afterEnemySpawn { enemyProjectiles = (map (\(x,y) -> (x, y-10)) (enemies afterEnemySpawn)) ++ enemyProjectiles afterEnemySpawn
                                              , enemyFireTimer = enemyFireCooldown afterEnemySpawn }
                         else afterEnemySpawn { enemyFireTimer = max 0 fireTimer }
    in if spawnHeld afterEnemyFire && timer <= 0
       then afterEnemyFire { friendlyProjectiles = kaelinLoc afterEnemyFire : friendlyProjectiles afterEnemyFire
                           , spawnTimer = cooldown }
       else afterEnemyFire { spawnTimer = max 0 timer }