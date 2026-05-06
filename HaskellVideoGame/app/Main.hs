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
    {   kaelinLoc :: (Float, Float),
        kaelinVelX :: Float,
        kaelinVelY :: Float,
        keysPressed :: (Bool, Bool, Bool, Bool),
        spawnHeld :: Bool,
        spawnTimer :: Float,
        enemySpawnTimer :: Float,
        entities :: [Entity],
        loseScreen :: Float
    }

data Entity = Entity
    {
        location :: (Float, Float),
        updateSelf :: KaelinGame -> Float -> Entity -> Entity,
        updateWorld :: KaelinGame -> Float -> Entity -> KaelinGame,
        pic :: Picture,
        entityTimer :: Float
    }



render :: KaelinGame -> Picture
render game =
        pictures (overlayPics ++ worldPics)
    where
        kaelin = color white $ uncurry translate (kaelinLoc game) $ circleSolid 10
        pics = map (\t -> uncurry translate (location t) (pic t)) $ entities game
        worldPics = kaelin : pics
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
        enemySpawnTimer = 0,
        entities = [],
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

updateEntity :: KaelinGame -> Float -> Entity -> Entity
updateEntity game seconds entity = (updateSelf entity) game seconds entity

updateEntities :: Float -> KaelinGame -> KaelinGame
updateEntities seconds game = game { entities = entityList }
    where
        entityList = map (updateEntity game seconds) (entities game)
                    
-- moveProjectile :: Float -> KaelinGame ->  KaelinGame
-- moveProjectile seconds game = game { friendlyProjectiles = moveAndFilter seconds moveOne (friendlyProjectiles game) }
--     where
--         moveOne (x, y) = (x, y + projectileVelocity game * seconds)

-- moveEnemyProjectile :: Float -> KaelinGame -> KaelinGame
-- moveEnemyProjectile seconds game = game { enemyProjectiles = moveAndFilter seconds moveOne (enemyProjectiles game) }
--     where
--         moveOne (x, y) = (x, y - projectileVelocity game * seconds)

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

-- enemyCollision :: KaelinGame -> KaelinGame
-- enemyCollision game = game {  = enemySurvivors, friendlyProjectiles = friendlySurvivors }
--     where
--         bullets = friendlyProjectiles game
--         enemyList = (map location (enemies game))
--         bulletRadius = 3
--         enemyRadius = 10
-- 
--         isHit listBy obj = any (\by -> collides by bulletRadius obj enemyRadius) listBy
--         enemySurvivors = filter (not . isHit bullets) enemyList
--         friendlySurvivors = filter (not . isHit enemyList) bullets

-- selfCollision :: KaelinGame -> KaelinGame
-- selfCollision game
--     | hitByEnemyProjectile || hitByEnemyBody = game { loseScreen = 1 }
--     | otherwise = game
--     where
--         playerPos = kaelinLoc game
--         playerRadius = 10
--         bulletRadius = 3
--         enemyRadius = 10
-- 
--         hitByEnemyProjectile = any (\bullet -> collides bullet bulletRadius playerPos playerRadius) (enemyProjectiles game)
--         hitByEnemyBody = any (\enemy -> collides enemy enemyRadius playerPos playerRadius) (map location (enemies game))




    

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

updateEnemy :: KaelinGame -> Float -> Entity -> Entity
updateEnemy game seconds self = self { location = (x, y), entityTimer = newTimer }
    where
        (oldX, oldY) = location self
        (x, y) = (oldX + 1, 60 * (sin (oldX / 100)))
        

enemyUpdateWorld :: KaelinGame -> Float -> Entity -> KaelinGame
enemyUpdateWorld game seconds self = game

update :: Float -> KaelinGame -> KaelinGame
update _ game | loseScreen game > 0 = game
update seconds game =
    let moved = (moveKaelin seconds . readInput) game
        timer = spawnTimer moved - seconds
        enemyTimer = enemySpawnTimer moved - seconds
        cooldown = 0.5
        enemySpawnCooldown = 2.0
        updated = updateEntities seconds moved
        afterEnemySpawn = if enemyTimer <= 0
                          then updated { entities = Entity { location = ((-550), 0), updateSelf = updateEnemy, updateWorld = enemyUpdateWorld, pic = color orange $ circleSolid 10, entityTimer: 0.0  } : entities moved
                                     , enemySpawnTimer = enemySpawnCooldown }
                          else updated { enemySpawnTimer = max 0 enemyTimer }
    in if spawnHeld afterEnemySpawn && timer <= 0
       then afterEnemySpawn { -- friendlyProjectiles = kaelinLoc afterEnemySpawn : friendlyProjectiles afterEnemySpawn
                            spawnTimer = cooldown }
       else afterEnemySpawn { spawnTimer = max 0 timer }