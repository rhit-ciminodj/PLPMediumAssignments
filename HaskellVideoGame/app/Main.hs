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
        entityTimer :: Float,
        radius :: Float,
        friendly :: Bool

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
updateEntities seconds game = gameAfterWorld
    where
        entityList = map (updateEntity game seconds) (entities game)

        inBoundsEntity e = let (_, y) = location e in y >= (-fromIntegral height / 2 - 50) && y <= (fromIntegral height / 2 + 50)
        filteredList = filter inBoundsEntity entityList

        gameWithEntities = game { entities = filteredList }

        gameAfterWorld = foldl (\g e -> updateWorld e g seconds e) gameWithEntities filteredList

type Pos = (Float, Float)

collides :: Pos -> Float -> Pos -> Float -> Bool
collides (x1, y1) r1 (x2, y2) r2 =
    let dx = x1 - x2
        dy = y1 - y2
        rs = r1 + r2
    in dx*dx + dy*dy <= rs*rs

playerRadius :: Float
playerRadius = 10

checkPlayerCollision :: KaelinGame -> Bool
checkPlayerCollision game = 
    any (\e -> not (friendly e) && collides (location e) (radius e) (kaelinLoc game) playerRadius) (entities game)

checkEnemyCollision :: KaelinGame -> Bool
checkEnemyCollision game =
    any hitEnemy (filter friendly (entities game))
    where
        enemyTargets = filter (not . friendly) (entities game)

        hitEnemy bullet = any (collidesEntity bullet) enemyTargets

        collidesEntity bullet enemy =
            collides (location bullet) (radius bullet) (location enemy) (radius enemy)

resolveEnemyHits :: KaelinGame -> KaelinGame
resolveEnemyHits game = game { entities = survivingEntities }
    where
        bullets = filter friendly (entities game)
        enemies = filter (not . friendly) (entities game)

        collidesEntity a b = collides (location a) (radius a) (location b) (radius b)

        hitFriendly e = friendly e && any (collidesEntity e) enemies
        hitEnemy e = not (friendly e) && any (collidesEntity e) bullets

        survivingEntities = filter (\e -> not (hitFriendly e || hitEnemy e)) (entities game)

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

updateEnemy :: KaelinGame -> Float -> Entity -> Entity
updateEnemy game seconds self = self { location = (x, y), entityTimer = newTimer }
    where
        (oldX, oldY) = location self
        (x, y) = (oldX + 1, (400) + 60 * (sin (oldX / 100)))
        newTimer = entityTimer self + seconds
        
updateProjectile :: KaelinGame -> Float -> Entity -> Entity
updateProjectile _ seconds self = self { location = (x, y'), entityTimer = entityTimer self + seconds }
    where
        (x, y) = location self
        speed = 300
        direction = if friendly self then 1 else -1
        y' = y + direction * speed * seconds

spawnProjectileFrom :: Entity -> Entity
spawnProjectileFrom source = Entity
    { location = location source, 
    updateSelf = updateProjectile,
    updateWorld = \g _ _ -> g,
    pic = if friendly source then color green $ circleSolid 4 else color red $ circleSolid 4,
    entityTimer = 0,
    radius = 3,
    friendly = friendly source
    }

spawnEnemyProjectile :: Entity -> Entity
spawnEnemyProjectile source = spawnProjectileFrom source { friendly = False }

spawnFriendlyProjectile :: Entity -> Entity
spawnFriendlyProjectile source = spawnProjectileFrom source { friendly = True }

spawnProjectileAt :: Pos -> Bool -> Entity
spawnProjectileAt pos isFriendly = Entity
    { location = pos,
    updateSelf = updateProjectile,
    updateWorld = \g _ _ -> g,
    pic = if isFriendly then color yellow $ circleSolid 4 else color red $ circleSolid 4,
    entityTimer = 0,
    radius = 3,
    friendly = isFriendly
    }


enemyUpdateWorld :: KaelinGame -> Float -> Entity -> KaelinGame
enemyUpdateWorld game seconds self =
    if entityTimer self >= shootInterval
    then let resetEnemy e = if not (friendly e) && location e == location self && radius e == radius self
                            then e { entityTimer = 0 }
                            else e
             resetEntities = map resetEnemy (entities game)
         in game { entities = spawnEnemyProjectile self : resetEntities }
    else game

enemyProjectileUpdate :: KaelinGame -> Float -> Entity -> Entity
enemyProjectileUpdate _ seconds self = self { location = (x, y - speed * seconds), entityTimer = entityTimer self + seconds }
    where
        (x, y) = location self
        speed = 300

shootInterval :: Float
shootInterval = 1.5

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
            then updated { entities = Entity { location = ((-550), 0), updateSelf = updateEnemy, updateWorld = enemyUpdateWorld, pic = color orange $ circleSolid 10, entityTimer = 0.0, radius = 10, friendly = False } : entities moved, enemySpawnTimer = enemySpawnCooldown }  

            else updated { enemySpawnTimer = max 0 enemyTimer }
        afterEnemyHits = if checkEnemyCollision afterEnemySpawn
            then resolveEnemyHits afterEnemySpawn
                
            else afterEnemySpawn
        afterCollision = if checkPlayerCollision afterEnemyHits
            then afterEnemyHits { loseScreen = 1 }

            else afterEnemyHits
        in if spawnHeld afterCollision && timer <= 0
            then afterCollision { entities = spawnProjectileAt (kaelinLoc afterCollision) True : entities afterCollision, spawnTimer = cooldown }

            else afterCollision { spawnTimer = max 0 timer }

main :: IO ()
main = play window background fps initialState render handleKeys update