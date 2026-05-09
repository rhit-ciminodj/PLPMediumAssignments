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
        shieldHeld :: Bool,
        shieldTimer :: Float,
        shieldActive :: Bool,
        bombHeld :: Bool,
        bombTimer :: Float,
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
        shieldHeld = False,
        shieldTimer = 0,
        shieldActive = False,
        bombHeld = False,
        bombTimer = 0,
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

        inBoundsEntity e = let (x, y) = location e in y >= (-fromIntegral height / 2 - 50) && y <= (fromIntegral height / 2 + 50) && x >= (-fromIntegral width / 2 - 50) && x <= (fromIntegral height / 2 + 50)
        filteredList = filter inBoundsEntity entityList

        gameWithEntities = game { entities = filteredList }

        gameAfterWorld = foldl (\g e -> updateWorld e g seconds e) gameWithEntities filteredList

type Movement = KaelinGame -> Float -> Entity -> Entity

-- Help entity clock for angles

tick :: Float -> Entity -> Entity
tick seconds entity = entity { entityTimer = entityTimer entity + seconds }

-- Composable Movements

-- Linear, constant velocity
linear :: Float -> Float -> Movement
linear vx vy _ seconds entity =
    let (x, y) = location entity
        x' = x + vx * seconds
        y' = y + vy * seconds
    in tick seconds $ entity { location = (x', y') }

-- Sine wavy motion with horizontal and vertical drift
sineX :: Float -> Float -> Float -> Float -> Movement
sineX amplitude frequency vx vy _ seconds entity =
    let (x, y) = location entity
        x' = x + vx * seconds
        yBase = y + vy * seconds
        y' = yBase + amplitude * sin (entityTimer entity * frequency)
    in tick seconds $ entity { location = (x', y') }

-- Circular motion around a center
circular :: (Float, Float) -> Float -> Movement
circular (centerX, centerY) speed _ seconds entity = tick seconds $ entity { location = (x', y') }
    where
        (x, y) = location entity
        oldAngle = atan2 (y - centerY) (x - centerX)
        angle = oldAngle + seconds * speed
        radius = sqrt (((x - centerX) ** 2) + ((y - centerY) ** 2))
        x' = centerX + radius * (cos angle)
        y' = centerY + radius * (sin angle)

-- Shield orbits around player dynamically
updateShield :: KaelinGame -> Float -> Entity -> Entity
updateShield game seconds self =
    let (px, py) = kaelinLoc game
        t = entityTimer self
        angle = t * 10
        x' = px + 30 * cos angle
        y' = py + 30 * sin angle
    in tick seconds $ self { location = (x', y') }

-- Track player
tracking :: Float -> Movement
tracking speed game seconds entity =
    let (ex, ey) = location entity
        (px, py) = kaelinLoc game
        dx = px - ex
        dy = py - ey
        distance = max 1 (sqrt (dx*dx + dy*dy))
        angleX = dx / distance
        angleY = dy / distance
        x' = ex + angleX * speed * seconds
        y' = ey + angleY * speed * seconds
    in tick seconds $ entity { location = (x', y') }



-- Combine Movements
combo :: Float -> Movement -> Movement -> Movement
combo time m1 m2 game seconds entity = 
    if entityTimer entity < time
        then m1 game seconds entity
        else m2 game seconds entity

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

handleKeys (EventKey (Char 'g') Down _ _) game =
    game { shieldHeld = True }

handleKeys (EventKey (Char 'g') Up _ _) game =
    game { shieldHeld = False }

handleKeys (EventKey (Char 'h') Down _ _) game =
    game { bombHeld = True }

handleKeys (EventKey (Char 'h') Up _ _) game =
    game { bombHeld = False }

handleKeys _ game = game

projectileMovement :: Bool -> Movement
projectileMovement isFriendly = linear 0 direction
    where
        direction = if isFriendly then 220 else -220

enemyUpdateWorld :: KaelinGame -> Float -> Entity -> KaelinGame
enemyUpdateWorld game seconds self =
    if entityTimer self >= 1.5
    then let resetEnemy e = if location e == location self then e { entityTimer = 0 } else e
             resetEntities = map resetEnemy (entities game)
             newProjectile = Entity { location = location self, updateSelf = projectileMovement False, updateWorld = \g _ _ -> g, pic = color red $ circleSolid 4, entityTimer = 0, radius = 3, friendly = False }
         in game { entities = newProjectile : resetEntities }
    else game

normalEnemy :: Pos -> Entity
normalEnemy pos = Entity
    { location = pos,
     updateSelf = updateEnemy,
      updateWorld = enemyUpdateWorld,
      pic = color orange $ circleSolid 10,
      entityTimer = 0,
      radius = 10,
      friendly = False
    }

updateEnemy :: KaelinGame -> Float -> Entity -> Entity
updateEnemy game seconds self = self { location = (x, y), entityTimer = newTimer }
    where
        (oldX, oldY) = location self
        (x, y) = (oldX + 1, (400) + 60 * (sin (oldX / 100)))
        newTimer = entityTimer self + seconds

trackerEnemy :: Pos -> Entity
trackerEnemy pos = Entity
        { location = pos,
            updateSelf = combo 2.0 (linear 0 (-45)) (combo 5.0 (tracking 220) (linear 0  (-300))),
            updateWorld = \g _ _ -> g,
            pic = color magenta $ circleSolid 8,
            entityTimer = 0,
            radius = 8,
            friendly = False
        }

circularEnemy :: Float -> Float -> Entity
circularEnemy x speed = Entity
    {   location = (x, 500),
        updateSelf = combo 2.0 (linear 0 (-45)) (combo 10.0 (circular (x, 275) speed) (combo 13.5 (tracking 300) (linear 0 300))),
        updateWorld = \g _ _ -> g,
        pic = color green $ circleSolid 10,
        entityTimer = 0,
        radius = 10,
        friendly = False
    }

spawnProjectileFrom :: Entity -> Entity
spawnProjectileFrom source = Entity
    { location = location source, 
      updateSelf = projectileMovement (friendly source),
      updateWorld = \g _ _ -> g,
      pic = if friendly source then color green $ circleSolid 4 else color red $ circleSolid 4,
      entityTimer = 0,
      radius = 3,
      friendly = friendly source
    }

spawnProjectileAt :: Pos -> Bool -> Entity
spawnProjectileAt pos isFriendly = Entity
    { location = pos,
      updateSelf = projectileMovement isFriendly,
      updateWorld = \g _ _ -> g,
      pic = if isFriendly then color yellow $ circleSolid 4 else color red $ circleSolid 4,
      entityTimer = 0,
      radius = 3,
      friendly = isFriendly
    }

bombMovement :: KaelinGame -> Float -> Entity -> Entity
bombMovement game seconds self = self { location = (x', y'), radius = newRadius, pic = newPic, entityTimer = newTimer }
    where
        (x, y) = location self
        timer = entityTimer self
        x' = x
        y' = y + 5
        entityList = entities game
        shouldUpdateRadius = any (\e -> let (ex, ey) = location e in (not (friendly e)) && ((radius e) + 30) >= (sqrt (((x - ex) ** 2) + ((y - ey) ** 2)))) entityList
        newPic = if timer >= 1.0 then color rose $ circleSolid 200 else color rose $ circleSolid 3
        newRadius = if timer >= 1.1 then 200 else 0
        newTimer = if timer >= 1.0 then timer + seconds else if shouldUpdateRadius then 1.0 else 0.0
        

bomb :: Pos -> Entity
bomb pos = Entity
    {   location = pos,
        updateSelf = bombMovement,
        updateWorld = \g _ _ -> g,
        pic = color rose $ circleSolid 2,
        entityTimer = 0,
        radius = 0,
        friendly = True
    }

update :: Float -> KaelinGame -> KaelinGame
update seconds game
    | loseScreen game > 0 = game
    | otherwise =
        let cooldown = 0.5
            enemySpawnCooldown = 2.5

            applyMovement = updateEntities seconds . moveKaelin seconds . readInput

            decrementTimers s g = g { spawnTimer = spawnTimer g - s, shieldTimer = shieldTimer g - s, enemySpawnTimer = enemySpawnTimer g - s, bombTimer = bombTimer g - s }

            spawnEnemy g =
                if enemySpawnTimer g <= 0
                    then g { entities = normalEnemy (-520, 300) : trackerEnemy (0, 500) : (circularEnemy 250 2) : (circularEnemy (-250) (-2)) : entities g, enemySpawnTimer = enemySpawnCooldown }
                else g { enemySpawnTimer = max 0 (enemySpawnTimer g) }

            resolveEnemyHitsIfNeeded g = if checkEnemyCollision g then resolveEnemyHits g else g

            setLoseIfCollided g = if checkPlayerCollision g then g { loseScreen = 1 } else g

            spawnPlayerProjectileIfNeeded g =
                if spawnHeld g && spawnTimer g <= 0
                then g { entities = spawnProjectileAt (kaelinLoc g) True : entities g, spawnTimer = cooldown }
                else g { spawnTimer = max 0 (spawnTimer g) }

            bombCooldown = 3.0
            spawnBombIfNeeded g =
                if bombHeld g && bombTimer g <= 0
                then g { entities = bomb (kaelinLoc g) : entities g, bombTimer = bombCooldown }
                else g { bombTimer = max 0 (bombTimer g) }

            shieldCooldown = 2.0

            spawnShieldIfNeeded g =
                if shieldHeld g && shieldTimer g <= 0 && not (shieldActive g)
                then let (px, py) = kaelinLoc g in g { entities = Entity { location = (px, py - 30), updateSelf = updateShield, updateWorld = \x _ _ -> x, pic = color cyan $ circleSolid 8, entityTimer = 0, radius = 8, friendly = True } : entities g, shieldTimer = shieldCooldown, shieldActive = True }
                else g { shieldTimer = max 0 (shieldTimer g) }
            
            updateShieldActive g =
                let shieldExists = any (\e -> friendly e && radius e == 8) (entities g)
                in g { shieldActive = shieldExists }

        in (spawnBombIfNeeded . updateShieldActive . spawnShieldIfNeeded . spawnPlayerProjectileIfNeeded . setLoseIfCollided . resolveEnemyHitsIfNeeded . spawnEnemy . decrementTimers seconds . applyMovement) game

main :: IO ()
main = play window background fps initialState render handleKeys update