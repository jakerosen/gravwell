{-# language RecordWildCards #-}
module Game where

import Control.Algebra
import Control.Carrier.State.Strict
import Control.Effect.Lens (assign, modifying, use)
import Control.Effect.Writer
import Control.Lens hiding (assign, modifying, use)
import Control.Monad.Random
import Data.Foldable (for_)
import Data.Generics.Labels ()
import Data.List
import Data.Set (Set)
import qualified Data.Set as Set
import GHC.Generics (Generic)
import Text.Printf

import Card
import Player
import RandomEffect
import Zoomy

data Game = Game
  { gamePlayer1 :: Player
  , gamePlayer2 :: Player
  , gamePlayer3 :: Player
  , gamePlayer4 :: Player
  , gameDerelict1 :: Int
  , gameDerelict2 :: Int
  , gameRound :: Int
  , gameState :: GameState
  , gameUnplayedCards :: [(PlayerNum, Card)]
  , gameUndraftedCards :: [(Card, Card)]
  , gameDraftOrder :: [PlayerNum]
  } deriving stock (Show, Generic)

-- The current game state to be parsed by the UI
data GameState
  = DraftBegan (forall sig m.
      (Has RandomEffect sig m) => m Game)

  | DraftPickPlayer (forall sig m.
      (Has (Writer [String]) sig m) => Int -> Maybe (m Game))

  | DraftPickAI (forall sig m.
      (Has RandomEffect sig m, Has (Writer [String]) sig m)
    => m Game)

  | RoundBegan Game

  | PickCard (forall sig m.
      (Has RandomEffect sig m) => Int -> Maybe (m Game))

  | ResolvingMovement (forall sig m.
      (Has (Writer [String]) sig m) => m Game)

  | RoundEnded Game

instance Show GameState where
  show = \case
    DraftBegan{} -> "DraftBegan"
    DraftPickPlayer{} -> "DraftPickPlayer"
    DraftPickAI{} -> "DraftPickAI"
    RoundBegan{} -> "RoundBegan"
    PickCard{} -> "PickCard"
    ResolvingMovement{} -> "ResolvingMovement"
    RoundEnded{} -> "RoundEnded"

data PlayerNum
  = Player1
  | Player2
  | Player3
  | Player4
  deriving stock (Bounded, Enum, Eq, Show)

data ShipNum
  = Ship1
  | Ship2
  | Ship3
  | Ship4
  | Derelict1
  | Derelict2
  deriving stock (Bounded, Enum, Eq, Show)

-- The initial game
initialGame :: Game
initialGame =
  Game
    { gamePlayer1 = Player 0 []
    , gamePlayer2 = Player 0 []
    , gamePlayer3 = Player 0 []
    , gamePlayer4 = Player 0 []
    , gameDerelict1 = 26
    , gameDerelict2 = 36
    , gameRound = 1
    , gameState = error "Initial state not set"
    , gameUnplayedCards = []
    , gameUndraftedCards = []
    , gameDraftOrder = []
    } & setStateDraftBegan

-- The current number of players supported
numPlayers :: Int
numPlayers = 4

-- | Returns 1 (forwards), -1 (backwards), or 0 (nowhere) per the natural motion
-- direction of the given index in the game.
gameMotion :: Game -> Int -> Int
gameMotion game i =
  case comp1 <> comp2 of
    LT -> -1
    EQ -> 0
    GT -> 1
  where
    comp1 = compare closestShipBehind closestShipAhead
    comp2 = compare numShipsAhead numShipsBehind

    (shipsBehind, shipsAhead) = Set.split i ships

    closestShipBehind = case (Set.lookupMax shipsBehind) of
      Nothing -> maxBound
      Just n -> i - n

    closestShipAhead = case (Set.lookupMin shipsAhead) of
      Nothing -> maxBound
      Just n ->  n - i

    numShipsBehind = length shipsBehind
    numShipsAhead = length shipsAhead

    ships :: Set Int
    ships = gameShips game

-- The list of game players
gamePlayers :: Game -> [Player]
gamePlayers game =
  [ gamePlayer1 game
  , gamePlayer2 game
  , gamePlayer3 game
  , gamePlayer4 game
  ]

-- gets the player lens from the player number
playerNumToPlayer :: PlayerNum -> Lens' Game Player
playerNumToPlayer = \case
  Player1 -> #gamePlayer1
  Player2 -> #gamePlayer2
  Player3 -> #gamePlayer3
  Player4 -> #gamePlayer4

-- gets the ship lens from the ship number
gameShipByNum :: ShipNum -> Lens' Game Int
gameShipByNum = \case
  Ship1 -> #gamePlayer1 . #playerShip
  Ship2 -> #gamePlayer2 . #playerShip
  Ship3 -> #gamePlayer3 . #playerShip
  Ship4 -> #gamePlayer4 . #playerShip
  Derelict1 -> #gameDerelict1
  Derelict2 -> #gameDerelict2

-- gets the ship number from the player number
playerNumToShipNum :: PlayerNum -> ShipNum
playerNumToShipNum = \case
  Player1 -> Ship1
  Player2 -> Ship2
  Player3 -> Ship3
  Player4 -> Ship4

-- The condition for ending the game. Either:
-- A ship is in the warp gate or
-- 6 Rounds are completed
gameOver :: Game -> Bool
gameOver game =
     game ^. #gameRound >= 7
  || any (>= gameWarpGate) (gameShipsList game)

-- The position of the warp gate (the goal to reach)
gameWarpGate :: Int
gameWarpGate = 54

-- | Positions of all ships in the game as a Set
gameShips :: Game -> Set Int
gameShips = Set.fromList . gameShipsList

-- | Positions of all ships in the game as a List
gameShipsList :: Game -> [Int]
gameShipsList game =
  [ gameDerelict1 game
  , gameDerelict2 game
  ] ++ map (view #playerShip) (gamePlayers game)

-- Sets the game state of this Game to PickCard
setStatePickCard :: Game -> Game
setStatePickCard game0 = game1
  where
    -- f :: Int -> Maybe Game
    -- f x = if x >= length (game1 ^. #gamePlayer1 . #playerHand)
    --   then Nothing
    --   else Just $ handlePlayCard x game1

    f :: (Has RandomEffect sig m) => Int -> Maybe (m Game)
    f x = if x >= length (game1 ^. #gamePlayer1 . #playerHand)
      then Nothing
      else Just (handlePickCards x & execState game1)

    game1 :: Game
    game1 = game0 { gameState = PickCard f }

-- Sets the game state of this Game to ResolvingMovement
setStateResolvingMovement :: Game -> Game
setStateResolvingMovement game0 = game1
  where
    game1 :: Game
    game1 = game0 { gameState =
      ResolvingMovement (handleResolveCard & execState game1) }

-- Sets the game state of this Game to DraftBegan
setStateDraftBegan :: Game -> Game
setStateDraftBegan game0 = game1
  where
    game1 :: Game
    game1 = game0
      { gameState = DraftBegan (handleDraftBegan & execState game1) }

-- Sets the game state of this Game to DraftPickPlayer
setStateDraftPickPlayer :: Game -> Game
setStateDraftPickPlayer game0 = game1
  where
    f :: (Has (Writer [String]) sig m) => Int -> Maybe (m Game)
    f x = if x >= length (game1 ^. #gameUndraftedCards)
      then Nothing
      else Just (handleDraftPickPlayer x & execState game1)

    game1 :: Game
    game1 = game0
      { gameState = DraftPickPlayer f }

-- Sets the game state of this Game to DraftPickAI
setStateDraftPickAI :: Game -> Game
setStateDraftPickAI game0 = game1
  where
    game1 :: Game
    game1 = game0
      { gameState = DraftPickAI (handleDraftPickAI & execState game1) }

-- Sets the game state of this Game to RoundBegan
setStateRoundBegan :: Game -> Game
setStateRoundBegan game0 = game1
  where
    game1 :: Game
    game1 = game0
      { gameState = RoundBegan game2 }

    game2 :: Game
    game2 = setStatePickCard game1

-- Sets the game state of this Game to RoundEnded
setStateRoundEnded :: Game -> Game
setStateRoundEnded game0 = game1
  where
    game1 :: Game
    game1 = game0 { gameState = RoundEnded game2 }

    game2 :: Game
    game2 = setStateDraftBegan game1

-- Handles resolving a played card.
handleResolveCard
  :: (Has (State Game) sig m, Has (Writer [String]) sig m)
  => m ()
handleResolveCard = do
  ~((playerNum, card):cards) <- use @Game #gameUnplayedCards
  assign @Game #gameUnplayedCards cards
  playCard card playerNum

  hand <- use @Game (#gamePlayer1 . #playerHand)

  modify
    if null cards
    then
      if null hand
      then \game -> game
        & #gameRound %~ (+1)
        & setStateRoundEnded
      else setStatePickCard
    else setStateResolvingMovement

-- Handles the draft.
-- Currently just assigns 6 random cards to each player.
handleDraftBegan
  :: (Has (State Game) sig m, Has RandomEffect sig m) => m ()
handleDraftBegan = do
  deck' <- shuffleCards deck
  game0 <- get @Game
  let
    numPiles = numPlayers * 3
    usedCards = take (numPiles * 2) deck'

    -- (revealed, hidden)
    piles :: [(Card, Card)]
    piles = uncurry zip (splitAt numPiles usedCards)

    order :: [PlayerNum]
    order = sortOn
      (\playerNum -> game0 ^. playerNumToPlayer playerNum . #playerShip)
      [minBound .. maxBound]

    orderOfDraft :: [PlayerNum]
    orderOfDraft = concat $ replicate 3 order

    p1 :: PlayerNum
    p1 = head orderOfDraft

  modify @Game (\game -> game
    & #gameUndraftedCards .~ piles
    & #gameDraftOrder .~ orderOfDraft
    & case p1 of
      Player1 -> setStateDraftPickPlayer
      _ -> setStateDraftPickAI
    )

-- Handles the draft.
handleDraftPickPlayer
  :: forall sig m.
    (Has (State Game) sig m, Has (Writer [String]) sig m)
  => Int
  -> m ()
handleDraftPickPlayer x = do
  pickedCards :: (Card, Card) <- zoomy @Game #gameUndraftedCards (pluck' x)
  modifying @Game (#gamePlayer1 . #playerHand) (pickedCards ^.. both ++)
  modifying @Game #gameDraftOrder tail
  tell ["Player1 picks " ++ ppCard (fst pickedCards)]

  toPick <- use @Game #gameUndraftedCards
  modify
    if null toPick
    then setStateRoundBegan
    else \game ->
      let p = head $ gameDraftOrder game
      in game
        & case p of
          Player1 -> setStateDraftPickPlayer
          _ -> setStateDraftPickAI

-- Handles the draft.
handleDraftPickAI
  :: forall sig m.
    ( Has (State Game) sig m
    , Has (Writer [String]) sig m
    , Has RandomEffect sig m
    )
  => m ()
handleDraftPickAI = do
  ~(playerNum:order) <- use @Game #gameDraftOrder
  aiDraftCards playerNum

  toPick <- use @Game #gameUndraftedCards
  modify
    if null toPick
    then setStateRoundBegan
    else \game ->
      let p = head order
      in game
        & #gameDraftOrder .~ order
        & case p of
          Player1 -> setStateDraftPickPlayer
          _ -> setStateDraftPickAI

-- Drafts cards for an AI player
-- Currently picks randomly
aiDraftCards
  :: ( Has RandomEffect sig m
     , Has (Writer [String]) sig m
     , Has (State Game) sig m
     )
  => PlayerNum
  -> m ()
aiDraftCards playerNum = do
  let
    player :: Lens' Game Player
    player = playerNumToPlayer playerNum
  len :: Int <- use @Game (#gameUndraftedCards . to length)
  i <- randomInt 0 (len - 1)
  pickedCards :: (Card, Card) <- zoomy @Game #gameUndraftedCards (pluck' i)
  modifying @Game (player . #playerHand) (pickedCards ^.. both ++)
  tell [show playerNum ++ " picks " ++ ppCard (fst pickedCards)]

-- Pick card for an AI player
-- Currently picks randomly
aiPickCard
  :: (Has RandomEffect sig m, Has (State Player) sig m)
  => m Card
aiPickCard = do
  player :: Player <- get
  let range = length (player ^. #playerHand) - 1
  i <- randomInt 0 range
  card <- zoomy @Player #playerHand (pluck' i)
  pure card

-- handle the picks for all of the players, with the given index chosen by
-- the player. This index must be valid.
handlePickCards :: forall sig m.
  (Has (State Game) sig m, Has RandomEffect sig m) => Int -> m ()
handlePickCards x = do
  let
    aiPicks :: m ()
    aiPicks =
      for_ [Player2 .. maxBound] \playerNum -> do
        let
          player :: Lens' Game Player
          player = cloneLens $ playerNumToPlayer playerNum
        card <- zoomy player aiPickCard
        modifying @Game #gameUnplayedCards  ((playerNum, card) :)

    pick :: m ()
    pick = do
      card <- zoomy @Game (#gamePlayer1 . #playerHand) (pluck' x)
      modifying @Game #gameUnplayedCards  ((Player1, card) :)

    orderPicks :: [(a, Card)] -> [(a, Card)]
    orderPicks = sortOn (view (_2 . #cardSymbol))

  aiPicks
  pick
  modifying @Game #gameUnplayedCards orderPicks
  modify setStateResolvingMovement

l :: Lens' a b -> Lens' a b
l = id

-- Play this Card and determine the resulting Game
playCard
  :: (Has (State Game) sig m, Has (Writer [String]) sig m)
  => Card
  -> PlayerNum
  -> m ()
playCard card playerNum = do
  let
    moveAmount = cardAmount card

    output :: [String]
    output = [printf "%s plays %s" (show playerNum) (ppCard card)]
  tell output

  case cardType card of
    Fuel -> do
      motion <- gets gameMotion
      moveShip' moveAmount motion (playerNumToShipNum playerNum)
    Repulsor -> do
      motion <- gets gameMotion
      moveShip' moveAmount (negate . motion) (playerNumToShipNum playerNum)
    Tractor -> do
      game <- get
      let
        p1 = game ^. playerNumToPlayer playerNum . #playerShip
        otherShips =
          sortOn
            (\otherShip ->
              abs (p1 - game ^. gameShipByNum otherShip))
            (delete (playerNumToShipNum playerNum) [minBound .. maxBound])
      for_ otherShips \otherShip ->
        let
          f :: Int -> Int
          f p2 = signum (p1 - p2)

        in moveShip' moveAmount f otherShip

-- Move the ship at this position
moveShip
  :: Set Int -- occupied spaces
  -> (Int -> Int) -- motion
  -> Int -- fuel amount
  -> Int -- starting pos
  -> Int
moveShip ships f fuel start =
    let
      motion :: Int
      motion =  f start

      move :: Int
      move = motion * fuel

      openPos :: Int -> Int
      openPos pos = if pos `Set.member` ships
        then openPos (pos + motion)
        else pos

      candidatePos :: Int
      candidatePos = start + move

      newPosition :: Int
      newPosition = max 0 $ if motion == 0
        then candidatePos
        else openPos candidatePos

    in newPosition

-- monadic version of moveship
moveShip'
  :: (Has (State Game) sig m, Has (Writer [String]) sig m)
  => Int -- fuel amount
  -> (Int -> Int) -- motion
  -> ShipNum -- The moving ship
  -> m ()
moveShip' fuel f shipNum = do
  game <- get @Game
  let
    ship :: Lens' Game Int
    ship = gameShipByNum shipNum

    pos1 = game ^. ship
    pos2 = moveShip (gameShips game) f fuel pos1

  assign ship pos2

  let
    output :: [String]
    output = [printf "%s moves from %d to %d" (show shipNum) pos1 pos2]

  tell output

-- validates the game to ensure it does not have an illegal state
validateGame :: Game -> IO ()
validateGame game@(Game { .. })  = do
  when
    (has
      ( folded
      . #playerHand
      . filtered ((>6)
      . length))
      ( gamePlayers game ))
    (die "Hand exceeded 6 cards")

  when (any (<0) (gameShips game)) (die "Ship index cannot be negative")

  when
    (length (filter (>=gameWarpGate) (gameShipsList game)) > 1)
    (die "Two ships cannot be in the warp gate at once")

  do
    let
      xs :: [Int]
      xs = filter (/=0) (gameShipsList game)

    when (length xs /= length (nub xs))
      (die "Ships cannot be on top of each other outside of singularity")

  where
    die :: [Char] -> IO ()
    die s = do
      print game
      fail s
