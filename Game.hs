{-# language RecordWildCards #-}
module Game where

-- import Data.List (sort)

-- import Debug.Trace

import Card
import Control.Lens
import Control.Monad.Random
import Control.Monad.State.Lazy (State, runState)
import qualified Control.Monad.State.Lazy as State
import Data.Foldable (for_, traverse_)
import Data.Generics.Labels ()
-- import Control.Monad
import Data.List
import Data.Map.Lazy (Map)
import qualified Data.Map.Lazy as Map
-- import Data.Maybe
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Traversable
import GHC.Generics (Generic)
import Player
import System.Random (StdGen)
import qualified System.Random as Random
import System.Random.Shuffle (shuffle')


data Game = Game
  { gamePlayer1 :: Player
  , gamePlayer2 :: Player
  , gamePlayer3 :: Player
  , gamePlayer4 :: Player
  , gameDerelict1 :: Int
  , gameDerelict2 :: Int
  , gameState :: GameState
  , gameRandom :: StdGen
  } deriving stock (Show, Generic)

instance Show GameState where
  show = \case
    RoundBegan{} -> "RoundBegan"
    DraftBegan{} -> "DraftBegan"
    RoundEnded{} -> "RoundEnded"

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

gamePlayers :: Game -> [Player]
gamePlayers game =
  [ gamePlayer1 game
  , gamePlayer2 game
  , gamePlayer3 game
  , gamePlayer4 game
  ]

gamePlayerByNum :: Map Int (ALens' Game Player)
gamePlayerByNum = Map.fromList
  [ (1, #gamePlayer1)
  , (2, #gamePlayer2)
  , (3, #gamePlayer3)
  , (4, #gamePlayer4)
  ]

gameOtherShips :: Map Int ([ALens' Game Int])
gameOtherShips = Map.fromList
  [ (1, [ #gamePlayer2 . #playerShip
        , #gamePlayer3 . #playerShip
        , #gamePlayer4 . #playerShip
        , #gameDerelict1, #gameDerelict2])
  , (2, [ #gamePlayer1 . #playerShip
        , #gamePlayer3 . #playerShip
        , #gamePlayer4 . #playerShip
        , #gameDerelict1, #gameDerelict2])
  , (3, [ #gamePlayer1 . #playerShip
        , #gamePlayer2 . #playerShip
        , #gamePlayer4 . #playerShip
        , #gameDerelict1, #gameDerelict2])
  , (4, [ #gamePlayer1 . #playerShip
        , #gamePlayer2 . #playerShip
        , #gamePlayer3 . #playerShip
        , #gameDerelict1, #gameDerelict2])
  ]

gameOver :: Game -> Bool
gameOver game = any (>= gameWarpGate) (gameShipsList game)

gameWarpGate :: Int
gameWarpGate = 30

-- | Positions of all ships in the game
gameShips :: Game -> Set Int
gameShips = Set.fromList . gameShipsList

gameShipsList :: Game -> [Int]
gameShipsList game =
  [ gameDerelict1 game
  , gameDerelict2 game
  ] ++ map (view #playerShip) (gamePlayers game)

data GameState =
    RoundBegan (Int -> Maybe Game)
  | DraftBegan Game
  | RoundEnded Game

-- The initial game state
initialGame :: StdGen -> Game
initialGame ran = setStateDraftBegan
  Game
    { gamePlayer1 = Player 0 []
    , gamePlayer2 = Player 0 []
    , gamePlayer3 = Player 0 []
    , gamePlayer4 = Player 0 []
    , gameDerelict1 = 10
    , gameDerelict2 = 20
    , gameState = undefined -- intentionally undefined, will be set later
    , gameRandom = ran
    }

-- Sets the game state of this Game to RoundBegan
setStateRoundBegan :: Game -> Game
setStateRoundBegan game0 = game1
  where
    f :: Int -> Maybe Game
    f x = if x >= length (game1 ^. #gamePlayer1 . #playerHand)
      then Nothing
      else Just $ handlePlayCard x game1

    game1 :: Game
    game1 = game0 { gameState = RoundBegan f }

-- Sets the game state of this Game to DraftBegan
setStateDraftBegan :: Game -> Game
setStateDraftBegan game0 = game1
  where
    game1 :: Game
    game1 = game0 { gameState = DraftBegan game2 }

    game2 :: Game
    game2 = game1
      & #gamePlayer1 . #playerHand .~ draftHand1
      & #gamePlayer2 . #playerHand .~ draftHand2
      & #gamePlayer3 . #playerHand .~ draftHand3
      & #gamePlayer4 . #playerHand .~ draftHand4
      & #gameRandom .~ ran2
      & setStateRoundBegan

    (ran1, ran2) = Random.split (gameRandom game1)

    deck' :: [Card]
    deck' = shuffle' deck 26 ran1

    (draftHand1, (draftHand2, (draftHand3, (draftHand4, _))))
      =             (splitAt 6 deck') -- hand 1
      & _2 %~       (splitAt 6) -- hand 2
      & _2._2 %~    (splitAt 6) -- hand 3
      & _2._2._2 %~ (splitAt 6) -- hand 4

-- Sets the game state of this Game to RoundEnded
setStateRoundEnded :: Game -> Game
setStateRoundEnded game0 = game1
  where
    game1 :: Game
    game1 = game0 { gameState = RoundEnded game2 }

    game2 :: Game
    game2 = setStateDraftBegan game1

-- Play the Card at this index and determine the resulting game
-- This must be a valid index
handlePlayCard :: Int -> Game -> Game
handlePlayCard x game0 =
  let
    handSize :: Int
    handSize = length (game0 ^. #gamePlayer1 . #playerHand)

    -- m :: Rand StdGen (Card, Player)
    -- t :: []
    -- b :: ((Card, Player), StdGen)
    -- a :: Player (?)
    -- mapM :: (a -> m b) -> t a -> m (t b)
    -- mapM
    --  :: (Player -> Rand StdGen (Card, Player)
    --  -> [Player]
    --  -> Rand StdGen [(Card, Player)]

    aiPickCard :: Player -> Rand StdGen (Card, Player)
    aiPickCard player = do
      let range = length (player ^. #playerHand) - 1
      i <- getRandomR (0, range)
      pure $ pluckPlayer i player

    aiPickCard' :: Lens' Game Player -> State Game Card
    aiPickCard' player = do
      game <- State.get
      let
        ran = game ^. #gameRandom
        rand = aiPickCard (game ^. player)
        ((card, player'), ran') = runRand rand ran
      State.put (game & player .~ player' & #gameRandom .~ ran')
      pure card

    aiPicks :: State Game [(ALens' Game Int, [ALens' Game Int], Card)]
    aiPicks =
      for [2, 3, 4] \playerNum -> do
        let
          player :: Lens' Game Player
          player = cloneLens $ gamePlayerByNum Map.! playerNum

          ship :: Lens' Game Int
          ship = player . #playerShip

          otherShips :: [ALens' Game Int]
          otherShips = gameOtherShips Map.! playerNum

        card <- aiPickCard' player
        pure (ship, otherShips, card)

    pick :: State Game Card
    pick = zoom (#gamePlayer1 . #playerHand) (pluckCard' x)

    picks :: State Game [(ALens' Game Int, [ALens' Game Int], Card)]
    picks = do
      card <- pick
      ais <- aiPicks
      let
        p1 :: Lens' Game Int
        p1 = #gamePlayer1 . #playerShip

        otherShips :: [ALens' Game Int]
        otherShips = gameOtherShips Map.! 1

      pure $ orderPicks ((p1, otherShips, card):ais)

    orderPicks :: [(a, b, Card)] -> [(a, b, Card)]
    orderPicks = sortBy
      (\p1 p2 -> compare
        (p1 ^. _3 . #cardSymbol)
        (p2 ^. _3 . #cardSymbol))

    playCard
      :: Card
      -> Lens' Game Int
      -> [ALens' Game Int]
      -> State Game ()
    playCard card ship otherShips = do
      let
        moveAmount = cardAmount card
      case cardType card of
        Fuel ->
          State.modify \game ->
            moveShip' moveAmount (gameMotion game) ship game
        Repulsor ->
          State.modify \game ->
            moveShip' moveAmount (negate . gameMotion game) ship game
        Tractor -> do
          game <- State.get
          let
            otherShips' = otherShips
          for_ otherShips' \(cloneLens -> otherShip) ->
            let
              p1 = game ^. ship
              f :: Int -> Int
              f p2 = signum (p1 - p2)

            in State.modify (moveShip' moveAmount f otherShip)

    -- m :: State Game
    -- a :: [stuff]
    -- m a :: State Game [stuff]
    -- b :: ()
    -- State Game [stuff] -> State Game ()
    -- f :: [stuff] -> State Game ()
    -- for_ [stuff] -> (stuff -> State Game ()) -> State Game ()
    -- (>>=) :: m a -> (a -> m b) -> m b
    -- (>>=)
    --  :: State Game [stuff]
    --  -> ([stuff] -> State Game ())
    --  -> State Game ()

    gameS :: State Game ()
    gameS = picks >>= traverse_ \((cloneLens -> player), otherShips, card)
      -> playCard card player otherShips

    game1 :: Game
    game1 = snd $ runState gameS game0

  in if x < 0 || x >= handSize then error "Invalid card index" else
    if handSize == 1
      then setStateRoundEnded game1
      else setStateRoundBegan game1

-- Play this Card and determine the resulting Game
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

moveShip'
  :: Int -- fuel amount
  -> (Int -> Int) -- motion
  -> Lens' Game Int -- The ship that is moving
  -> Game -- starting pos
  -> Game
moveShip' fuel f ship game =
  game & ship %~ moveShip (gameShips game) f fuel

validateGame :: Game -> IO ()
validateGame game@(Game { .. })  = do
  when
    (has
      ( folded
      . #playerHand
      . filtered ((>6)
      . length))
      ( gamePlayers game ))
    -- (any (\player -> length (playerHand player) > 6) (gamePlayers game))
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
