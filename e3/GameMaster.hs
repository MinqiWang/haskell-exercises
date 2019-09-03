module GameMaster where

import Control.Monad (ap, liftM)

import GameMasterDef

-- Question 1.

-- The game master for the guessing game.  The parameter is the secret number
-- the player has to guess.
guessingGame :: MonadGameMaster m => Integer -> m Ending
guessingGame secret
    | secret < 1 || secret > 100 = error "invalid game"
    | otherwise = doGame 1 100
        where doGame lo hi = do
                req1 <- gmAction lo hi
                case req1 of
                    Surrender -> return (Lose secret)
                    Guess i
                        | i < lo || i > hi -> doGame lo hi -- Go back to guess with given the same range (lo, hi)
                        | i == secret -> return (Win) -- The player wins because of the correct guess
                        | i < secret -> doGame (i+1) hi -- Go back to guess with given the range (i+1, hi)
                        | otherwise -> doGame lo (i-1) -- Go back to guess with given the range (lo, i-1)


-- Question 2.

instance Functor FreeGameMaster where
    -- fmap :: (a -> b) -> FreeGameMaster a -> FreeGameMaster b
    -- If you are confident with your Monad instance, you can just write
    -- fmap = liftM
    fmap = liftM

instance Applicative FreeGameMaster where
    -- pure :: a -> FreeGameMaster a
    -- If you are confident with your Monad instance, you can just write
    -- pure = return
    pure = return

    -- (<*>) :: FreeGameMaster (a -> b) -> FreeGameMaster a -> FreeGameMaster b
    -- If you are confident with your Monad instance, you can just write
    -- (<*>) = ap
    (<*>) = ap

instance Monad FreeGameMaster where
    -- return :: a -> FreeGameMaster a
    return a = Pure a

    -- (>>=) :: FreeGameMaster a -> (a -> FreeGameMaster b) -> (FreeGameMaster b)
    (Pure a) >>= f = f a
    GMAction lo hi next >>= f = GMAction lo hi next_new
        where next_new = (\playerMsg -> ((next playerMsg) >>= f))

instance MonadGameMaster FreeGameMaster where
    -- gmAction :: Integer -> Integer -> FreeGameMaster PlayerMsg
    gmAction lo hi = GMAction lo hi (\playerMsg -> return playerMsg)

-- Question 3.

testGame :: (Integer -> FreeGameMaster Ending) -> Bool
testGame = \game -> case (game 42) of
                        Pure result -> result == Win || result == (Lose 42)
                        GMAction lo hi next ->
                            (case (next Surrender) of
                                Pure testLose -> testLose == Lose 42
                                GMAction _ _ _ -> False)
                            &&
                            (case (next (Guess 20)) of
                                Pure _ -> False
                                GMAction lo1 hi1 next1 -> lo1 == 21 && hi1 == 100
                                    &&
                                    (case (next1 (Guess 60)) of
                                        Pure _ -> False
                                        GMAction lo2 hi2 next2 -> lo2 == 21 && hi2 == 59
                                            &&
                                            (case (next2 (Guess 42)) of
                                                Pure testWin -> testWin == Win
                                                GMAction _ _ _ -> False)))

