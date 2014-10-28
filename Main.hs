{-# LANGUAGE TemplateHaskell #-}

module Main where

import Graphics.Gloss.Interface.IO.Game
import Data.Array.IO
import Control.Monad
import System.Random
import Data.List

data World = World { _field :: IOUArray (Int,Int) Int }

width, height :: Int
(width, height) = (4, 4)

size :: Int
size = 100

fps :: Int
fps = 60

(<<) :: Monad m => m a -> m b -> m a
(<<) = flip (>>)

whenM :: Monad m => m Bool -> m () -> m ()
whenM p m = do
        b <- p
        if b then m else return ()

ifM :: Monad m => m Bool -> m a -> m a -> m a
ifM mp ma mb = do
        p <- mp
        if p then ma else mb


moveR :: World -> IO World
moveR (World field) = return (World field) << do
    [0 .. height-1] `forM_` \y -> do
        [width-2, width-3 .. 0] `forM_` \x -> do
            [x .. width-2] `forM_` \i -> do
                v <- readArray field (i, y)
                w <- readArray field (i+1, y)
                if v == w 
                    then writeArray field (i+1, y) (v*2) >> writeArray field (i, y) 0
                    else when (w == 0) $ writeArray field (i+1, y) v >> writeArray field (i, y) 0

moveL :: World -> IO World
moveL (World field) = return (World field) << do
    [0 .. height-1] `forM_` \y -> do
        [1 .. width-1] `forM_` \x -> do
            [x, x-1 .. 1] `forM_` \i -> do
                v <- readArray field (i, y)
                w <- readArray field (i-1, y)
                if v == w 
                    then writeArray field (i-1, y) (v*2) >> writeArray field (i, y) 0
                    else when (w == 0) $ writeArray field (i-1, y) v >> writeArray field (i, y) 0

moveU :: World -> IO World
moveU (World field) = return (World field) << do
    [0 .. width-1] `forM_` \x -> do
        [height-2, height-3 .. 0] `forM_` \y -> do
            [y .. height-2] `forM_` \i -> do
                v <- readArray field (x, i)
                w <- readArray field (x, i+1)
                if v == w 
                    then writeArray field (x, i+1) (v*2) >> writeArray field (x, i) 0
                    else when (w == 0) $ writeArray field (x, i+1) v >> writeArray field (x, i) 0

moveD :: World -> IO World
moveD (World field) = return (World field) << do
    [0 .. width-1] `forM_` \x -> do
        [1 .. height-1] `forM_` \y -> do
            [y, y-1 .. 1] `forM_` \i -> do
                v <- readArray field (x, i)
                w <- readArray field (x, i-1)
                if v == w 
                    then writeArray field (x, i-1) (v*2) >> writeArray field (x, i) 0
                    else when (w == 0) $ writeArray field (x, i-1) v >> writeArray field (x, i) 0

countUp :: World -> IO World
countUp (World field) = return (World field) << do
    return ()


update :: Float -> World -> IO World
update = (return .) . const id

event :: Event -> World -> IO World
event (EventKey (SpecialKey KeyRight) Up _ _) = moveR >=> countUp
event (EventKey (SpecialKey KeyLeft) Up _ _) = moveL >=> countUp
event (EventKey (SpecialKey KeyUp) Up _ _) = moveU >=> countUp
event (EventKey (SpecialKey KeyDown) Up _ _) = moveD >=> countUp
event _ = return

draw :: World -> IO Picture
draw (World field) = fmap (pictures . concat) $ (\x -> return x) =<< do
    [0 .. width-1] `forM` \x -> do
        [0 .. height-1] `forM` \y -> do
            v <- readArray field (x, y)
            let fsize = fromIntegral size
                fx = fromIntegral x
                fy = fromIntegral y
                px = fx*fsize - fromIntegral width * fsize / 2
                py = fy*fsize - fromIntegral width * fsize / 2
            return  $ translate (px) (py) 
                    $ scale 0.2 0.2 
                    $ color black 
                    $ text $ show v

initialField :: IO (IOUArray (Int, Int) Int)
initialField = do
        field <- newListArray ((0,0),(width-1,height-1)) (repeat 0)
        xs <- fmap (take 3 . nub . randomRs (0, width-1)) newStdGen
        ys <- fmap (take 3 . nub . randomRs (0, height-1)) newStdGen
        let list = iterate (* 2) 1
        vs <- fmap (map (list !!) . take 3 . randomRs (0, 2)) newStdGen
        zip3 xs ys vs `forM_` \(x, y, v) -> writeArray field (x, y) v
        return field

main :: IO ()
main = do
        field <- initialField
        playIO (InWindow "2048" (width*size,height*size) (200,200))
            white
            fps
            (World field)
            draw
            event
            update
