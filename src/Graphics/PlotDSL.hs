{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE UnicodeSyntax #-}

module Graphics.PlotDSL where

import Control.Applicative
import Control.Monad
import Control.Monad.Trans.RWS
import Data.Char
import Data.List (intercalate)
import System.Process

------------------------------------------------------------------------------

data Free f a = (Functor f) => Pure a | Roll (f (Free f a))

instance (Functor f) => Functor (Free f) where
  fmap f (Pure a) = Pure (f a)
  fmap f (Roll as) = Roll (fmap (fmap f) as)

instance (Functor f) => Applicative (Free f) where
  pure = Pure
  (Pure f) <*> x = fmap f x
  (Roll f) <*> x = Roll $ fmap (<*> x) f

instance (Functor f) => Monad (Free f) where
  Pure a >>= f = f a
  Roll as >>= f = Roll (fmap (>>= f) as)

liftF :: (Functor f) => f a -> Free f a
liftF = Roll . fmap Pure

retract :: Monad f => Free f a -> f a
retract (Pure a) = return a
retract (Roll as) = as >>= retract

toLowerCase = map toLower

------------------------------------------------------------------------------

data StyleValue = Lines | Points | Dots | Impulses | Linespoints
  deriving (Show)

data ColorValue
  = Default
  | Red
  | Blue
  | Green
  | Yellow
  | Orange
  | Magenta
  | Cyan
  | Violet
  | White
  | Black
  | Raw String
  deriving (Show)

data SmoothValue = Unique | Csplines | Acsplines | Bezier | Sbezier
  deriving (Show)

data Axis = X | Y | XY
  deriving (Show)

data ScaleValue = NoLogscale Axis | Logscale Axis Double
  deriving (Show)

------------------------------------------------------------------------------

data Graph next
  = Name String next
  | Color ColorValue next
  | Style StyleValue next
  | Smooth SmoothValue next
  deriving (Functor)

type GraphF a = Free Graph a

name :: String -> GraphF ()
name n = liftF $ Name n ()

color :: ColorValue -> GraphF ()
color c = liftF $ Color c ()

style :: StyleValue -> GraphF ()
style s = liftF $ Style s ()

smooth :: SmoothValue -> GraphF ()
smooth s = liftF $ Smooth s ()

values :: a -> GraphF a
values = pure

------------------------------------------------------------------------------

data Plot a next
  = Plot (GraphF a) next
  | Scale ScaleValue next
  | Title String next
  deriving (Functor)

type PlotF a = Free (Plot a) ()

plot :: GraphF a -> PlotF a
plot g = liftF $ Plot g ()

title :: String -> PlotF a
title t = liftF $ Title t ()

scale :: ScaleValue -> PlotF a
scale s = liftF $ Scale s ()

------------------------------------------------------------------------------

compileToGnuPlot ::
  (GnuPlotData a) =>
  PlotF a ->
  RWS GnuPlotTerminal String [GraphF a] ()
compileToGnuPlot (Roll (Title t next)) = do
  tell $ "set title \"" ++ t ++ "\"\n"
  compileToGnuPlot next
compileToGnuPlot (Roll (Scale s next)) = do
  tell $ fn s ++ "\n"
  compileToGnuPlot next
  where
    fn (NoLogscale a) = "unset logscale " ++ toLowerCase (show a)
    fn (Logscale a v) = "set logscale " ++ toLowerCase (show a) ++ " " ++ show v
compileToGnuPlot (Roll (Plot g next)) = do
  s <- get
  put $ s ++ [g]
  compileToGnuPlot next
compileToGnuPlot (Pure ()) = do
  gs <- get
  t <- ask
  tell $ "set term " ++ toLowerCase (show t) ++ "\n"
  tell $ compileGraphs gs ++ concatMap (serialize . extract) gs
  where
    extract :: GraphF a -> a
    extract (Pure a) = a
    extract (Roll (Name _ n)) = extract n
    extract (Roll (Color _ n)) = extract n
    extract (Roll (Smooth _ n)) = extract n
    extract (Roll (Style _ n)) = extract n

compileGraphs :: [GraphF a] -> String
compileGraphs gs = "plot " ++ intercalate ", " (map f gs) ++ "\n"
  where
    f :: GraphF a -> String
    f x = show "-" ++ " " ++ compileGraphAttributes x

compileGraphAttributes :: GraphF a -> String
compileGraphAttributes (Roll (Name t next)) =
  unwords ["title", show t, compileGraphAttributes next]
compileGraphAttributes (Roll (Color c next)) =
  unwords [compileColorToGnuPlot c, compileGraphAttributes next]
compileGraphAttributes (Roll (Style s next)) =
  unwords [compileStyleToGnuPlot s, compileGraphAttributes next]
compileGraphAttributes (Roll (Smooth s next)) =
  unwords [compileSmoothToGnuPlot s, compileGraphAttributes next]
compileGraphAttributes (Pure _) = ""

compileColorToGnuPlot :: ColorValue -> String
compileColorToGnuPlot c = " linecolor rgb(\"" ++ toLowerCase (show c) ++ "\") "

compileStyleToGnuPlot :: StyleValue -> String
compileStyleToGnuPlot s = " with " ++ toLowerCase (show s)

compileSmoothToGnuPlot :: SmoothValue -> String
compileSmoothToGnuPlot s = " smooth " ++ toLowerCase (show s)

------------------------------------------------------------------------------

data GnuPlotTerminal = Dumb | Wxt | X11 deriving (Show)

gnuplot :: (GnuPlotData a) => GnuPlotTerminal -> PlotF a -> IO String
gnuplot t p = readProcess "gnuplot" ["-p"] plotScript
  where
    plotScript = snd $ evalRWS (compileToGnuPlot p) t []

dumbplot, wxtplot :: (GnuPlotData a) => PlotF a -> IO ()
dumbplot = gnuplot Dumb >=> putStrLn
wxtplot = void . gnuplot Wxt

------------------------------------------------------------------------------

class GnuPlotData a where
  serialize :: a -> String

instance (Show a, Show b) => GnuPlotData [(a, b)] where
  serialize = pairsToGnuplotData

pairsToGnuplotData :: (Show a, Show b) => [(a, b)] -> String
pairsToGnuplotData [] = "e\n"
pairsToGnuplotData ((x, y) : xs) =
  concat [show x, " ", show y, "\n"] ++ pairsToGnuplotData xs
