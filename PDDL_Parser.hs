{-# LANGUAGE NoImplicitPrelude, OverloadedStrings, NoMonomorphismRestriction #-}

module PDDL_Parser where

import ClassyPrelude

import qualified Data.List as L
import Data.Maybe

import Data.AttoLisp
import Data.Attoparsec as A
import Filesystem.Path.CurrentOS

import Debug.Trace

data Domain = Domain { dName :: Text, dActions :: [Action] } deriving Show
data Problem = Problem { pName :: Text, pDomain :: Text, pInitState :: Facts, pGoal :: [Fact]} deriving Show

data Action = Action { aName :: Text, aPreconditions :: [Pattern], aEffects :: [Pattern] } deriving Show

type Fact = Predicate Obj
type Pattern = Predicate Pat

type Facts = Set Fact

newtype Pat = Pat Text deriving (Show, Eq, Ord)
newtype Obj = Obj Text deriving (Show, Eq, Ord)

data Mixed = MPat Pat | MObj Obj deriving Show

isObj (MObj _) = True
isObj (MPat _) = False

toObj (MObj x) = x

data Predicate p = PosPred {predName :: Text, predArgs :: [p]} | NegPred {predName :: Text, predArgs :: [p]} deriving (Show, Eq, Ord)

negatePred (PosPred t a) = NegPred t a
negatePred (NegPred t a) = PosPred t a

parse_pddl parser fname = do
  let f = decode fname
  txt <- readFile f
  let res = parseOnly lisp txt
  case res of
    Right r -> return $ parser r
    Left msg -> fail msg

sexpToDomain sexp = do
  name <- findTuple "domain" sexp
  let actions_sexps = filterSubExp ":action" sexp
  let actions = map parse_action actions_sexps
  return $ Domain name actions

sexpToProblem sexp = do
  name <- findTuple "problem" sexp
  domain <- findTuple ":domain" sexp
  init <- (parse_predicate_list Obj . List . L.tail) <$> findSubExp ":init" sexp
  goal <- (parse_predicate_list Obj . (\ [x, lst] -> lst)) <$> findSubExp ":goal" sexp
  return $ Problem name domain (fromList init) goal

parse_action sexp = let
  pairs = sexpToPairs sexp
  name = fromMaybe (error "no action name") $ unSymbol <$> lookup ":action" pairs
  precond = fromMaybe (error "no precondition") $ parse_predicate_list Pat <$> lookup ":precondition" pairs
  effect = fromMaybe (error "no effect") $ parse_predicate_list Pat <$> lookup ":effect" pairs
 in Action name precond effect

parse_predicate_list wrap (List ((Symbol "and") : rest)) = map (parse_predicate wrap) rest
parse_predicate_list wrap (List lst) = map (parse_predicate wrap) lst

parse_predicate wrap (List [Symbol "not", pred]) = let p = parse_predicate wrap pred
                                                   in NegPred {predName = predName p, predArgs = predArgs p}
parse_predicate wrap (List (Symbol name: args)) = PosPred name (map (wrap . unSymbol) args)

findSubExp name lst = listToMaybe $ filterSubExp name lst

filterSubExp name (List lst) = (\(List xs) -> xs) <$> filter (\e -> case e of List (Symbol n : _) | n == name -> True; _ -> False) lst
sexpToPairs (Symbol x: y: rest) = (x,y) : sexpToPairs rest
sexpToPairs [] = []

findTuple name sexp = (\ [_, Symbol x] -> x) <$> findSubExp name sexp

unSymbol (Symbol s) = s