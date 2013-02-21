{-# LANGUAGE NoImplicitPrelude, OverloadedStrings, NoMonomorphismRestriction #-}
module PDDL_Searcher where

import PDDL_Parser

type Env = Map Var Pat

isApplicable :: [Predicate Obj] -> Action

findMatches :: Env -> Predicate Pat -> [Predicate Obj] -> [Env]
findMatches env pred facts = let
  mixed = mix env pred
  case mixed of
    PosPred _ _ -> catMaybes $ map (match env mixed) facts
    PosNeg _ _ ->









