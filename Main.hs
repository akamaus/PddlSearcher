{-# LANGUAGE NoImplicitPrelude, OverloadedStrings #-}
module Main where

import ClassyPrelude

import PDDL_Parser
import PDDL_Searcher

import Filesystem.Path.CurrentOS

main = do
  args <- getArgs
  case args of
    [domain_file, problem_file] -> do
      dom <- parse_pddl sexpToDomain (fromText domain_file)
      prob <- parse_pddl sexpToProblem (fromText problem_file)
      case (dom, prob) of
        (Just d, Just p) -> do
          sol <- solveProblem d p
          case sol of
            Nothing -> putStrLn "No solution found"
            (Just s) -> do putStrLn "Found solution:"
                           mapM_ print s
    _ -> print "Usage: pddl_searcher <domain description file> <problem description file>"
