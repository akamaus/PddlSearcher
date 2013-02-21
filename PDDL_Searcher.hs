{-# LANGUAGE NoImplicitPrelude, OverloadedStrings, NoMonomorphismRestriction #-}
module PDDL_Searcher where

import ClassyPrelude
import PDDL_Parser

import Data.Graph.AStar

type Env = Map Pat Obj

--isApplicable :: [Predicate Obj] -> Action
solveProblem :: Domain -> Problem -> IO (Maybe [Facts])
solveProblem domain problem = aStarM moves cost heuro finish start
 where moves facts = do putStrLn $ "at " ++ show facts;
                        let facts' = concatMap (applyAction `flip` facts) (dActions domain)
                        return $ fromList facts'
       cost a b = return 1
       heuro x = return 1
       finish x = let g = fromList (pGoal problem) in do print x; return $ g == g `intersect` x
       start = return $ pInitState problem

-- применяет действие всеми возможными способами
applyAction :: Action -> Facts -> [Facts]
applyAction act facts = do
  env <- applyPrecondition empty (aPreconditions act) facts
  return $ applyEffects env (aEffects act) facts

-- сопоставляет предусловие действия целиком
applyPrecondition :: Env -> [Pattern] -> Facts -> [Env]
applyPrecondition env [] _ = [env]
applyPrecondition env (p:ps) facts = do
  env' <- findMatches env p facts
  applyPrecondition env' ps facts

-- применяет эффекты действия
applyEffects :: Env -> [Pattern] -> Facts -> Facts
applyEffects _ [] facts = facts
applyEffects env (p:ps) facts = let new_fact = instantinatePattern env p
                                in case new_fact of
                                  PosPred _ _ -> applyEffects env ps (insert new_fact facts)
                                  NegPred _ _ -> applyEffects env ps (facts \\ singleton (negatePred new_fact))

-- сопоставляет шаблон предиката с фактами, на каждую удачу возвращает новый словарь переменных
findMatches :: Env -> Pattern -> Facts -> [Env]
findMatches env pred facts = let
  matches = catMaybes $ map (match env (predArgs pred) . predArgs) $ toList facts
 in case pred of
      PosPred _ _ -> matches
      NegPred _ args -> case () of
        _ | not $ all (\p -> isJust $ lookup p env) $ nub args -> error "negative predicate must be last"
        _ -> case matches of
               [] -> [env] -- отрицательный предикат не должен ни с кем сопоставляться
               _  -> []

-- попытка сопоставить шаблон с фактом
match :: Env -> [Pat] -> [Obj] -> Maybe Env
match env [] [] = Just env
match env [] (o:_) = error "fact has more arguments than a template"
match env (p:_) [] = error "template has more arguments than a fact"
match env (p:ps) (f:fs) = case lookup p env of
  Nothing -> match (insert p f env) ps fs
  Just v | v == f -> match env ps fs
         | True -> Nothing

-- трансформирует шаблон в факт, для этого все переменные должны быть словаре
instantinatePattern :: Env -> Pattern -> Fact
instantinatePattern env p = p {predArgs = instantinatePatternArgs env (predArgs p)}

instantinatePatternArgs :: Env -> [Pat] -> [Obj]
instantinatePatternArgs env [] = []
instantinatePatternArgs env (p:ps) = case lookup p env of
  Just v -> v : instantinatePatternArgs env ps
  Nothing -> error $ "can't instantinate " ++ show p