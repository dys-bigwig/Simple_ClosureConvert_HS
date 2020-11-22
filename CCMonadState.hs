{-# LANGUAGE FlexibleContexts #-}
module CC where

import Data.List
import Data.Maybe
import Control.Applicative
import Control.Monad.State
import Control.Monad.State.Class

data Scope = Scope { locals :: [Expr]
                   , env :: [Expr]
                   , captures :: [Expr]
                   , globals :: [Expr] } deriving(Eq, Show)

data Storage = Loc | Env | Glo deriving(Eq, Show)

data Expr = EVar String
          | EBool Bool
          | EInt Int
          | ELam [Expr] Expr
          | EApp Expr Expr deriving(Eq, Show)

data CExpr = CVar Storage Int
           | CBool Bool
           | CInt Int
           | CClos [CExpr] CExpr
           | CApp CExpr CExpr deriving(Eq, Show)

fromLocal :: (MonadState Scope m) => Expr -> m CExpr
fromLocal v = do
  scope <- get
  case (elemIndex v $ env scope) of
    (Just i) -> return $ CVar Loc i

fromGlobals :: (MonadState Scope m) => Expr -> m CExpr
fromGlobals v = do
  scope <- get
  case (elemIndex v $ env scope) of
    (Just i) -> return $ CVar Glo i

classify :: (MonadState Scope m) => Expr -> m CExpr
classify var = do
  scope <- get
  f var scope
  where f var scope | (Just i) <- var `inLocals` scope = return $ CVar Loc i
                    | (Just i) <- var `inEnv` scope =
                      case var `inCaptures` scope of
                        (Just i) -> return $ CVar Env i
                        Nothing -> do put $ scope{captures = captures scope ++ [var]} 
                                      scope' <- get
                                      return $ CVar Env ((length $ captures scope') - 1)
                    | (Just i) <- var `inGlobals` scope = return $ CVar Glo i 
                    | otherwise = error $ show var ++ "unbound" ++ (show scope)
        inLocals v s = elemIndex v (locals s)
        inEnv v s = elemIndex v (env s)
        inCaptures v s = elemIndex v (captures s)
        inGlobals v s = elemIndex v (globals s)

cc :: (MonadState Scope m) => Expr -> m CExpr
cc expr = do
  scope <- get
  case expr of EVar _ -> classify expr
               EBool b -> return $ CBool b
               EInt i -> return $ CInt i
               ELam vs e -> do vs' <- mapM cc (captures bodyScope)
                               return $ CClos vs' e'
                               where (e', bodyScope) = runState (cc e) (Scope vs
                                                                              (locals scope ++ env scope)
                                                                              []
                                                                              (globals scope))
               EApp e1 e2 -> do e1' <- cc e1
                                e2' <- cc e2
                                return $ CApp e1' e2'
