{-# LANGUAGE MultiWayIf #-}
module CC where
import Data.List
import Data.Maybe
import Control.Applicative
import Control.Monad.State

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

classify :: Expr -> State Scope CExpr 
classify var = do
  scope <- get
  f var scope
  where f var scope | (Just i) <- elemIndex var $ locals scope = return $ CVar Loc i
                    | (Just i) <- elemIndex var $ env scope =
                      case elemIndex var $ captures scope of
                        (Just i) -> return $ CVar Env i
                        Nothing -> do put $ Scope (locals scope)
                                                  (env scope)
                                                  (captures scope ++ [var])
                                                  (globals scope)
                                      scope' <- get
                                      return $ CVar Env ((length $ captures scope') - 1)
                    | (Just i) <- elemIndex var $ globals scope = return $ CVar Glo i 
                    | otherwise = error $ show var ++ "unbound" ++ (show scope)
        f ::  Expr -> Scope -> State Scope CExpr

cc :: Expr -> State Scope CExpr
cc expr = do
  scope <- get
  case expr of EVar _ -> classify expr
               EBool b -> return $ CBool b
               EInt i -> return $ CInt i
                               -- (Expr -> State Scope) -> [Expr] -> State Scope [Expr]
               ELam vs e -> do vs' <- mapM cc (captures bodyScope)
                               return $ CClos vs' e'
                               where (e', bodyScope) = runState (cc e) (Scope vs
                                                                       (locals scope ++ env scope)
                                                                       []
                                                                       (globals scope))
               EApp e1 e2 -> do e1' <- cc e1
                                e2' <- cc e2
                                return $ CApp e1' e2'
