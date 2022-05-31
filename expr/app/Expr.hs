{-# LANGUAGE OverloadedStrings #-}

module Expr where

import Flow ((|>))
import qualified TextShow as TS

data Expr a
  = Lit a
  | Add (Expr a) (Expr a)
  | Mult (Expr a) (Expr a)

myEval :: Num a => Expr a -> a
myEval expr =
  case expr of
    Lit a ->
      a
    Add e1 e2 ->
      myEval e1 + myEval e2
    Mult e1 e2 ->
      myEval e1 * myEval e2

instance TS.TextShow a => TS.TextShow (Expr a) where
  showbPrec p e =
    case e of
      Lit a ->
        TS.showb a
      Add e1 e2 ->
        showbPrec' p 5 "+" e1 e2
      Mult e1 e2 ->
        showbPrec' p 6 "*" e1 e2
    where
      showbPrec' outerPrec innerPrec op e1 e2 =
        TS.showbPrec innerPrec e1 <> op <> TS.showbPrec innerPrec e2
          |> TS.showbParen (outerPrec > innerPrec)

expr1 :: Expr Int
expr1 =
  Mult (Add (Lit 2) (Mult (Lit 3) (Lit 3))) (Lit 5)

expr2 :: Expr Int
expr2 =
  Add
    (Add (Lit 1) (Mult (Add (Lit 1) (Lit 2)) (Add (Lit 2) (Mult (Lit 2) (Add (Lit 1) (Lit 2))))))
    (Add (Lit 1) (Mult (Lit 3) (Lit 2)))
