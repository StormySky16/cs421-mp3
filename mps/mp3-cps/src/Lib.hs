--- Given Code
--- ==========

module Lib where

import System.IO (hPutStrLn, hPutStr, stdout, hFlush)

import Data.List (intercalate)

import Data.Functor.Identity (Identity)
import Text.ParserCombinators.Parsec hiding (Parser)
import Text.Parsec.Prim (ParsecT)

--- The Types
--- ---------

data Stmt = Decl String [String] Exp
            deriving (Eq)

instance Show Stmt where
    show (Decl f params exp) = f ++ " " ++ intercalate " " params ++ " = " ++ (show exp)

data Exp = IntExp Integer
         | VarExp String
         | LamExp String Exp
         | IfExp Exp Exp Exp
         | OpExp String Exp Exp
         | AppExp Exp Exp
         deriving (Eq)

instance Show Exp where
    show (VarExp s)       = s
    show (IntExp i)       = show i
    show (LamExp x e)     = "(\\" ++ x ++ " -> " ++ (show e) ++ ")"
    show (IfExp e1 e2 e3) = "(if " ++ show e1 ++ " then " ++ show e2
                            ++ " else " ++ show e3 ++ ")"
    show (OpExp op e1 e2) = "(" ++ show e1 ++ " " ++ op ++ " " ++ show e2 ++ ")"
    show (AppExp f e)     = show f ++ " " ++ show e

ctorShow :: Exp -> String
ctorShow (VarExp s)       = "VarExp " ++ show s
ctorShow (IntExp i)       = "IntExp " ++ show i
ctorShow (LamExp x e)     = "LamExp " ++ show x ++ " (" ++ ctorShow e ++ ")"
ctorShow (IfExp e1 e2 e3) = "IfExp (" ++ ctorShow e1 ++ ") ("
                                ++ ctorShow e2 ++ ") ("
                                ++ ctorShow e3 ++ ")"
ctorShow (OpExp op e1 e2) = "OpExp " ++ show op ++ " ("
                                ++ ctorShow e1 ++ ") ("
                                ++ ctorShow e2 ++ ")"
ctorShow (AppExp f e)     = "AppExp (" ++ ctorShow f ++ ") (" ++ ctorShow e ++ ")"

--- Problems
--- ========

--- Manual Translation
--- ------------------

--- ### `factk :: Integer -> (Integer -> t) -> t`

factk :: Integer -> (Integer -> t) -> t
factk 0 k = k 1
factk n k = factk (n - 1) (\result -> k (n * result))

--- ### `evenoddk :: [Integer] -> (Integer -> t) -> (Integer -> t) -> t`

evenoddk :: [Integer] -> (Integer -> t) -> (Integer -> t) -> t
evenoddk [x] evenk oddk
    | even x = evenk x
    | otherwise = oddk x
evenoddk (x:xs) evenk oddk
    | even x = evenoddk xs (\result -> evenk (x + result)) oddk
    | otherwise = evenoddk xs evenk (\result -> oddk (x + result))
    --Assisted by ChatGPT to figure out how best to find state when single element remained in a list

--- Automated Translation
--- ---------------------

gensym :: Integer -> (String, Integer)
gensym i = ("v" ++ show i, i + 1)

--- ### Define `isSimple`

isSimple :: Exp -> Bool
isSimple (AppExp e1 args) = False
isSimple (IfExp e1 e2 e3) = isSimple e1 && isSimple e2 && isSimple e3
isSimple (OpExp op e1 e2) = isSimple e1 && isSimple e2
isSimple _ = True

--- ### Define `cpsExp` - Overview

cpsExp :: Exp -> Exp -> Integer -> (Exp, Integer)

--- #### Define `cpsExp` for Integer and Variable Expressions
cpsExp (IntExp i) k n = (AppExp k (IntExp i), n)
cpsExp (VarExp v) k n = (AppExp k (VarExp v), n)
--- #### Define `cpsExp` for Application Expressions
cpsExp (AppExp f exp) k n
    | isSimple exp = (AppExp (AppExp f exp) k, n)
    | otherwise = let (v, n') = gensym n
                  in cpsExp exp (LamExp v (AppExp (AppExp f (VarExp v)) k)) n'
                  --referenced Campuswire Post for creation of recursive case

--- #### Define `cpsExp` for Operator Expressions
cpsExp (OpExp op e1 e2) k n
    | isSimple e1 && isSimple e2 = (AppExp k (OpExp op e1 e2) , n)
    | not (isSimple e1) && isSimple e2 =
        let (v, n') = gensym n
        in cpsExp e1 (LamExp v (AppExp k (OpExp op (VarExp v) e2))) n'
    | isSimple e1 && not (isSimple e2) =
        let (v, n') = gensym n
        in cpsExp e2 (LamExp v (AppExp k (OpExp op e1 (VarExp v)))) n'
    | otherwise =
        let (v1, n') = gensym n
            (v2, n'') = gensym n'
            (cpsExp2, _) = cpsExp e2 (LamExp v2 (AppExp k (OpExp op (VarExp v1) (VarExp v2)))) n'
        in cpsExp e1 (LamExp v1 cpsExp2) n''
        --referenced Campuswire posts regarding how to deal with the nested call
--- #### Define `cpsExp` for If Expressions
cpsExp (IfExp e1 e2 e3) k n
    | isSimple e1 = 
        let (cpsExp2, _) = cpsExp e2 k n
            (cpsExp3, _) = cpsExp e3 k n
        in (IfExp e1 cpsExp2 cpsExp3, n)
    | otherwise =
        let (v, n') = gensym n
            (cpsExp2, _) = cpsExp e2 k n'
            (cpsExp3, _) = cpsExp e3 k n'
        in cpsExp e1 (LamExp v (IfExp (VarExp v) cpsExp2 cpsExp3)) n'
--- ### Define `cpsDecl`

cpsDecl :: Stmt -> Stmt
cpsDecl (Decl f params e1) = 
    let (cpsExp1, _) = cpsExp e1 (VarExp "k") 0
    in Decl f (params ++ ["k"]) cpsExp1
    --referenced Shang Lu's CampusWire reply regarding what "k" was and how to properly create the return value
