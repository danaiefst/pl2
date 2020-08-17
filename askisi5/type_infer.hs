import Text.Read
import Data.Map.Strict as M
import Debug.Trace

data Type  =  Tvar Int | Tfun Type Type                        deriving Eq
data Expr  =  Evar String | Eabs String Expr | Eapp Expr Expr  deriving Eq

data Rule = Curry Type Type

instance Show Rule where
  show (Curry t1 t2) = "Curry " ++ show t1 ++ " " ++ show t2

always = True

-- False omits parentheses whenever possible

instance Show Expr where
  showsPrec p (Evar x) = (x ++)
  showsPrec p (Eabs x e) =
    showParen (always || p > 0) ((("\\" ++ x ++ ". ") ++) . showsPrec 0 e)
  showsPrec p (Eapp e1 e2) =
    showParen (always || p > 1) (showsPrec 1 e1 . (" " ++) . showsPrec 2 e2)

-- Parsing of expressions

instance Read Expr where
  readPrec = (do Ident x <- lexP
                 return (Evar x)) <++
             (do Punc "(" <- lexP
                 Punc "\\" <- lexP
                 Ident x <- lexP
                 Symbol "." <- lexP
                 e <- readPrec
                 Punc ")" <- lexP
                 return (Eabs x e)) <++
             (do Punc "(" <- lexP
                 e1 <- readPrec
                 e2 <- readPrec
                 Punc ")" <- lexP
                 return (Eapp e1 e2))

-- Pretty printing of types

instance Show Type where
  showsPrec p (Tvar alpha) = ("@" ++) . showsPrec 0 alpha
  showsPrec p (Tfun sigma tau) =
    showParen (p > 0) (showsPrec 1 sigma . (" -> " ++) . showsPrec 0 tau)


-- First type assignment

first_type :: Expr -> (Type, [Rule], Bool)
first_type expr = let (_, tp, rules, check) = first_type' 0 M.empty expr
                    in (tp, rules, check)
  where first_type' id dict (Evar str) = case M.lookup str dict of
                                           Nothing -> (id, Tvar (-1), [], True)
                                           Just v -> (id, Tvar v, [], False)
        first_type' id dict (Eabs str expr) = let (id', t, rules, err) = first_type' (id+1) (M.insert str id dict) expr
                                               in (id', Tfun (Tvar id) t, rules, err)

        first_type' id dict (Eapp e1 e2) = let (id', t1, rules1, err1) = first_type' id dict e1
                                               (id'', t2, rules2, err2) = first_type' id' dict e2
                                            in (id'' + 1, Tvar id'', Curry t1 (Tfun t2 (Tvar id'')) : (rules1++rules2), err1 || err2)

-- Unification of Rules

unify :: [Rule] -> ([Rule], Bool)
unify [] = ([], False)
unify (Curry (Tfun t11 t12) (Tfun t21 t22) : rest) = unify (Curry t11 t21 : Curry t12 t22 : rest)
unify (Curry (Tvar v) t2 : rest) = if (Tvar v) == t2 then
                                    let (rest', err) = unify rest
                                      in (rest', err)
                                   else
                                    if exists (Tvar v) t2 then ([], True) else let (rest', err) = unify (Prelude.map (substitute (Tvar v) t2) rest)
                                                                                in ((Curry (Tvar v) t2 : rest'), err)
unify (Curry t1 (Tvar v) : rest) = unify (Curry (Tvar v) t1 : rest)

exists t (Tfun t1 t2) = exists t t1 || exists t t2
exists t t1 = (t == t1)

substitute :: Type -> Type -> Rule -> Rule
substitute prev_t next_t (Curry t1 t2) = let t1' = substitute' prev_t next_t t1
                                             t2' = substitute' prev_t next_t t2
                                          in Curry t1' t2'

substitute' prev_t next_t (Tvar v) = if prev_t == (Tvar v) then next_t else (Tvar v)
substitute' prev_t next_t (Tfun t1 t2) = Tfun (substitute' prev_t next_t t1) (substitute' prev_t next_t t2)

-- Final form

final :: [Rule] -> Type -> Type
final [] tp = trace (show tp) $ rearrange tp
final (Curry t1 t2 : rest) tp =  final rest (substitute' t1 t2 tp)

rearrange :: Type -> Type
rearrange tp = let (t, _, _) = rearrange' M.empty 0 tp in t
  where rearrange' dict id (Tvar v) = case M.lookup v dict of
                                        Nothing -> (Tvar id, id + 1, M.insert v id dict)
                                        Just var -> (Tvar var, id, dict)
        rearrange' dict id (Tfun t1 t2) = let (t1', id', dict') = rearrange' dict id t1
                                              (t2', id'', dict'') = rearrange' dict' id' t2
                                           in (Tfun t1' t2', id'', dict'')


-- Main program

doOne  =  do  s <- getLine
              let e = read s :: Expr
              let (tp, rules, check) = first_type e

              if check then putStrLn ("type error") else
                let (unified, check') = unify rules
                  in trace (show rules ++ show unified) $ if check' then putStrLn ("type error") else print (final unified tp)

count n m  =  sequence $ take n $ repeat m

main     =  do  n <- readLn
                count n doOne
