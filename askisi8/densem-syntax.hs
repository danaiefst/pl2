-- Syntax
import Debug.Trace

data P = Pint Integer | Ptrue | Pfalse
       | Padd | Pneg | Pmul | Pdiv | Plt | Peq | Pand | Pnot
       | Pnop | Pdup | Ppop | Pswap | Pswap2
       | Pseq P P | Pcond P P | Ploop P

-- Denotational semantics

data V = VI Integer | VB Bool

data T = I | B deriving (Eq, Show)

type S = [V]
type ST = [T]

type_check :: P -> ST -> ST
type_check p s = case p of
  Pseq p1 p2 -> type_check p2 (type_check p1 s)
  Pint n -> (I : s)
  Ptrue -> (B : s)
  Pfalse -> (B : s)
  Padd -> case s of
    (I : I : t) -> (I : t)
    _ -> error "Invalid arguments for operator '+'"
  Pneg -> case s of
    (I : t) -> (I : t)
    _ -> error "Invalid arguments for operator '-'"
  Pmul -> case s of
    (I : I : t) -> (I : t)
    _ -> error "Invalid arguments for operator '*'"
  Pdiv -> case s of
    (I : I : t) -> (I : I : t)
    _ -> error "Invalid arguments for operator '/'"
  Plt -> case s of
    (I : I : t) -> (B : t)
    _ -> error "Invalid arguments for operator '<'"
  Peq -> case s of
    (B : B : t) -> (B : t)
    (I : I : t) -> (B : t)
    _ -> error "Invalid arguments for operator '='"
  Pand -> case s of
    (B : B : t) -> (B : t)
    _ -> error "Invalid arguments for operator 'and'"
  Pnot -> case s of
    (B : t) -> (B : t)
    _ -> error "Invalid arguments for operator 'not'"
  Pnop -> s
  Pdup -> case s of
    (elem : t) -> (elem : elem : t)
    _ -> error "Not enough arguments for function 'dup'"
  Ppop -> case s of
    (elem : t) -> t
    _ -> error "Not enough arguments for function 'pop'"
  Pswap -> case s of
    (elem1 : elem2 : t) -> (elem2 : elem1 : t)
    _ -> error "Not enough arguments for function 'swap'"
  Pswap2 -> case s of
    (elem1 : elem2 : elem3 : t) -> (elem3 : elem2 : elem1 : t)
    _ -> error "Not enough arguments for function 'swap2'"
  Pcond p1 p2 -> case s of
    (B : t) -> let t1 = type_check p1 t
                   t2 = type_check p2 t
                  in if (t1 == t2) then t1 else
                    error "'cond' arguments must be of same type"
    _ -> error "Invalid argument for function 'cond'"
  Ploop p1 -> case s of
      (B : t) -> case (type_check p1 t) of
          (B : t1) -> if t1 == t then t1 else
            error "'loop' argument must preserve stack and add bool on head"
          _ -> error "'loop' argument must preserve stack and add bool on head"
      _ -> error "Invalid argument for function 'loop'"


sem :: P -> S -> S
sem p s = case p of
  Pseq p1 p2 -> sem p2 (sem p1 s)
  Pint n -> (VI n : s)
  Ptrue -> (VB True : s)
  Pfalse -> (VB False : s)
  Padd -> case s of
    (VI n1 : VI n2 : t) -> (VI (n1 + n2) : t)
  Pneg -> case s of
    (VI n : t) -> (VI (-n) : t)
  Pmul -> case s of
    (VI n1 : VI n2 : t) -> (VI (n1 * n2) : t)
  Pdiv -> case s of
    (VI n1 : VI n2 : t) -> (VI (mod n2 n1) : VI (div n2 n1) : t)
  Plt -> case s of
    (VI n1 : VI n2 : t) -> (VB (n2 < n1) : t)
  Peq -> case s of
    (VB b1 : VB b2 : t) -> (VB (b2 == b1) : t)
    (VI n1 : VI n2 : t) -> (VB (n1 == n2) : t)
  Pand -> case s of
    (VB b1 : VB b2 : t) -> (VB (b2 && b1) : t)
  Pnot -> case s of
    (VB b1 : t) -> (VB (not b1) : t)
  Pnop -> s
  Pdup -> case s of
    (elem : t) -> (elem : elem : t)
  Ppop -> case s of
    (elem : t) -> t
  Pswap -> case s of
    (elem1 : elem2 : t) -> (elem2 : elem1 : t)
  Pswap2 -> case s of
    (elem1 : elem2 : elem3 : t) -> (elem3 : elem2 : elem1 : t)
  Pcond p1 p2 -> case s of
    (VB b : t) -> if b then (sem p1 t) else (sem p2 t)
  Ploop p1 -> case s of
      (VB b : t) -> if b then (sem (Ploop p1) (sem p1 t)) else t

-- Main function: interpreter

main = do
  input <- getContents
  let x = read input :: P
  let y = type_check x []
  mapM_ print $ y `seq` sem x []

-- Pretty-printing

instance Show P where
  showsPrec d (Pint n) = showsPrec 0 n
  showsPrec d Ptrue = ("true" ++)
  showsPrec d Pfalse = ("false" ++)
  showsPrec d Padd = ("+" ++)
  showsPrec d Pneg = ("-" ++)
  showsPrec d Pmul = ("*" ++)
  showsPrec d Pdiv = ("/" ++)
  showsPrec d Plt = ("<" ++)
  showsPrec d Peq = ("=" ++)
  showsPrec d Pand = ("and" ++)
  showsPrec d Pnot = ("not" ++)
  showsPrec d Pnop = ("nop" ++)
  showsPrec d Pdup = ("dup" ++)
  showsPrec d Ppop = ("pop" ++)
  showsPrec d Pswap = ("swap" ++)
  showsPrec d Pswap2 = ("swap2" ++)
  showsPrec d (Pseq p1 p2) =
    showParen (d > 0) $ showsPrec 1 p1 . (" " ++) . showsPrec 0 p2
  showsPrec d (Pcond p1 p2) =
    ("cond [" ++) . showsPrec 0 p1 . (" | " ++)
                  . showsPrec 0 p2 . ("]" ++)
  showsPrec d (Ploop p) = ("loop [" ++) . showsPrec 0 p . ("]" ++)

instance Show V where
  showsPrec d (VI n) = showsPrec d n
  showsPrec d (VB True) = ("true" ++)
  showsPrec d (VB False) = ("false" ++)

-- Parsing

next (x : r) = [(x, r)]
next s = []

instance Read P where
  readsPrec d s =
    readParen False   (\s -> [(Pint n, r)      | (n, r) <- readsPrec 0 s]) s ++
    readParen False   (\s -> [(Ptrue, r)       | ("true", r) <- lex s]) s ++
    readParen False   (\s -> [(Pfalse, r)      | ("false", r) <- lex s]) s ++
    readParen False   (\s -> [(Padd, r)        | ("+", r) <- lex s]) s ++
    readParen False   (\s -> [(Pneg, r)        | ("-", r) <- lex s]) s ++
    readParen False   (\s -> [(Pmul, r)        | ("*", r) <- lex s]) s ++
    readParen False   (\s -> [(Pdiv, r)        | ("/", r) <- lex s]) s ++
    readParen False   (\s -> [(Plt, r)         | ("<", r) <- lex s]) s ++
    readParen False   (\s -> [(Peq, r)         | ("=", r) <- lex s]) s ++
    readParen False   (\s -> [(Pand, r)        | ("and", r) <- lex s]) s ++
    readParen False   (\s -> [(Pnot, r)        | ("not", r) <- lex s]) s ++
    readParen False   (\s -> [(Pnop, r)        | ("nop", r) <- lex s]) s ++
    readParen False   (\s -> [(Pdup, r)        | ("dup", r) <- lex s]) s ++
    readParen False   (\s -> [(Ppop, r)        | ("pop", r) <- lex s]) s ++
    readParen False   (\s -> [(Pswap, r)       | ("swap", r) <- lex s]) s ++
    readParen False   (\s -> [(Pswap2, r)      | ("swap2", r) <- lex s]) s ++
    readParen (d > 0) (\s -> [(Pseq p1 p2, r)  | (p1, t) <- readsPrec 1 s,
                                                 (p2, r) <- readsPrec 0 t]) s ++
    readParen False   (\s -> [(Pcond p1 p2, r) | ("cond", t1) <- lex s,
                                                 ("[", t2) <- lex t1,
                                                 (p1, t3) <- readsPrec 0 t2,
                                                 ("|", t4) <- lex t3,
                                                 (p2, t5) <- readsPrec 0 t4,
                                                 ("]", r) <- lex t5]) s ++
    readParen False   (\s -> [(Ploop p, r)     | ("loop", t1) <- lex s,
                                                 ("[", t2) <- lex t1,
                                                 (p, t3) <- readsPrec 0 t2,
                                                 ("]", r) <- lex t3]) s
