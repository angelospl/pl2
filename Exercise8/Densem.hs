-- Syntax

data P = Pint Integer | Ptrue | Pfalse
       | Padd | Pneg | Pmul | Pdiv | Plt | Peq | Pand | Pnot
       | Pnop | Pdup | Ppop | Pswap | Pswap2
       | Pseq P P | Pcond P P | Ploop P

-- Denotational semantics

data V = VI Integer | VB Bool | Exn Integer

type S = [V]

pop::S -> (V,S)
pop s = case s of
  (a:xs) -> (a,xs)
  otherwise -> (Exn 1,[])

pop2::S -> (V,V,S)
pop2 s = case s of
  (a:b:xs) -> (a,b,xs)
  otherwise -> (Exn 1,Exn 1,[])

fix :: (a -> a) -> a
fix f = f (fix f)

sem :: P -> S -> S
sem p s = case p of
  Pint n -> (VI n:s)
  Ptrue -> (VB True:s)
  Pfalse -> (VB False:s)
  Padd -> (VI (x + y):xs) where
    (VI x,VI y,xs) = pop2 s
  Pneg -> (VI (-x):xs)
    where (VI x,xs) = pop s
  Pmul -> (VI (x * y):xs) where
    (VI x,VI y,xs) = pop2 s
  Pdiv -> (VI (x `div` y):VI (x `mod` y):xs) where
    (VI x,VI y,xs) = pop2 s
  Plt -> if x < y then (VB True:xs)
        else (VB False:xs)
        where
          (VI x,VI y,xs) = pop2 s
  Peq -> if x == y then (VB True:xs)
         else (VB False:xs)
         where
           (VI x,VI y,xs) = pop2 s
  Pand -> (VB (x && y):xs) where
    (VB x,VB y,xs) = pop2 s
  Pnot -> (VB (not x):xs) where
    (VB x,xs) = pop s
  Pnop -> []
  Pdup -> (x:s) where
    (x,_) = pop s
  Ppop -> xs where
    (_,xs) = pop s
  Pswap -> (y:x:xs) where
    (x,y,xs) = pop2 s
  Pswap2 -> (z:x:y:zs) where
    (x,y,ys) = pop2 s
    (z,zs) = pop ys
  Pseq a b -> sem b (sem a s)
  Pcond p1 p2 ->
    if (x==True) then sem p1 xs
    else sem p2 xs
    where
      (VB x,xs) = pop s
  Ploop p2 ->
    if (x==True) then xs ---------TO CHANGE
    else xs
    where
      (VB x,xs) = pop s

-- Main function: interpreter

main = do
  input <- getContents
  mapM_ print $ sem (read input) []

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
