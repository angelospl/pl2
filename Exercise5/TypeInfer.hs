import Data.Char
import System.IO
import Text.Read

data Type  =  Tvar Int | Tfun Type Type                        deriving Eq
data Expr  =  Evar String | Eabs String Expr | Eapp Expr Expr  deriving Eq

-- Pretty printing of expressions

always = True    -- False omits parentheses whenever possible

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

-- Main program

readOne  =  do  s <- getLine
                let e = read s :: Expr
                putStrLn ("Parsed: " ++ show e)
                print $ readExpr e
                print $ unify (snd (readExpr e))

count n m  =  sequence $ take n $ repeat m

--diavazei mia ekfrasi expr kai gyrnaei mia toupla me ton typo tis expr
--ka8ws kai mia lista me tous periorismous (touples typwn)
readExpr::Expr->(Type,[(Type,Type)])
readExpr expr = (retyp,retconstr)
  where helper::Expr->(String->Int)->Int->[(Type,Type)]->(Type,Int,[(Type,Type)])
        --an i ekfrasi einai evar an emfanizetai prwti fora lamvanei neo typo
        --kai au3anei to pli8os ton diaforetikwn typwn (cnt + 1)
        --den pros8etei kapoion periorismo.
        --ousiastika einai o prwtos kanonas a la curry
        helper (Evar x) f cnt constr =
          if (f x == -1 ) then (Tvar cnt,cnt+1,constr)
          else (Tvar (f x),cnt,constr)

        --an i ekfrasi einai synarthsh var->expr kalei tin helper anadromika
        --gia na vrei ton typo tis ekfrasis ka8ws kai tous neous periorismous
        --oi typoi au3anontai kata enan ka8ws prosti8etai o typos epistrofis tis sinartisis
        --o typos tis 8a einai typos var -> typos expr. ousiastika einai o 2os kanonas a la curry
        helper (Eabs var expr) f cnt constr =
          let
            nfun = \x -> if x == var then cnt else fun x
            (typ,ncnt,nconstr) = helper expr nfun (cnt+1) constr
          in
            (Tfun (Tvar cnt) typ,ncnt,nconstr)
        --an exoume mia ekfrasi pou einai expr1 expr2 kaloume tin helper stin expr1
        --gia na paroume ton typo ths kai tous periorismous klp
        --epeita tin 3anakaloume se auta pou gyrizei gia tin expr2
        --exoume tin voi8itiki synarthsh extr pou tin kaloume gia ton typo epistrofis tis expr1
        --an einai synarthsh t1->t2 tote o typos epistrofis einai t2 kai pros8etoume kai pros8etoume ton periorismo
        --oti t2 = tp1 (tp1 o typos tis expr1)
        --an i expr1 einai tis morfis Tvar x tote o typos epistrofis einai kainourios typos ntyp2
        --kai pros8etoume tous periorismous oti to type x = ntyp1->ntyp2 kai oti ntyp1 = tp1
        --tritos kanonas a la curry
        helper (Eapp expr1 expr2) f cnt constr =
          let
            (tp1,cnt1,constr1) = helper expr1 f cnt constr
            (tp2,cnt2,constr2) = helper expr2 f cnt1 constr1

            extr (Tfun t1 t2) = (t2,cnt2,((t1,tp2):constr2))
            extr (Tvar x) =
              let
                ntyp1 = Tvar cnt2
                ntyp2 = Tvar (cnt2+1)
                nfuntyp = Tfun ntyp1 ntyp2
              in
                (ntyp2,cnt2+2,((Tvar x,nfuntyp):(ntyp1,tp1):constr2))
          in
            extr tp1
        fun x = -1
        (retyp,_,retconstr) = helper expr fun 0 []

inside::Type->Type->Bool
typ `inside` (Tvar a) = typ == (Tvar a)
typ `inside` (Tfun a b) =
  (typ `inside` a || typ `inside` b)

unify::[(Type,Type)]->Bool
unify [] = True
unify constr@((t1,t2):ts)
  | t1 == t2 = unify ts
  | not (matchFun t1) && (matchFun t2) &&  not (t1 `inside` t2) = True && unify (map (replace t1 t2) constr)
  | not (matchFun t2) && (matchFun t1) && not (t2 `inside` t1) = True && unify (map (replace t2 t1) constr)
  | matchFun t1 && matchFun t2 =
    let
      (nt1,nt2) = retFun t1
      (ft1,ft2) = retFun t2
    in
      True && unify ((nt1,ft1):(nt2,ft2):constr)
  | otherwise = False

matchFun::Type->Bool
matchFun (Tvar _) = False
matchFun (Tfun _ _) = True

retFun::Type->(Type,Type)
retFun (Tfun t1 t2) = (t1,t2)

replace::Type->Type->(Type,Type)->(Type,Type)
replace a repl (t1,t2)= (helprepl t1 a repl,helprepl t2 a repl)
  where helprepl::Type->Type->Type->Type
        helprepl (Tvar x) a repl =
          if (Tvar x) == a then repl
          else Tvar x
        helprepl (Tfun x y) a repl =
          if (Tfun x y) == a then repl
          else Tfun (helprepl x a repl) (helprepl y a repl)

main     =  do  n <- readLn
                count n readOne
