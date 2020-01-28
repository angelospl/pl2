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

count n m  =  sequence $ take n $ repeat m

readExpr::Expr->(Type,[(Type,Type)])
readExpr expr =
  where helper (Evar x) cnt constr=
          if (givetype x == - 1) then (Tvar cnt,cnt+1,constr)
          else (Tvar (givetype x),cnt,constr)

--afora ton prwto kanona tou curry
--epistrefei ton elegxo typ == tvar a
--alliws an einai synarthsh a->b elegxei anadromika ean
--einai typ == a kai typ==b. ola ginontai sto perivallon G pou 8ewritika
--mas dinetai
typeinenvironment::Type->Type->Bool
typeinevironment typ (Tvar a) = typ == (Tvar a)
typeinenvironment typ (Tfun a b) =
  (typeenvironment typ a || typeinenvironment typ b)

--pairnei ousiastika typous typ1->typ2 se ena perivallon constrlst (lista)
--gyrnaei ti lista me tous periorismous efarmozontas tin synarthsh helper
--
functype::Type->Type->[(Type,Type)]->[(Type,Type)]
functype _ _ []=[]
functype typ1 typ2 ((t1,t2):constrlst) = (helper typ1 typ2 t1,helper typ1 typ2 t2)):(functype typ1 typ2 constrlst)
  where helper::Type->Type->Type->Type
        helper a b (Tvar c) =
          if a== (Tvar c) then b
          else Tvar c
        helper a b (Tfun c d)=
          if a == (Tfun c d) then b
          else Tfun (helper a b c) (helper a b d)


main     =  do  n <- readLn
                count n readOne
