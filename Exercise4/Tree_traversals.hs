module Tree_traversals where

data Tree a= Node a [Tree a]
  deriving Show

dfn::Tree a -> Tree(a,Int)
dfn t=fst (aux 1 t)
  where aux::Int->Tree a->(Tree (a,Int),Int)
        aux k (Node x ts)= (Node (x,k) ts',k')
          where (ts',k') = auxs (k+1) ts
        auxs::Int->[Tree a]->([Tree (a,Int)],Int)
        auxs k [] = ([],k)
        auxs k (t:ts) = (t':ts',k'')
          where (t',k')=aux k t
                (ts',k'')=auxs k' ts

bfn::Tree a -> Tree(a,Int)
bfn t=t'
  where (t',ks') = aux ks t
        ks = 1:ks'
        aux::[Int]->Tree a->(Tree (a,Int),[Int])
        aux (k:ks) (Node x ts)= (Node (x,k) ts',k+1:ks')
          where (ts',ks')= auxs ks ts
        auxs::[Int]->[Tree a]->([Tree (a,Int)],[Int])
        auxs ks []=([],ks)
        auxs ks (t:ts)=(t':ts',ks'')
          where (t',ks')=aux ks t
                (ts',ks'')= auxs ks' ts


--Examples
t1 = Node 1 [ Node 2 [ Node 3 []
                   , Node 4 []
                   ]
          , Node 5 [ Node 6 [] ]
          ]

t2 = Node 'a' [ Node 'b' []
            , Node 'c' [ Node 'e' []
                       , Node 'f' []
                       ]
            , Node 'd' []
            ]
