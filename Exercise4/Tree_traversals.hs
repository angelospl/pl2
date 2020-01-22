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

foldTree::(a->[b]->b)->Tree a->b
foldTree f (Node x lst)=f x (map (foldTree f) lst)

heightTree ::Tree a -> Int
heightTree t = foldTree help t
    where help _ [] = 1
          help _ cl = 1 + (maximum cl)

--epistrefei lista stoixeiwn tou dentrou
nodesTree::Tree a->[a]
nodesTree t=foldTree (\x -> \lst -> (x:(foldl (++) [] lst))) t

--metraei tous komvous tou dentrou
sizeTree::Tree a->Int
sizeTree t=length $ nodesTree t

--koureuei to dentro gia ypsos n. An valoume n=0 pairnoume ti riza
trimTree::Tree a->Int->Tree a
trimTree t n= trim_help 0 t
    where
      trim_help d (Node a lst)=
        if d==n then (Node a [])
          else Node a (map (trim_help (d+1)) lst)

merge::(a->a->a)->Tree a->Tree a->Tree a
merge f (Node x xs) (Node y [])=Node x xs
merge f (Node x []) (Node y ys)=Node y ys
merge f (Node x xs) (Node y ys)=
  Node (f x y) $ myzipWith (merge f) xs ys

myzipWith :: (a->a->a) -> [a]->[a]->[a]
myzipWith f= go
  where
    go [] x = x
    go x [] = x
    go (x:xs) (y:ys) = f x y : go xs ys


wrong :: (a->a->a)->Tree a->Tree a->Tree a
wrong f (Node x tsx) (Node y tsy) = Node (f x y) $ zipWith (wrong f) tsx tsy


--Examples
t1 = Node 1 [ Node 2 [ Node 4 []]
          , Node 3 [ Node 5 []
                    ,Node 6 [Node 7 []] ]
          ]

t2 = Node 3 [ Node 1 [ Node 5 [ Node 7 []]
                     , Node 6 [ Node 8 []]]
            , Node 2 [Node 4 [ Node 9 []
                             , Node 7 []]]  ]
