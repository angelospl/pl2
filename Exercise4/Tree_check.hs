import Tree_traversals
import Test.QuickCheck


--xekinaei me enan megalo ari8mo m
--pairnei enan arbitrary t
--kai epilegei me ti 8a diairesei to m anamesa sto 2 kai sto 4
--n einai ta paidia tou komvou apo 0 mexri m/2 (stin xeiroteri an divider=1)
--to gegonos oti diairoume mas dinei vevaiotita oti 8a termatisei kai 8a
--ftiaxnei ena sxetika mikro dentro
instance Arbitrary a => Arbitrary (Tree a) where
  arbitrary=
   sized arbitrarySizedTree where
        arbitrarySizedTree::Arbitrary a=>Int->Gen (Tree a)
        arbitrarySizedTree m=do
          t<-arbitrary
          divider <- choose (2,3)
          n <- choose (0, m `div` divider)
          ts <- vectorOf n (arbitrarySizedTree (m `div` divider))
          return (Node t ts)
  shrink (Node a [])=[]
  shrink (Node a (x:xs))=
     --(x:xs)++     --afairw ton idio ton komvo --xwris afairesi komvou nmz kalytera
    [Node a lista' | lista' <- shrink (x:xs)] --afairw ypodentra


foldTree::(a->[b]->b)->Tree a->b
foldTree f (Node x lst)=f x (map (foldTree f) lst)

--metraei tous komvous tou dentrou
nodeNum::Tree a->Int
nodeNum t=length (foldTree (\x -> \lst -> (x:(foldl (++) [] lst))) t)

--koureuei to dentro gia ypsos n. An valoume n=0 pairnoume ti riza
trimTree::Tree a->Int->Tree a
trimTree t n= trim_help 0 t
    where
      trim_help d (Node a lst)=
        if d==n then (Node a [])
          else Node a (map (trim_help (d+1)) lst)

main :: IO ()
main = do
  x<-generate arbitrary::IO (Tree Int)
  print $ x
  print $ nodeNum x
  print $ shrink x
  print $ trimTree x 0
