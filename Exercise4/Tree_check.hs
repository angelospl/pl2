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


main :: IO ()
main = do
  x<-generate arbitrary::IO (Tree Int)
  print $ x
