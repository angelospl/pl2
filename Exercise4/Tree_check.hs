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


--ta dentra diathroun to ypsos tous meta apo tin
--efarmogi tis f panw tous
maintainHeightProp::(Tree a->Tree (a,Int))->Tree a->Bool
maintainHeightProp f t= sizeTree t == sizeTree (f t)

--h riza twn dentrwn exei panta ton ari8mo 1
rootNumProp::(Tree a->Tree (a,Int))->Tree a->Bool
rootNumProp f t=
  let t' = f t in
  case (trimTree t' 0) of
    Node (_,x) _ -> x == 1

--to ypsos twn dentrwn einai panta mikrotero h iso apo to mege8os tous
heightSizeProp::(Tree a->Tree (a,Int))->Tree a->Bool
heightSizeProp f t= heightTree (f t) <= sizeTree (f t)

--idiotita symfwna me tin opoia to merged tree
--prepei na exei to idio ypsos me to megisto ypsos
--twn 2 dentrwn pou kanoume merge
mergeProp::((Int->Int->Int)->Tree Int->Tree Int->Tree Int)->(Int->Int->Int)->Tree Int->Tree Int->Bool
mergeProp test_f f t1 t2= m == (maximum [h1,h2])
  where h1 = heightTree t1
        h2 = heightTree t2
        m = heightTree (test_f f t1 t2)

--ka8e shrinked tree pou parage i shrink prepei na
--exei mege8os mikrotero (ari8mo komvwn) apo to arxiko tree
shrinkProp::Tree Int->Bool
shrinkProp t = helper heightList
  where heightList = map sizeTree (shrink t)
        h1 = sizeTree t
        helper::[Int]->Bool
        helper []= True
        helper (x:xs)= (x < h1) && helper xs

testProp::(Testable prop)=>prop->IO ()
testProp x=(quickCheckWith stdArgs {maxSuccess=100,maxShrinks=10} x)

testRoot::(Tree Int->Tree (Int,Int))->IO ()
testRoot f= testProp (rootNumProp f::Tree Int->Bool)

testHeight::(Tree Int-> Tree (Int,Int))-> IO ()
testHeight f= testProp (heightSizeProp f)

testMaintain::(Tree Int->Tree (Int,Int))-> IO ()
testMaintain f = testProp (maintainHeightProp f)

testMerge::(Int->Int->Int)->IO ()
testMerge f = testProp (mergeProp merge f)

testWrong::(Int->Int->Int)->IO ()
testWrong f = testProp (mergeProp wrong f)

testShrink::IO ()
testShrink = testProp shrinkProp

main :: IO ()
main = do
  testRoot dfn
  testRoot bfn
  testHeight dfn
  testHeight bfn
  testMaintain dfn
  testMaintain bfn
  testMerge (+)
  testWrong (+)
  testShrink
