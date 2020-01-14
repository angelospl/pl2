all:tree tree_check

tree: Tree_traversals.hs
		ghc Tree_traversals.hs -O

tree_check: Tree_check.hs Tree_traversals.hi
	ghc Tree_check.hs -O -o tree_check

clean:
	rm *.o *.hi tree tree_check
