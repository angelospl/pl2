Στο .zip αρχείο έχουμε όλα τα απαραίτητα source files Haskell καθώς και το .cabal.

Για να κάνουμε compile τρέχουμε:
$ cabal v2-build Exercise6.cabal

Δημιουργεί έναν φάκελο dist-newstyle που μέσα έχει τα απαραίτητα αρχεία.
Η μεταγλώττιση γίνεται για 4 πυρήνες αν θέλουμε να το αλλάξουμε αλλάζουμε την επιλογή "-with-rtsopts=-N4".
Αν θέλουμε να μην παράγονται eventlog αρχεία για το τρέξιμο των εκτελέσιμων αλλάζουμε τη σημαία -eventlog.

Για το τρέξιμο:
  - σειριακό : $ cabal v2-run serial < inputfile
  - παράλληλο : $ cabal v2-run parallel < inputfile
  - ταυτόχρονο : $ cabal v2-run concurrent < inputfile

Ο φάκελος επίσης περιέχει την αναφορά καθώς και δύο testcases το inputeven.in και inputuneven.in.
Τα eventlog τα βλέπουμε με το πρόγραμμα threadscope.
