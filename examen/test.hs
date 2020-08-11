import Data.List
import Data.Char
import Data.Maybe

{- 
În acest exercițiu urmărim crearea unui index de cuvinte dintr-un text: pentru fiecare cuvânt, vom avea o listă cu liniile din text pe care acest apare. În acest scop definim următoarele alias-uri de tip.
-}

type Text = String
type Pair = (String, Int)			-- pereche între un cuvânt și o linie din text
type Index = [(String, [Int])]   -- pereche între un cuvânt și lista tuturor liniilor pe care acesta apare

{-
1. (10p)
Un text constă într-un string de cuvinte care pot fi separate prin spații sau enter (caracterul '\n'). De asemenea, în text pot apărea semne de punctuație precum virgulă, punct, semnul întrebării sau semnul exclamării (după care apare minim un spațiu sau enter).
Să se implementeze funcția textToLines care primește un text și întoarce o listă de perechi de forma 
		(linie, text_aflat_pe acea_linie_procesat).
Un text procesat reprezintă textul din care s-au eliminat semnele de punctuație iar literele mari au fost transformate în litere mici.

Utile: 
lines :: String -> [String]	-- împarte un text în linii, parsând după '\n'
toLower :: Char -> Char			-- transformă o literă mare în literă mică

ex: 
*Main> textToLines "A venit,\na venit\ntoamna!"
[(1,"a venit"),(2,"a venit"),(3,"toamna")]
-}

textToLines :: Text -> [(Int, Text)]
textToLines text = zipWith func [1..len] textProc1
	where
		semne = [',', '.', '?', '!']
		textProc1 = lines $ map toLower textProc
		textProc = filter (\c -> not $ c `elem` semne) text
		len = length textProc
		func x y = (x, y)
	

{-
2. (10p)
Să se implementeze funcția insPair care inserează o pereche (cuvant, linie) într-un index, fără a crea linii duplicate.

ex:
*Main> insPair ("toamna",2) [("a", [1,2])]
[("toamna",[2]),("a",[1,2])]
*Main> insPair ("toamna",2) [("a", [1,2]), ("toamna", [1])]
[("toamna",[2,1]),("a",[1,2])]
*Main> insPair ("toamna",2) [("a", [1,2]), ("toamna", [1,2])]
[("a",[1,2]),("toamna",[1,2])]											-- nu s-a mai adăugat încă o dată linia 2
-}

insPair :: Pair -> Index -> Index
insPair = undefined
{-
3. (10p)
Să se implementeze funcțiile allPairs și textToIndex. 
allPairs primește un text și creează toate perechile (cuvânt, linie) pentru acel text.
textToIndex primește un text și creează indexul (sortat alfabetic după cuvânt) pentru acel text.

Utile: words :: String -> [String]	-- împarte un text în cuvinte, parsând după spații
Obs: tuplurile sunt ordonabile în Haskell (după primul element din tuplu, apoi al doilea, etc).

ex:
*Main> allPairs "A venit,\na venit\ntoamna!"
[("a",1),("venit",1),("a",2),("venit",2),("toamna",3)]
*Main> textToIndex "A venit,\na venit\ntoamna!"
[("a",[1,2]),("toamna",[3]),("venit",[1,2])]
-}
allPairs :: Text -> [Pair]
allPairs = undefined
										 
textToIndex :: Text -> Index
textToIndex = undefined

instance (Ord a) => Ord [a] where
	l1 <= l2 = last $ sort(l1) <= head $ sort(l2)