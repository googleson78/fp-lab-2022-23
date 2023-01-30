module Lib where
import Data.Char

type Dict a b = [(a, b)]

data Tree a = Null | Node a (Tree a) (Tree a)
    deriving (Eq, Show)

makeTree :: String -> Tree Int
makeTree "" = Null
makeTree (c0:r) = 
  if c0 == 'N' 
  then 
    (if (head r) == 'u' 
    then Null 
    else (Node (getValue r 0) 
    (makeTree (left r)) 
    (makeTree (right (tail (rest r)) 1)))) 
  else (makeTree r)
  where
    getValue "" _ = error "Invalid string!"
    getValue (c:cs) res = 
      if (isDigit c) 
        then getValue cs (10 * res + (digitToInt c)) 
        else 
          (if res > 0 
          then res 
          else getValue cs res)
    
    helper :: String -> Int -> String -> String
    helper "" _ res = reverse res
    helper _ 0 res = reverse res
    helper (c:cs) n res = 
      if c == '(' 
        then helper cs (n + 1) (c:res) 
        else 
          (if c == ')' 
          then helper cs (n - 1) (c:res) 
          else  helper cs n (c:res))
    
    rest "" = error "Invalid string!"
    rest (c:cs) = 
      if c == '(' 
        then (c:cs) 
        else 
          (if c == 'N' && (head cs) == 'u' 
          then c:cs 
          else rest cs)
    
    left s = 
      if (take 4 (rest s)) == "Null" 
        then "Null" 
        else '(':helper (tail (rest s)) 1 ""
    
    right :: String -> Int -> String
    right "" _ = ""
    right (c:cs) n = 
      if c == '(' 
        then 
          (if n == 0 
          then c:cs 
          else right cs (n + 1)) 
        else 
          (if c == ')' 
          then right cs (n - 1) 
          else 
            (if c == 'N' && n == 0 && (head cs) == 'u' 
            then "Null" 
            else right cs n))


makeTreeAndLeaves :: String -> (Tree Int, String)
makeTreeAndLeaves s = (makeTree (helperTree s ""), (tail (init (helperLeaves (init s)))))
  where
    helperTree "" res = res
    helperTree (c:cs) res = 
      if c == ',' 
        then reverse (')':res) 
        else helperTree cs (c:res)
    
    helperLeaves "" = ""
    helperLeaves (c:cs) = 
      if c == ',' 
        then 
          (if cs == "" 
          then error "Invalid string!" 
          else reverse (init (helperTree cs ""))) 
        else helperLeaves cs


main :: IO()
main = do
    word <- readFile "encodeInput.txt"
    print (encode word)
    tree <- readFile "decodeTreeInput.txt"
    bits <- readFile "decodeBitsInput.txt"
    print (decode (makeTreeAndLeaves tree) bits)

in_keys :: Eq a => a -> Dict a b -> Bool
in_keys _ [] = False
in_keys x (y:ys) = if x == (fst y) then True else in_keys x ys


value :: Eq a => a -> Dict a b -> b
value _ [] = error "Key not in dictionary!"
value x (y:ys) = if x == (fst y) then (snd y) else value x ys


increment :: Eq a => a -> Dict a Int -> Dict a Int
increment _ [] = []
increment _el (x:xs) = if _el == (fst x) then (_el, (snd x + 1)):xs else x:(increment _el xs)


histogram :: Eq a => [a] -> Dict a Int
histogram s = helper s []
  where
    helper [] res = res
    helper (c:cs) res = if in_keys c res then helper cs (increment c res) else helper cs ((c, 1):res)


least :: Dict a Int -> a
least [] = error "Empty dictionary!"
least (p:ps) = helper (fst p) ps (snd p)
  where
    helper el [] _ = el
    helper el (x:xs) val = if (snd x) < val then helper (fst x) xs (snd x) else helper el xs val


remove :: Eq a => a -> [a] -> [a]
remove el l = helper el l []
  where
    helper _ [] res = reverse res
    helper x (y:ys) res = if x == y then helper x ys res else helper x ys (y:res)


root :: Tree a -> a
root Null = error "Empty tree!"
root (Node n _ _) = n


hufmanTree :: String -> (Tree Int, String)
hufmanTree "" = (Null, "")
hufmanTree s = helper (remove (least_often s) s) ((Node (value (least_often s) (histogram s)) Null Null), [(least_often s)])
  where
    least_often _str = least (histogram _str)
    helper str res = if str == "" then res else
        helper (remove (least_often str) str) ((Node ((root (fst res)) + (value (least_often str) (histogram str)))
        (Node (value (least_often str) (histogram str)) Null Null) (fst res)), (least_often str):(snd res))


leaf :: Tree a -> Bool
leaf (Node _ Null Null) = True
leaf _ = False


encode_histogram :: String -> Dict Char String
encode_histogram s = helper (fst tree_uniques) "" [] (snd tree_uniques)
  where
    tree_uniques = hufmanTree s
    helper Null _ _ _ = []
    helper (Node _ lt rt) code res at_leaves = if lt == Null && rt == Null then ((head at_leaves), code):res else (left_res ++ right_res)
      where
        left_res = helper lt (code ++ "0") res (if leaf lt then [(head at_leaves)] else (init at_leaves))
        right_res = helper rt (code ++ "1") res (if leaf rt then [(last at_leaves)] else (tail at_leaves))


encode :: String -> String
encode s = helper s
  where
    code_histogram = encode_histogram s
    helper "" = ""
    helper (c:cs) = (value c code_histogram) ++ helper cs


decode :: (Tree Int, String) -> String -> String
decode (tree, at_leaves) code = helper tree code at_leaves ""
  where
    helper Null _ _ _ = ""
    helper (Node _ Null Null) bits _at_leaves res = helper tree bits at_leaves (res ++ _at_leaves)
    helper _ "" _ res = res
    helper (Node _ lt rt) (c:cs) _at_leaves res =
        if c == '0'
            then helper lt cs (if leaf lt
              then [(head _at_leaves)]
              else (init _at_leaves)) res
            else (if c == '1'
                then helper rt cs 
                (if leaf rt
                  then [(last _at_leaves)]
                  else (tail _at_leaves)) res
                else error "Unrecognized symbol!")