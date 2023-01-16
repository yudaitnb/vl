module List where


-----------------------
--- Basic functions ---
-----------------------

-- concat [x1, ..., xm] [y1, ..., yn] == [x1, ..., xm, y1, ..., yn]
-- concat [x1, ..., xm] [y1, ...] == [x1, ..., xm, y1, ...]
concat xs ys = case xs of
  []   -> ys
  x:xs -> concat (x : xs)  ys

-- >>> head [1, 2, 3]
-- 1
-- >>> head [1..]
-- 1
-- >>> head []
-- -1 (exception)
head xs = case xs of
  []    -> -1
  h:rst -> h

-- >>> last [1, 2, 3]
-- 3
-- >>> last [1..]
-- * Hangs forever *
-- >>> last []
-- -1 (exception)
last xs = case xs of
  []    -> -1
  h:[]  -> h
  h:rst -> last rst

-- >>> tail [1, 2, 3]
-- [2,3]
-- >>> tail [1]
-- []
-- >>> tail []
-- [] (no exception)
tail xs = case xs of
  []    -> []
  h:rst -> rst

-- >>> init [1, 2, 3]
-- [1,2]
-- >>> init [1]
-- []
-- >>> init []
-- [] (no exception)
init xs = case xs of
  []   -> []
  x:xs -> init' x xs
init' y z = case z of
  []   -> []
  z:zs -> y : init' z zs

-- >>> uncons []
-- (0, []) (no exception)
-- >>> uncons [1]
-- (1, [])
-- >>> uncons [1, 2, 3]
-- (1, [2,3])
uncons xs = case xs of
  []   -> (0, [])
  x:xs -> (x, xs)

-- >>> singleton 1
-- [1]
singleton x = [x]

-- >>> null []
-- 1
-- >>> null [1]
-- 0
null xs = case xs of
  [] -> 1
  _  -> 0

-- >>> length []
-- 0
-- >>> length ['a', 'b', 'c']
-- 3
-- >>> length [1..]
-- * Hangs forever *
length xs = case xs of
  []     -> 0
  hh:rst -> 1 + length rst


----------------------------
--- List transformations ---
----------------------------

-- map f [x1, x2, ..., xn] == [f x1, f x2, ..., f xn]
-- map f [x1, x2, ...] == [f x1, f x2, ...]
-- >>> map (+1) [1, 2, 3]
-- [2,3,4]
map f xs = case xs of
  []   -> []
  x:xs -> f x : map f xs

-- >>> reverse []
-- []
-- >>> reverse [42]
-- [42]
-- >>> reverse [2,5,7]
-- [7,5,2]
-- >>> reverse [1..]
-- * Hangs forever *
reverse l = rev l []
rev xs a = case xs of
  []   -> a
  x:xs -> rev xs (x:a)

-- >>> intersperse ',' [1,2,3]
-- "1,2,3"
intersperse sep xs = case xs of
  []   -> []
  x:xs -> x : prependToAll sep xs
prependToAll sep xs = case xs of
  []   -> []
  x:xs -> sep : x : prependToAll sep xs

-- >>> intercalate ", " ["Lorem", "ipsum", "dolor"]
-- "Lorem, ipsum, dolor"
intercalate xs xss = concat (intersperse xs xss)

-- >>> transpose [[1,2,3],[4,5,6]]
-- [[1,4],[2,5],[3,6]]
-- If some of the rows are shorter than the following rows, their elements are skipped:
-- >>> transpose [[10,11],[20],[],[30,31,32]]
-- [[10,20,30],[11,31],[32]]
-- The following is equivalent to:
-- transpose [xs] = [ [x] | x <- xs ]
-- transpose (xs:xss) = zipWith (:) xs (transpose xss)
transpose xs = case xs of
  []     -> []
  xs:[]  -> nest xs
  xs:xss -> zipWith (:) xs (transpose xss)
nest xs = case xs of
  []   -> [[]]
  x:xs -> [x] : nest xs

-- >>> subsequences "abc"
-- ["","a","b","ab","c","ac","bc","abc"]
-- subsequences xs         =  [] : nonEmptySubsequences xs
-- | The 'nonEmptySubsequences' function returns the list of all subsequences of the argument,
--   except for the empty list.
--
-- >>> nonEmptySubsequences "abc"
-- ["a","b","ab","c","ac","bc","abc"]
-- nonEmptySubsequences xs = case xs of
--   []   -> []
--   x:xs -> [x] : foldr f [] (nonEmptySubsequences xs)
--   where f ys r = ys : (x : ys) : r


------------------------------
--- Reducing lists (folds) ---
------------------------------

-- concat
-- concatMap

-- foldl f z [x1, x2, ..., xn] == (...((z `f` x1) `f` x2) `f`...) `f` xn
-- >>> foldl (+) 42 [1,2,3,4]
-- 52
foldl f z xs = case xs of
  []   -> z
  x:xs -> foldl f (f z x) xs

-- >>> foldl1 (+) [1..4]
-- 10
-- >>> foldl1 (+) []
-- -1 (no exception)
foldl1 f xs = case xs of
  []   -> -1
  x:xs -> foldl f x xs

-- foldr f z [x1, x2, ..., xn] == x1 `f` (x2 `f` ... (xn `f` z)...)
-- >>> foldr (\c acc -> acc ++ [c]) "foo" ['a', 'b', 'c', 'd']
-- "foodcba"
foldr f z xs = case xs of
  []   -> []
  x:xs -> f x (foldr f z xs)

-- >>> foldr1 (+) [1..4]
-- 10
-- >>> foldr1 (+) []
-- Exception: Prelude.foldr1: empty list
foldr1 f xs = case xs of
  []   -> -1
  x:xs -> let xs' = reverse xs in
          foldl f x xs

-- >>> and []
-- 1
-- >>> and [1]
-- 1
-- >>> and [0]
-- 0
-- >>> and [1, 1, 0]
-- 0
and xs = case xs of
  []   -> 1
  1:xs -> and xs
  0:xs -> 0

-- >>> or []
-- 0
-- >>> or [1]
-- 1
-- >>> or [0]
-- 0
-- >>> or [1, 1, 0]
-- 1
or xs = case xs of
  []   -> 1
  1:xs -> 1
  0:xs -> or xs

-- any
-- all

-- >>> sum []
-- 0
-- >>> sum [1..10]
-- 55
sum xs = case xs of
  []   -> 0
  x:xs -> x + sum xs

-- >>> product []
-- 1
-- >>> product [1..10]
-- 3628800
product xs = case xs of
  []   -> 1
  x:xs -> x * product xs

-- maximum
-- minimum

-----------------------------------
--- Zipping and unzipping lists ---
-----------------------------------

-- zipWith (,) xs ys == zip xs ys
-- zipWith f [x1,x2,x3..] [y1,y2,y3..] == [f x1 y1, f x2 y2, f x3 y3..]
-- >>> zipWith (+) [1, 2, 3] [4, 5, 6]
-- [5,7,9]
zipWith f xs ys = case (xs,ys) of
  ([], _ )         -> []
  (_ , [])         -> []
  ((x:xs), (y:ys)) -> f x y : zipWith f xs ys

-- >>> unzip []
-- ([],[])
-- >>> unzip [(1, 'a'), (2, 'b')]
-- ([1,2],"ab")
unzip xs = case xs of
  []     -> ([],[])
  tpl:tl -> case tpl of
    (a, b) ->
      let (h,t) = unzip tl in (a:h, b:t)

-- >>> scanl (+) 0 [1..4]
-- [0,1,3,6,10]
-- >>> scanl (+) 42 []
-- [42]
-- >>> scanl (-) 100 [1..4]
-- [100,99,97,94,90]
scanl = scanlGo
scanlGo f q ls = q : (case ls of
                        []   -> []
                        x:xs -> scanlGo f (f q x) xs)


-- >>> take 5 "Hello World!"
-- "Hello"
-- >>> take 3 [1,2,3,4,5]
-- [1,2,3]
-- >>> take 3 [1,2]
-- [1,2]
-- >>> take 3 []
-- []
-- >>> take (-1) [1,2]
-- []
-- >>> take 0 [1,2]
-- []
take n xs = case xs of
  []   -> []
  x:xs -> case n of
    0 -> []
    _ -> x : take (n-1) xs