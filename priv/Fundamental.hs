module Fundamental where

--
-- Erik Palmgren
--
-- Theorem. For a finite, connected simplicial complex K
--          its fundamental group is isomorphic to the finitely
--          presented group G(K,L), where L is a simply
--          connected subcomplex of K which contains all
--          all vertices of K.
--

import Data.List (sortBy)

-- Fundamental> sgp (-1) (namer ["a","b"]) kleinBottle
--  "<a, b | baab=1>"

type Eqtest a             = a -> a -> Bool

filteraway :: (v -> Bool) -> [v] ->[v]
filteraway p [] = []
filteraway p (w:ws) | (p w)     = filteraway p ws
                   | not (p w) = (w : filteraway p ws)

deleteElt :: v -> (Eqtest v) -> [v] -> [v] 
deleteElt v e ws = filteraway (\y -> (e v y)) ws

deleteDup :: [v] -> (Eqtest v) -> [v]
deleteDup [] e =[]
deleteDup (w:ws) e = (w: deleteDup (deleteElt w e ws) e)

member :: v-> (Eqtest v) -> [v] -> Bool
member v e [] = False
member v e (x:xs) = if (e v x) then True else (member v e xs) 

entrynumber :: v -> (Eqtest v) -> [v] -> Int
entrynumber v e [] = 1
entrynumber v e (x:xs) = if (e v x) then 1 else (1 + (entrynumber v e xs))

data Result v = No | Value v

find :: (v -> Bool) -> [v] -> (Result v)
find p [] =  No
find p (a:as) = if (p a) then (Value a) else (find p as)

data Symbol v = G v | I v

inv :: (Symbol v) -> (Symbol v)
inv (G v) = I v
inv (I v) = G v

type GWord v = [Symbol v]

occ :: v -> (Eqtest v) -> (GWord v) -> Bool
occ v e [] = False
occ v e ((G u):ws) = if (e v u) then True else (occ v e ws)
occ v e ((I u):ws) = if (e v u) then True else (occ v e ws)

mul :: (GWord v) -> (GWord v) -> (GWord v)
mul x y = x ++ y

invert :: (GWord v) -> (GWord v)
invert [] = []
invert (a:as) = (invert as) ++ [inv a]

eqSymbol :: (Eqtest v) -> (Symbol v) -> (Symbol v) -> Bool
eqSymbol e (G a) (G b) = (e a b)
eqSymbol e (G a) (I b) = False
eqSymbol e (I a) (G b) = False
eqSymbol e (I a) (I b) = (e a b)

eqGWord :: (Eqtest v) -> (GWord v) -> (GWord v) -> Bool
eqGWord e []     []     = True
eqGWord e []     (b:bs) = False
eqGWord e (a:as) []     = False
eqGWord e (a:as) (b:bs) = if (eqSymbol e a b) then (eqGWord e as bs) else False

eqRelations :: (Eqtest v) -> [GWord v] -> [GWord v] -> Bool
eqRelations e []     []     = True
eqRelations e []     (b:bs) = False
eqRelations e (a:as) []     = False
eqRelations e (a:as) (b:bs) = if (eqGWord e a b) then (eqRelations e as bs) else False

reduce :: (Eqtest v) -> (GWord v) -> (GWord v)
reduce e w = reverse (red2 e [] w)

red2 ::  (Eqtest v) -> (GWord v) -> (GWord v) -> (GWord v)
red2 e []     []     = []
red2 e []     (b:bs) = red2 e [b] bs
red2 e (a:as) (b:bs) = if (inverses e a b) then (red2 e as bs) 
                                           else (red2 e (b:(a:as)) bs)
red2 e x      []     = x

inverses :: (Eqtest v) -> (Symbol v) -> (Symbol v) -> Bool
inverses e (G u) (G v) = False
inverses e (G u) (I v) = (e u v)
inverses e (I u) (G v) = (e u v)
inverses e (I u) (I v) = False

subst :: (Eqtest v) -> v -> (GWord v) -> (GWord v) -> (GWord v)
subst e v w [] = []
subst e v w ((G u):as) = if (e v u) then w ++ (subst e v w as)
                                    else (G u): (subst e v w as)
subst e v w ((I u):as) = if (e v u) then (invert w) ++ (subst e v w as)
                                    else (I u): (subst e v w as)


parts :: (GWord v) -> [[GWord v]]
parts [a] = [[[],[a],[]]]
parts (a:as) = [[],[a],as]: (adjoinleft a (parts as))

part1 :: [GWord v] -> (GWord v)
part1 [l,[c],r] = l
part2 :: [GWord v] -> (Symbol v)
part2 [l,[c],r] = c
part3 :: [GWord v] -> (GWord v)
part3 [l,[c],r] = r

adjoinleft a [] = []
adjoinleft a ([l,c,r]:rest) = ([(a:l),c,r]: (adjoinleft a rest))

mayisolate :: (Eqtest v) -> [GWord v] -> Bool
mayisolate e [u,[(G x)],v] = (not (occ x e u)) && (not (occ x e v))
mayisolate e [u,[(I x)],v] = (not (occ x e u)) && (not (occ x e v))
mayisolate e ws = False

isolcandidates :: (Eqtest v) -> (GWord v) -> [[GWord v]]
isolcandidates e w = filter (\x -> mayisolate e x) (parts w)

showGen u = u

showSymbol (G u)     =  showGen u
showSymbol (I u)     =  (showGen u) ++ "^{-1}"

showGWord [] = "1"
showGWord [a] = showSymbol a
showGWord (a: (b: rest)) = (showSymbol a) ++ (showGWord (b:rest))

showRelations []              = ""
showRelations (e :[])         = (showGWord e)++ "=1"
showRelations (e : (f:rest))  = (showGWord e)++ "=1, " ++ (showRelations (f:rest))

showGenerators []             = ""
showGenerators (e:[])         = showGen e
showGenerators (e : (f:rest)) = (showGen e) ++ ", " ++ (showGenerators (f:rest))

data GroupPres v = Grel [v] [GWord v]

generatorlist :: GroupPres v -> [v]
generatorlist (Grel gen rel) = gen

eqLists :: (Eqtest v) -> [v] -> [v] -> Bool
eqLists e []     []     = True
eqLists e []     (b:bs) = False
eqLists e (a:as) []     = False
eqLists e (a:as) (b:bs) = if (e a b) then (eqLists e as bs) else False

eqGroupPres :: (Eqtest v) -> (GroupPres v) -> (GroupPres v) -> Bool
eqGroupPres e (Grel g1 r1) (Grel g2 r2) 
   = (eqLists e  g1 g2) && (eqRelations e r1 r2)

showGroupPres (Grel gen rel) = "<" ++ (showGenerators gen) ++ " | " ++ (showRelations rel) ++ ">"

type Renamer v = ([v] ->Int -> String)

renameGen :: [v] -> (Renamer v) -> (Eqtest v) -> v -> String
renameGen gen f e a = f gen (entrynumber a e gen)

renameGWord :: [v] -> (Renamer v) -> (Eqtest v) -> (GWord v) -> (GWord String)
renameGWord gen f e [] = []
renameGWord gen f e ((G u):as) = ((G (f gen (entrynumber u e gen))): (renameGWord gen f e as))
renameGWord gen f e ((I u):as) = ((I (f gen (entrynumber u e gen))): (renameGWord gen f e as))

renameGroupPres :: (Renamer v) -> (Eqtest v) -> (GroupPres v) -> (GroupPres String)
renameGroupPres f e (Grel gen rel)
   =  Grel (map (\x -> (renameGen gen f e x)) gen)
               (map (\w -> (renameGWord gen f e w)) rel)

namer:: [String] -> [v] -> Int -> String
namer ws gen n = let lth = length ws
                     k = n-1
                 in
                 if (n <= lth) then (ws !! k)
                               else (ws !!(k `mod` lth))
                                     ++ "_{"++ (show (k `div` lth)) ++ "}"


eliminate :: (Eqtest v) -> v -> (GWord v) -> (GroupPres v) -> (GroupPres v)

eliminate e v w (Grel gen rel)
   = (Grel (deleteElt v e gen) (map (\x -> (subst e v w x)) rel))

eliminpossiblewith :: (Eqtest v) -> (GWord v) -> Bool
eliminpossiblewith e [] = False
eliminpossiblewith e w  = not (emptyList (isolcandidates e w))

reduceGroupPres :: (Eqtest v)  -> (GroupPres v) -> (GroupPres v)
reduceGroupPres e (Grel gen rel) 
   = (Grel gen (map (\x -> (reduce e x)) rel))

ordlength :: [v] -> [v] -> Ordering
ordlength x y | ((length x) < (length y))  = LT
              | ((length y) < (length x))  = GT
              | ((length x) == (length y)) = EQ

sorttolength :: [[v]] -> [[v]]
sorttolength x = sortBy ordlength x

sortGroupPres :: (GroupPres v) -> (GroupPres v)
sortGroupPres (Grel gen rel)
   = (Grel gen (sorttolength rel))

emptyList :: [v] -> Bool
emptyList [] = True
emptyList (a:as) = False

deleteEmpty :: (GroupPres v) -> (GroupPres v)
deleteEmpty (Grel gen rel)  = (Grel gen (filteraway emptyList rel))

deleteDupRel :: (Eqtest v) -> (GroupPres v) -> (GroupPres v)
deleteDupRel e (Grel gen rel)
  = (Grel gen (deleteDup rel (\x -> (\y-> eqGWord e x y))))

freeGroupPres :: (GroupPres v) -> Bool
freeGroupPres (Grel gen rel) = emptyList rel

simplify :: Int -> (Eqtest v) -> (GroupPres v) -> (GroupPres v)
simplify n e gr = if (n < 0)
                  then (simplify2 (length (generatorlist gr)) e gr)
                  else (simplify2 n e gr)

simplify2 :: Int -> (Eqtest v) -> (GroupPres v) -> (GroupPres v)
simplify2 0 e gr = gr
simplify2 n e gr = let u = simplify1pass e gr
                   in if (eqGroupPres e u gr) then u
                                              else (simplify (n-1) e u)

simplify1pass :: (Eqtest v) -> (GroupPres v) -> (GroupPres v)
simplify1pass e gr =
  let pre = deleteEmpty (deleteDupRel e (reduceGroupPres e gr))
  in if (freeGroupPres pre) then pre
     else let sorted = sortGroupPres pre
          in (tryEliminate e sorted)

select :: [v] -> v
select (a:as) = a

tryEliminate :: (Eqtest v) -> (GroupPres v) -> (GroupPres v)
tryEliminate e (Grel gen rel) = 
   case (find (\x -> eliminpossiblewith e x) rel) of
        No -> (Grel gen rel)
        (Value w) -> let ws = select (isolcandidates e w)
                         rel2 = deleteElt w (\x -> (\y-> eqGWord e x y)) rel  
                     in 
                     let l = part1 ws
                         c = part2 ws
                         r = part3 ws
                     in case c of 
                        (G u) -> eliminate e u (mul (invert l) (invert r)) (Grel gen rel2) 
                        (I u) -> eliminate e u (mul r l)                   (Grel gen rel2) 

choose1 xs = [[x] | x <- xs]

choose2 :: [v] -> [[v]]
choose2 [] = []
choose2 (x:xs) = let y = choose1 xs
                 in [(x:z) | z <- y] ++ choose2 xs

choose3 :: [v] -> [[v]]
choose3 [] = []
choose3 (x:xs) = let y = choose2 xs
                 in [(x:z) | z <- y] ++ choose3 xs

vertexof          :: v -> (Eqtest v) -> [[v]] -> Bool
vertexof a e []     = False
vertexof a e (x:xs) = if (member a e x)  then True   else (vertexof a e xs)

spanning :: (Eqtest v) -> [[v]] -> [[v]]
spanning e [] = []
spanning e (x:xs) = spanning2 e (length xs) [x] xs
spanning2 :: (Eqtest v) -> Int -> [[v]] -> [[v]] -> [[v]]
spanning2 e 0 t  xs = t
spanning2 e n t  xs = let u = extendtree e t xs
                     in  
                     if (length u == length t) then u
                                               else (spanning2 e (n-1) u xs)

extendtree :: (Eqtest v) -> [[v]] -> [[v]] -> [[v]]
extendtree e t [] = t
extendtree e t ([x1,x2]:xs) = if (xor (vertexof x1 e  t)  (vertexof x2 e  t))
                              then extendtree e ([x1,x2]:t) xs
                              else extendtree e t xs

xor :: Bool -> Bool -> Bool
xor True True  = False
xor True False = True
xor False True = True
xor False False = False

type Vertex = Integer

eqVertex :: Vertex -> Vertex -> Bool
eqVertex = (==)
ordVertex :: Vertex -> Vertex -> Ordering
ordVertex x y | (x < y) = LT
              | (y < x) = GT
              | (x == y) = EQ

type Simplex = [Vertex]
type Scomplex = [Simplex]

eqSimplex :: Simplex -> Simplex -> Bool
eqSimplex = (==)

normalise :: Scomplex -> Scomplex
normalise sc = deleteDup [ (sortBy ordVertex s) | s <- sc] eqSimplex 

simp1 :: Scomplex -> Scomplex
simp1 [] = []
simp1 (s:sc) = (choose2 s) ++ (simp1 sc)

simplices1 :: Scomplex -> Scomplex
simplices1 sc = normalise (simp1 sc)

simp2 :: Scomplex -> Scomplex
simp2 [] = []
simp2 (s:sc) = (choose3 s) ++ (simp2 sc)

simplices2 :: Scomplex -> Scomplex
simplices2 sc = normalise (simp2 sc)

grouppres :: Scomplex -> GroupPres Simplex
grouppres sc = let generators   = simplices1 sc
                   s2   = simplices2 sc
                   tree = spanning (eqVertex) generators
                   relations = [[(G t)] | t <- tree ]
                              ++[[(G (v12 s)), (G (v23 s)), (I (v13 s))] | s <- s2]
               in
               Grel generators relations


v12:: Simplex -> Simplex
v12 [x1,x2,x3] = [x1,x2]
v23:: Simplex -> Simplex
v23 [x1,x2,x3] = [x2,x3]
v13:: Simplex -> Simplex
v13 [x1,x2,x3] = [x1,x3]

std gen n = show (gen !! (n-1))
sgp n naming sc = showGroupPres (renameGroupPres naming eqSimplex (simplify n eqSimplex (grouppres sc)))
sh gr = showGroupPres gr
test1 sc = reduceGroupPres eqSimplex (grouppres sc)
test2 sc = deleteDupRel eqSimplex (reduceGroupPres eqSimplex (grouppres sc))

flatten :: [[v]] -> [v]
flatten []     = []
flatten (a:as) = a ++ (flatten as)

vertexcount :: Scomplex -> Int
vertexcount sc = length (deleteDup (flatten sc) eqVertex)

connected :: Scomplex -> Bool
connected sc = let tree = spanning (eqVertex) (simplices1 sc)
                in (vertexcount sc == vertexcount tree)

--   Klein's bottle
--
--   1 -- 2 -- 3 -- 1
--   |  / |  / |  / |
--   | /  | /  | /  |
--   4 -- 5 -- 6 -- 7
--   |  / |  / |  / |
--   | /  | /  | /  |
--   7 -- 8 -- 9 -- 4
--   |  / |  / |  / |
--   | /  | /  | /  |
--   1 -- 2 -- 3 -- 1
--

kleinBottle :: Scomplex
kleinBottle =[[1,2,4],[2,3,5],[3,1,6],[2,5,4],[3,6,5],[1,7,6],
              [4,5,7],[5,6,8],[6,7,9],[5,8,7],[6,9,8],[7,4,9],
              [7,8,1],[8,9,2],[9,4,3],[8,2,1],[9,3,2],[4,1,3]]

-- A bocquet of four discs
discs = [[1,2,3],[1,4,5],[1,6,7],[1,8,9]]
circles = simplices1 discs

--   projective plane
--
--   1 -- 2 -- 3 -- 4
--   |  / |  / |  / |
--   | /  | /  | /  |
--   5 -- 6 -- 7 -- 8
--   |  / |  / |  / |
--   | /  | /  | /  |
--   8 -- 10 --11-- 5
--   |  / |  / |  / |
--   | /  | /  | /  |
--   4 -- 3 -- 2 -- 1
--

projective = [[1,2,5],[2,5,6],[2,3,6],[3,6,7],[3,4,7],[4,7,8],
              [5,6,8],[6,8,10],[6,7,10],[7,10,11],[7,8,11],
              [5,8,11],[4,8,10],[3,4,10],[3,10,11],[2,3,11],[2,5,11],[1,2,5]]


--   torus
--
--   1 -- 2 -- 3 -- 1
--   |  / |  / |  / |
--   | /  | /  | /  |
--   4 -- 5 -- 6 -- 4
--   |  / |  / |  / |
--   | /  | /  | /  |
--   7 -- 8 -- 9 -- 7
--   |  / |  / |  / |
--   | /  | /  | /  |
--   1 -- 2 -- 3 -- 1
--

torus = [[1,2,4],[2,4,5],[2,3,5],[3,5,6],[1,3,6],[1,4,6],[4,5,7],
         [5,7,8],[5,6,8],[6,8,9],[4,6,9],[4,7,9],[1,7,8],[1,2,8],
         [2,8,9],[2,3,9],[3,7,9],[1,3,7]]


