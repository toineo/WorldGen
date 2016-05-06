import Data.List (intersperse)
import Control.Arrow
import System.Random

-- TODO: operators module
-- The $ operator with left associativity
infixl 0 $$
($$) :: (a -> b) -> a -> b
f $$ x = f x


-- 3D objects
type Pos = Rational
showPos n = show . fromRational $ n

type Coord = (Pos, Pos, Pos)

data Dir = X | Y | Z deriving (Show)

data Mesh = Cube Coord Coord deriving (Show)

shift :: Dir -> Pos -> Coord -> Coord
shift X n (x, y, z) = (x + n, y, z)
shift Y n (x, y, z) = (x, y + n, z)
shift Z n (x, y, z) = (x, y, z + n)

printCoord :: Coord -> String
printCoord (x, y, z) = showString "v " . concat . intersperse " " . map showPos $ [x, y, z]

-- printFace indeed depends on *Int* parameters: those are references, not coordinates
printFace :: (Int, Int, Int, Int) -> String
printFace (v1, v2, v3, v4) = showString "f " . concat . intersperse " " . map show $ [v1, v2, v3, v4]

printMesh :: Mesh -> Int -> String
printMesh (Cube (x1, y1, z1) (x2, y2, z2)) ofst =
  -- For now, this assumes that coordinates are properly ordered
  (concat . intersperse "\n" . map printCoord $
    [ (x1, y1, z1),
      (x2, y1, z1),
      (x1, y2, z1),
      (x1, y1, z2),
      (x2, y2, z1),
      (x2, y1, z2),
      (x1, y2, z2),
      (x2, y2, z2)])
    ++ "\n" ++
      (concat . intersperse "\n" . map (printFace . mapTuple4 (+ ofst)) $
        [ (1, 2, 5, 3),
          (1, 2, 6, 4),
          (1, 3, 7, 4),
          (2, 5, 8, 6),
          (3, 5, 8, 7),
          (4, 6, 8, 7)
          ])

-- Little temporary hack related to the way .obj files (don't) name vertices
printMeshOffset = (+ 8)

printMeshList :: [Mesh] -> String
printMeshList l = fst (foldl (\(s, n) m -> (s ++ "\n" ++ (printMesh m n), printMeshOffset n)) ("", 0) l)


-- Randomness
type Seed = Int

coord2Gen :: Seed -> Int -> Coord -> StdGen
coord2Gen seed shift = mapTuple3 (fromInteger . floor)  >>> \ (x, y, z) -> mkStdGen $ seed + 2 * x + 3 * y + 5 * z + 7 * shift

-- Internal function
findWeightList _ [] = undefined -- Invalid call: the target is bigger than the sum of the weights
findWeightList tgt ((x, w) : tl)
    | tgt < w = x
    | tgt >= w = findWeightList (tgt - w) tl

-- Select an element in a weighted list at random
selectedWeightList :: [(a, Int)] -> StdGen -> a
selectedWeightList xs gen =
    findWeightList s xs
  where
    total = foldr $$ (+) . snd $$ 0 $$ xs
    -- Ugly to do so (multiple queries), but the values returned by StdGen tend to be biased on the first queries
    (raw_s, gen') = next . snd . next . snd . next $ gen
    -- (raw_s, gen') = next . snd . next . snd . next . snd . next $ gen -- Actually, check how nicely biased this one is!
    s = raw_s `mod` total

-- Generators
-- For now, this is just a quick and dirty solution; the nice, abstract one is to come

-- Functions of this type evaluate their argument (some ground location) and return a weight that represents the likelihood
-- that the associated sort of object "spawns" on this ground. For instance, a house generator has an bigger weight on a
-- small block than on a big one; a tall building generator has the converse (and possibly even 0 when the block is too small)
-- FIXME: for now, we only use the x/y sizes of the block to specify it -- should use more info
type WeightFun = Pos -> Pos -> Int

-- Random generators are often called with an arbitrary shift; this is to avoid having a generator
-- and its subgenerators use the same random sequence (as they are on the same coordinates)
genWorld :: Seed -> Pos -> Pos -> [Mesh]
genWorld s n m =
  Cube (0, 0, -1) (n, m, 0) -- World support
    : genList 0 0
      where
        ep = 1
        subSz = 10
        genList x y
          | x <= n - subSz, y <= m - subSz = genBlock s (x, y, 0) (subSz - ep) (subSz - ep) ++ (genList $$ x + subSz $$ y)
          | x > n - subSz, y <= m - subSz = genList 0 (y + subSz)
          | otherwise = []


-- genRoad

genBlock :: Seed -> Coord -> Pos -> Pos -> [Mesh]
genBlock s root xsz ysz =
    case selectedWeightList ws gen of House1 -> genHouse1 root xsz ysz
                                      Building1 -> genBuildg1 root xsz ysz
    -- [Cube root . shift X xsz . shift Y ysz . shift Z 10 $ root]
  where
      gen = coord2Gen s 0 root
      ws = [(House1, weightHouse1 xsz ysz), (Building1, weightBuildg1 xsz ysz)] -- TODO

-- Building generators
data BuildingType = House1 | Building1 deriving (Show)

-- Simple house
weightHouse1 :: WeightFun
weightHouse1 _ _ = 1 -- TODO

genHouse1 root xsz ysz = [Cube root . shift X xsz . shift Y ysz . shift Z 10 $ root]

-- Simple building
weightBuildg1 :: WeightFun
weightBuildg1 _ _ = 1 -- TODO

genBuildg1 root xsz ysz = [Cube root . shift X xsz . shift Y ysz . shift Z 30 $ root]

main = do
  putStrLn . printMeshList $ genWorld 0 100 100


-- Misc
-- What's the nicest point-free way to write this?
mapTuple3 :: (a -> b) -> (a, a, a) -> (b, b, b)
mapTuple3 f (x, y, z) = (f x, f y, f z)

mapTuple4 :: (a -> b) -> (a, a, a, a) -> (b, b, b, b)
mapTuple4 f (x, y, z, w) = (f x, f y, f z, f w)