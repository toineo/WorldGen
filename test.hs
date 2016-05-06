import Data.List (intersperse)

-- 3D objects
type Coord = (Int, Int, Int)

data Mesh = Cube Coord Coord deriving (Show)

printCoord :: Coord -> String
printCoord (x, y, z) = showString "v " . concat . intersperse " " . map show $ [x, y, z]

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



-- Generators
genWorld :: Int -> Int -> [Mesh]
genWorld n m =
  [ Cube (0, 0, -1) (n, m, 0) -- World support
    ]


main = do
  putStrLn $ printMeshList $ genWorld 100 100
  -- putStrLn $ printMeshList [Cube (0, 0, 0) (1, 1, 1), Cube (1, 1, 1) (3, 3, 3)]


-- Misc
-- What's the nicest point-free way to write this?
mapTuple4 :: (a -> b) -> (a, a, a, a) -> (b, b, b, b)
mapTuple4 f (x, y, z, w) = (f x, f y, f z, f w)