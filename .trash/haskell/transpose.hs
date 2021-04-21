transpose :: [[a]] -> [[a]]
transpose [] = []
transpose ([]:xss) = transpose xss
transpose ((x:xs):xss) =
  (x : [h | (h:_) <- xss]) : transpose (xs : [t | (_:t) <- xss])

-- x x    x y z
-- y y -> x y z
-- z z
