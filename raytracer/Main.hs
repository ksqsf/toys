{-

This is a Haskell implementation of "Ray tracing in one weekend".
https://raytracing.github.io/books/RayTracingInOneWeekend.html

The following code seems inefficient...

-}

{-# LANGUAGE Strict #-}
{-# LANGUAGE StrictData #-}
module Main where

import           Control.Lens ((^.))
import           Data.Foldable
import           Data.IORef
import           Data.Massiv.Array as A
import qualified Data.Massiv.Array.IO as A
import           Data.Massiv.Array.IO hiding (Image, Pixel)
import           Data.Maybe
import           Linear.Metric
import           Linear.V3
import           Linear.Vector
import           System.Environment
import           System.Random

type Pixel = A.Pixel (SRGB 'NonLinear) Word8
type Image = Matrix S Pixel

type Pt3 = V3 Double
type Vec3 = V3 Double
type Color' = V3 Double
type Pixel' = Color'
type Image' = Matrix S Pixel'

encodePixel :: V3 Double -> Int -> Pixel
encodePixel (V3 r g b) n_samples =
  let conv = floor . (*256) . clamp1 . sqrt . (/ realToFrac n_samples)
  in PixelSRGB (conv r) (conv g) (conv b)

data Ray = Ray
  { rayOrig :: Pt3
  , rayDir  :: Vec3
  }

at :: Ray -> Double -> Pt3
at Ray{..} t = rayOrig + rayDir ^* t

class Surface a where
  hit :: Ray -> a -> Double -> Double -> Maybe HitRecord

data HitRecord = HitRecord
  { hitPt :: Pt3          -- ^ The intersection point.
  , hitNormal :: Vec3     -- ^ The normal that goes against the ray. Not always pointing outward.
  , hitT :: Double        -- ^ The parameter @t@.
  , hitFrontFace :: Bool  -- ^ Whether the ray hits the front face?
  , hitScatter :: Scatter -- ^ Not used until Test 6.
  }

data Sphere = Sphere
  { sphereCenter :: Pt3
  , sphereRadius :: Double
  , sphereScatter :: Scatter
  }

instance Surface Sphere where
  hit r@Ray{..} Sphere{..} t_min t_max =
    let oc = rayOrig - sphereCenter
        a = rayDir `dot` rayDir
        half_b = oc `dot` rayDir
        c = (oc `dot` oc) - (sphereRadius * sphereRadius)
        discriminant = half_b * half_b - a * c
        sqrtd = sqrt discriminant
        root1 = (-half_b - sqrtd) / a
        root2 = (-half_b + sqrtd) / a
    in if | discriminant < 0                 -> Nothing
          | root1 >= t_min && root1 <= t_max -> makeHitRecord root1
          | root2 >= t_min && root2 <= t_max -> makeHitRecord root2
          | otherwise                        -> Nothing
    where makeHitRecord root =
            let pt = r `at` root
                t = root
                outward = (pt - sphereCenter) ^/ sphereRadius
                (frontFace, normal) = if (rayDir `dot` outward) < 0
                                      then (True, outward)
                                      else (False, negate outward)
            in Just (HitRecord pt normal t frontFace sphereScatter)

data Some k = forall a. k a => Some a

type SurfaceList = [Some Surface]

instance Surface SurfaceList where
  hit r xs t_min t_max = go xs t_min t_max Nothing
    where
      go [] _ _ res = res
      go (Some x : xs) t_min t_max res =
        case hit r x t_min t_max of
          Nothing -> go xs t_min t_max res
          Just hr@HitRecord{hitT} -> go xs t_min hitT (Just hr)

infinity :: Floating a => a
infinity = 1 / 0

-- |Wrap the camera.
data Camera = Camera
  { cOrigin :: Pt3
  , cHorizontal :: Vec3
  , cVertical :: Vec3
  , cLowerLeft :: Pt3
  , cu, cv, cw :: Vec3
  , lensRadius :: Double
  }

-- |Positional cameras.
mkCamera :: Pt3 -> Pt3 -> Vec3 -> Double -> Rational -> Double -> Double -> Camera
mkCamera lookfrom lookat vup vfov aspect_ratio aperture focus_dist =
  let h = tan (vfov / 2)
      cViewportHeight = 2.0 * h :: Double
      cViewportWidth = realToFrac aspect_ratio * cViewportHeight

      w = signorm (lookfrom - lookat)  -- z-axis
      u = signorm (vup `cross` w)      -- x-axis
      v = w `cross` u                  -- y-axis

      cOrigin = lookfrom
      cHorizontal = focus_dist * cViewportWidth *^ u
      cVertical = focus_dist * cViewportHeight *^ v
      cLowerLeft = cOrigin - cHorizontal ^/ 2 - cVertical ^/ 2 - focus_dist *^ w

      (cu,cv,cw) = (u,v,w)
      lensRadius = aperture / 2
  in Camera{..}

clamp :: Ord a => (a, a) -> a -> a
clamp (lo, hi) x
  | x < lo    = lo
  | x > hi    = hi
  | otherwise = x

clamp1 :: Double -> Double
clamp1 = clamp (0.0, 0.999)

randomInUnitSphere :: IO Pt3
randomInUnitSphere =
  let r = randomRIO (0.0, 0.999) :: IO Double
  in do p <- sequence (V3 r r r)
        if distance p (V3 0 0 0) < 1.0
          then return p
          else randomInUnitSphere

randomUnitVector :: IO Vec3
randomUnitVector = signorm <$> randomInUnitSphere

randomInUnitDisk :: IO Vec3
randomInUnitDisk = do
  x <- randomRIO (-1, 1)
  y <- randomRIO (-1, 1)
  let p = V3 x y 0
  if p `dot` p < 1 then return p else randomInUnitDisk

nearZero :: Vec3 -> Bool
nearZero v = v < 1e-8

-- |A uniform interface for ray-object interaction.
type Scatter = Ray -> HitRecord -> IO (Maybe (Color', Ray))

lambertian :: Color' -> Scatter
lambertian albedo _ HitRecord{..} = do
  rv <- randomUnitVector
  let scatterDir = let d = hitNormal + rv
                   in if nearZero d
                      then hitNormal
                      else d
      scattered = Ray hitPt scatterDir
  pure $ Just (albedo, scattered)

-- |Mirror reflection.
reflect :: Vec3 -> Vec3 -> Vec3
reflect v n = v - 2 * v `dot` n *^ n

-- |Metals mirror-reflect.
metal :: Color' -> Double -> Scatter
metal albedo fuzz r_in HitRecord{..} = do
  rv <- randomInUnitSphere
  let reflected = reflect (signorm (rayDir r_in)) hitNormal
      fuzz' = fuzz `min` 1
      scattered = Ray hitPt (reflected + fuzz' *^ rv)
  return $ if reflected `dot` hitNormal > 0
    then Just (albedo, scattered)
    else Nothing

rayColor :: Ray -> Some Surface -> Int -> IO Color'
rayColor _ _ 0 = pure $ V3 0 0 0
rayColor r@Ray{..} (Some world) depth =
  case hit r world 0.001 infinity of
    Just hr@HitRecord{..} -> do
      hitScatter r hr >>= \case
        Just (attenuation, scattered) -> do
          rc' <- rayColor scattered (Some world) (depth-1)
          return $ attenuation * rc'
        Nothing -> return $ V3 0 0 0
    Nothing -> pure $ lerp (0.5 * ((signorm rayDir ^. _y) + 1.0)) (V3 1 1 1) (V3 0.5 0.7 1.0)

-- |Refraction is described by Snell's law.
refract :: Vec3 -> Vec3 -> Double -> Vec3
refract uv n etai_over_etat =
  let cosθ           = (-uv `dot` n) `min` 1.0
      r_out_perp     = etai_over_etat *^ (uv + cosθ *^ n)
      r_out_parallel = (-sqrt (abs (1.0 - r_out_perp `dot` r_out_perp))) *^ n
  in r_out_perp + r_out_parallel

-- |Schlick approximation.
dielectric :: Double -> Scatter
dielectric ir {- index of refraction -} r_in HitRecord{..} = do
  rd <- randomRIO (0.0, 0.999)
  let attenuation = V3 1.0 1.0 1.0
      refractionRatio = if hitFrontFace then 1.0/ir else ir
      unitDir = signorm (rayDir r_in)
      cosθ = ((-unitDir) `dot` hitNormal) `min` 1.0
      sinθ = sqrt (1.0 - cosθ ^ (2::Int))
      cannotRefract = refractionRatio * sinθ > 1.0
      direction = if cannotRefract || reflectance cosθ refractionRatio > rd
                  then reflect unitDir hitNormal
                  else refract unitDir hitNormal refractionRatio
      scattered = Ray hitPt direction
      reflectance :: Double -> Double -> Double
      reflectance cosine ref_idx =
        let r0 = realToFrac (1-ref_idx) / realToFrac (1+ref_idx)
            r  = r0 * r0
        in r + (1-r) * (1-cosine) ^ (5::Int)
  return $ Just (attenuation, scattered)

getRay :: Camera -> (Double, Double) -> IO Ray
getRay Camera{..} (s, t) = do
  r <- randomInUnitDisk
  let rd = lensRadius *^ r
      offset = cu ^* (rd ^. _x) + cv ^* (rd ^. _y)
  pure $ Ray { rayOrig = cOrigin + offset
             , rayDir  = cLowerLeft + s *^ cHorizontal + t *^ cVertical - cOrigin - offset
             }

randomV3 :: IO Vec3
randomV3 = do
  r1 <- randomIO
  r2 <- randomIO
  r3 <- randomIO
  pure (V3 r1 r2 r3)

randomMaterial :: IO Scatter
randomMaterial = do
  g <- randomRIO (0.0, 1.0) :: IO Double
  if | g < 0.8 -> do c1 <- randomV3
                     c2 <- randomV3
                     pure $ lambertian (c1 * c2)
     | g < 0.95 -> do albedo <- randomRIO (0.5, 1)
                      fuzz   <- randomRIO (0, 0.5)
                      pure $ metal albedo fuzz
     | otherwise -> do pure $ dielectric 1.5

randomScene :: IO (Some Surface)
randomScene = do
  let randomObj :: Int -> Int -> IO (Maybe (Some Surface))
      randomObj a b = do
        da <- randomRIO (0, 1.0)
        db <- randomRIO (0, 1.0)
        let center = V3 (realToFrac a + 0.9*da) 0.2 (realToFrac b + 0.9*db)
        if (distance center (V3 4 0.2 0) > 0.9)
          then do mat <- randomMaterial
                  return . Just . Some $ Sphere center 0.2 mat
          else do return Nothing

  objs <- catMaybes <$> sequence [randomObj a b | a <- [-11..10], b <- [-11..10]]

  let s1 = Some $ Sphere (V3 0 1 0) 1.0 (dielectric 1.5)
      s2 = Some $ Sphere (V3 (-4) 1 0) 1.0 (lambertian (V3 0.4 0.2 0.1))
      s3 = Some $ Sphere (V3 4 1 0) 1.0 (metal (V3 0.7 0.6 0.5) 0)

  return (Some $ [s1, s2, s3] ++ objs)

main :: IO ()
main = do
  args <- getArgs

  -- Image
  let aspect_ratio = 3 / 2 :: Rational
      img_height   = floor $ realToFrac img_width / aspect_ratio
      max_depth    = 50

      -- (Small)
      img_width    = 400  :: Int
      n_samples    = 20   :: Int

      -- (Large)
      -- Unfortunately, this can take 10 days to complete!
      -- img_width = 1200 :: Int
      -- n_samples = 500  :: Int

  -- World
  world <- randomScene

  -- Camera
  let lookfrom = V3 13 2 3
      lookat   = V3 0 0 0
      vup      = V3 0 1 0
      dist_to_focus = 10.0
      aperture     = 0.1
      cam = mkCamera lookfrom lookat vup (pi/9) aspect_ratio aperture dist_to_focus

  -- Render
  let msaa :: Scheduler RealWorld () -> MArray RealWorld S Ix2 Pixel -> IO ()
      msaa sched marr = do
        for_ [img_height - 1, img_height - 2 .. 0] $ \j -> do
          for_ [0 .. img_width - 1] $ \i -> scheduleWork_ sched $ do
            print (j, i)
            pixelColorRef <- newIORef (V3 0.0 0.0 0.0)
            for_ [1..n_samples] $ \_ -> do
              du <- randomRIO (0.0, 0.999)
              dv <- randomRIO (0.0, 0.999)
              let u = (realToFrac i + du) / ((realToFrac img_width) - 1.0) :: Double
                  v = (realToFrac (img_height - j) - dv) / ((realToFrac img_height) - 1.0) :: Double
              r <- getRay cam (u, v)
              color <- rayColor r world max_depth
              modifyIORef' pixelColorRef (+ color)
            color <- readIORef pixelColorRef
            A.write_ marr (j :. i) (encodePixel color n_samples)
  arr <- A.createArray_ Par (Sz (img_height :. img_width)) msaa

  A.writeImage (args!!0) arr
