{-

This is a Haskell implementation of "Ray tracing in one weekend".
https://raytracing.github.io/books/RayTracingInOneWeekend.html

The following code seems inefficient...

-}

{-# LANGUAGE Strict #-}
{-# LANGUAGE StrictData #-}
module Steps where

import           Control.Lens ((^.))
import           Data.Foldable
import           Data.IORef
import           Data.Massiv.Array as A
import qualified Data.Massiv.Array.IO as A
import           Data.Massiv.Array.IO hiding (Image, Pixel)
import           Data.Maybe
import           Data.Traversable
import           Linear.Metric
import           Linear.V3
import           Linear.Vector
import           System.Random

-- |In the RGBA colorspace, a pixel has 3 channels of [Word8]s.
--
-- JuicyPixels's Pixel type is a newtype of Color's [Color], which is
-- a data family. Here, [PngPixel] is an instance of [Num], [Ord],
-- [Eq].  So you can, e.g., add two pixels together.
type Pixel = A.Pixel (SRGB 'NonLinear) Word8

-- |Image is a matrix of [Pixel]s.
type Image = Matrix S Pixel

-- |We use massiv-io to load and save image data.
--
-- massiv and massiv-io are usually pretty fast, but it's
-- strongly-typed interfaces can be confusing at times.
--
-- Quick recap of important bits:
--
-- 1. [Image r cs e] is a two-dimensional array with representation r, colorspace cs, and element type e.
-- 2. [Image r cs e] is equivalent to type [Matrix r (Pixel cs e)], which is in turn equivalent to [Array r Ix2 (Pixel cs e)].
-- 3. Different file formats expect different array types. E.g., for PNG, you cannot use [Double] as the element type, otherwise you won't be able to use [writeArray].
-- 4. Use [writeArray] to save array data into a file.
-- 5. The representation should be [S], which means [Storable]. [U] (unboxed) and [P] (primitive) won't work.
testImageSave :: IO Image
testImageSave = do
  let
    arr = A.makeArrayR S Par (Sz (256 :. 256)) (\ (i :. j) -> pixelAt i j)

    pixelAt :: Int -> Int -> Pixel
    pixelAt i j =
      let r = (realToFrac i) / 255.0 * 255.999 :: Double
          g = (realToFrac j) / 255.0 * 255.999 :: Double
          b = 0.25 * 255.999 :: Double
      in PixelSRGB (floor r) (floor g) (floor b)

  A.writeImage "output.png" arr
  return arr

-- |Use [readImage] to read image data from a file.
--
-- You can check [testImageSave] by
--
-- >>> img1 <- testImageSave
-- >>> img2 <- testImageRead
-- >>> print (img1 == img2)
--
-- It should print [True].
testImageRead :: IO Image
testImageRead = A.readImage "output.png"

type Pt3 = V3 Double
type Vec3 = V3 Double

-- |Internal color format.
type Color' = V3 Double

-- |Internal pixel format.
type Pixel' = Color'

-- |Internal image format.
type Image' = Matrix S Pixel'

encodePixel :: V3 Double -> Pixel
encodePixel (V3 x y z) = PixelSRGB (floor $ x * 255.999) (floor $ y * 255.999) (floor $ z * 255.999)

-- |A ray is defined by an origin point and a direction.
data Ray = Ray
  { rayOrig :: Pt3
  , rayDir  :: Vec3
  }

at :: Ray -> Double -> Pt3
at Ray{..} t = rayOrig + rayDir ^* t

-- |A gradient color.
rayColor1 :: Ray -> Color'
rayColor1 Ray{..} =
  let rayDir' = signorm rayDir
      t = 0.5 * ((rayDir' ^. _y) + 1.0)
  in lerp t (V3 0.5 0.7 1.0) (V3 1.0 1.0 1.0)

testSendingRaysIntoScene :: (Ray -> Color') -> IO ()
testSendingRaysIntoScene rayColor = do
  -- Image
  let aspectRatio = 16 / 9 :: Rational
      imgWidth    = 3200 :: Int
      imgHeight   = floor ((realToFrac imgWidth) / aspectRatio) :: Int

  -- Camera
  let viewportHeight = 2.0  :: Double
      viewportWidth  = (realToFrac aspectRatio) * viewportHeight :: Double
      focalLength    = 1.0  :: Double

      origin     = V3 0 0 0
      horizontal = V3 viewportWidth 0 0
      vertical   = V3 0 viewportHeight 0
      lowerLeftCorner = origin - horizontal ^/ 2 - vertical ^/ 2 - V3 0 0 focalLength

  -- Render
  let arr = A.makeArrayR S Par (Sz (imgHeight :. imgWidth)) (\(j :. i) -> pixelAt j i)
      pixelAt j i =
        let u = (realToFrac i) / ((realToFrac imgWidth) - 1.0) :: Double
            v = (realToFrac (imgHeight - j)) / ((realToFrac imgHeight) - 1.0) :: Double
            r = Ray origin (lowerLeftCorner + u *^ horizontal + v *^ vertical - origin)
        in encodePixel (rayColor r)
  A.writeImage "output.png" arr

-- |Check if the ray hits the sphere.
sphereHit :: Pt3 -> Double -> Ray -> Bool
sphereHit center radius Ray{..} =
  let oc = rayOrig - center
      a = rayDir `dot` rayDir
      b = 2.0 * (oc `dot` rayDir)
      c = (oc `dot` oc) - radius * radius
      discriminant = b * b - 4 * a * c
  in discriminant > 0

-- |Put a sphere on (0, 0, -1). Color it red.
rayColor2 :: Ray -> Color'
rayColor2 r
  | sphereHit (V3 0 0 (-1)) 0.5 r = V3 1 0 0
  | otherwise                     = rayColor1 r

-- |A hit function that consider surface normals.
--
-- Different from 'sphereHit', it returns the solution to the
-- equiation now.
sphereHitNormal :: Pt3 -> Double -> Ray -> Double
sphereHitNormal center radius Ray{..} =
  let oc = rayOrig - center
      a = rayDir `dot` rayDir
      b = 2.0 * (oc `dot` rayDir)
      c = (oc `dot` oc) - radius * radius
      discriminant = b * b - 4 * a * c
  in if discriminant < 0
     then -1.0
     else ((-b) - sqrt discriminant) / (2.0 * a)

-- |Simplify the above function using algebra.
sphereHitNormalSimplified :: Pt3 -> Double -> Ray -> Double
sphereHitNormalSimplified center radius Ray{..} =
  let oc = rayOrig - center
      a = rayDir `dot` rayDir
      half_b = oc `dot` rayDir
      c = (oc `dot` oc) - radius * radius
      discriminant = half_b * half_b - a * c
  in if discriminant < 0
     then -1.0
     else ((-half_b) - sqrt discriminant) / a

-- |If the ray hits the sphere, compute its surface normal at that point.
rayColorNormal :: Ray -> Color'
rayColorNormal r@Ray{..} =
  let t = sphereHitNormalSimplified (V3 0 0 (-1)) 0.5 r
  in if t > 0.0
     then let n = signorm (r `at` t - (V3 0 0 (-1)))
          in 0.5 *^ (n + 1.0)
     else let rayDir' = signorm rayDir
              t = 0.5 * (rayDir' ^. _y + 1.0)
          in lerp t (V3 0.5 0.7 1.0) (V3 1.0 1.0 1.0)

-- Test 2
-- main :: IO ()
-- main = testSendingRaysIntoScene rayColorNormal

-- |We make a common interface for all things that can be "hit" by a ray.
class Surface a where
  hit :: Ray -> a -> Double -> Double -> Maybe HitRecord

data HitRecord = HitRecord
  { hitPt :: Pt3         -- ^ The intersection point.
  , hitNormal :: Vec3    -- ^ The normal that goes against the ray. Not always pointing outward.
  , hitT :: Double        -- ^ The parameter @t@.
  , hitFrontFace :: Bool -- ^ Whether the ray hits the front face?
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

-- |We don't care what the type is.
data SomeSurface = forall a. Surface a => SomeSurface a

type SurfaceList = [SomeSurface]

instance Surface SurfaceList where
  hit r xs t_min t_max = go xs t_min t_max Nothing
    where
      go [] _ _ res = res
      go (SomeSurface x : xs) t_min t_max res =
        case hit r x t_min t_max of
          Nothing -> go xs t_min t_max res
          Just hr@HitRecord{hitT} -> go xs t_min hitT (Just hr)

infinity :: Floating a => a
infinity = 1 / 0

rayColorHittable :: Ray -> SomeSurface -> Color'
rayColorHittable r@Ray{..} (SomeSurface world) =
  case hit r world 0 infinity of
    Just HitRecord{..} -> 0.5 *^ (hitNormal + (V3 1 1 1))
    Nothing            -> lerp (0.5 * ((signorm rayDir ^. _y) + 1.0)) (V3 1 1 1) (V3 0.5 0.7 1.0)

testHittableList :: IO ()
testHittableList = do
  -- Image
  let aspectRatio = 16 / 9 :: Rational
      imgWidth    = 3200 :: Int
      imgHeight   = floor ((realToFrac imgWidth) / aspectRatio) :: Int

  -- World
  let world = SomeSurface [SomeSurface sphere1, SomeSurface sphere2]
      sphere1 = Sphere (V3 0 0 (-1)) 0.5 undefined
      sphere2 = Sphere (V3 0 (-100.5) (-1)) 100 undefined

  -- Camera
  let viewportHeight = 2.0  :: Double
      viewportWidth  = (realToFrac aspectRatio) * viewportHeight :: Double
      focalLength    = 1.0  :: Double

      origin     = V3 0 0 0
      horizontal = V3 viewportWidth 0 0
      vertical   = V3 0 viewportHeight 0
      lowerLeftCorner = origin - horizontal ^/ 2 - vertical ^/ 2 - V3 0 0 focalLength

  -- Render
  let arr = A.makeArrayR S Par (Sz (imgHeight :. imgWidth)) (\(j :. i) -> pixelAt j i)
      pixelAt j i =
        let u = (realToFrac i) / ((realToFrac imgWidth) - 1.0) :: Double
            v = (realToFrac (imgHeight - j)) / ((realToFrac imgHeight) - 1.0) :: Double
            r = Ray origin (lowerLeftCorner + u *^ horizontal + v *^ vertical - origin)
        in encodePixel (rayColorHittable r world)
  A.writeImage "output.png" arr

-- Test 3
-- main = testHittableList

-- |Wrap the camera.
data Camera = Camera
  { cOrigin :: Pt3
  , cHorizontal :: Vec3
  , cVertical :: Vec3
  , cLowerLeft :: Pt3
  , cu, cv, cw :: Vec3
  , lensRadius :: Double
  }

defaultCamera :: Camera
defaultCamera =
  let cAspectRatio = 16 / 9 :: Rational
      cViewportHeight = 2.0 :: Double
      cViewportWidth = (realToFrac cAspectRatio) * cViewportHeight
      cFocalLength = 1.0
      cOrigin = V3 0 0 0
      cHorizontal = V3 cViewportWidth 0 0
      cVertical = V3 0 cViewportHeight 0
      cLowerLeft = cOrigin - cHorizontal ^/ 2 - cVertical ^/ 2 - V3 0 0 cFocalLength
      (cu, cv, cw) = undefined
      lensRadius = undefined
  in Camera{..}

-- |Send a ray from the origin to (u, v).
rayTo :: Camera -> (Double, Double) -> Ray
rayTo Camera{..} (u, v) = Ray
  { rayOrig = cOrigin
  , rayDir  = cLowerLeft + u *^ cHorizontal + v *^ cVertical - cOrigin
  }

clamp :: Ord a => (a, a) -> a -> a
clamp (lo, hi) x
  | x < lo    = lo
  | x > hi    = hi
  | otherwise = x

clamp1 :: Double -> Double
clamp1 = clamp (0.0, 0.999)

encodePixelMSAA :: V3 Double -> Int -> Pixel
encodePixelMSAA (V3 r g b) n_samples =
  let conv = floor . (*256) . clamp1 . (/ realToFrac n_samples)
  in PixelSRGB (conv r) (conv g) (conv b)

testMSAA :: IO ()
testMSAA = do
  -- Image
  let n_samples = 100 :: Int
      aspectRatio = 16 / 9 :: Rational
      imageWidth = 400 :: Int
      imageHeight = floor (realToFrac imageWidth / realToFrac aspectRatio :: Double)

  -- World
  let world = SomeSurface [sphere1, sphere2]
      sphere1 = SomeSurface $ Sphere (V3 0 0 (-1)) 0.5 undefined
      sphere2 = SomeSurface $ Sphere (V3 0 (-100.5) (-1)) 100 undefined

  -- Camera
  let cam = defaultCamera

  -- Render
  let msaa :: Scheduler RealWorld () -> MArray RealWorld S Ix2 Pixel -> IO ()
      msaa _sched marr = do   -- FIXME: cannot get scheduler to work
        for_ [imageHeight-1,imageHeight-2..0] $ \j -> do
          for_ [0..imageWidth-1] $ \i -> do
            pixelColorRef <- newIORef (V3 0.0 0.0 0.0)
            for_ [1..n_samples] $ \_ -> do
              du <- randomRIO (0.0, 0.999)
              dv <- randomRIO (0.0, 0.999)
              let u = (realToFrac i + du) / ((realToFrac imageWidth) - 1.0) :: Double
                  v = (realToFrac (imageHeight - j) - dv) / ((realToFrac imageHeight) - 1.0) :: Double
                  r = cam `rayTo` (u, v)
              modifyIORef' pixelColorRef (+ rayColorHittable r world)
            color <- readIORef pixelColorRef
            A.write_ marr (j :. i) (encodePixelMSAA color n_samples)
  arr <- A.createArray_ Par (Sz (imageHeight :. imageWidth)) msaa
  A.writeImage "output.png" arr

-- Test 4
-- main :: IO ()
-- main = testMSAA


-- |Find a random point in a unit sphere.
randomInUnitSphere :: IO Pt3
randomInUnitSphere =
  let r = randomRIO (0.0, 0.999) :: IO Double
  in do p <- sequence (V3 r r r)
        if distance p (V3 0 0 0) < 1.0
          then return p
          else randomInUnitSphere

-- |Used by Lambertian reflection.
randomUnitVector :: IO Vec3
randomUnitVector = signorm <$> randomInUnitSphere

rayColorDiffuse :: Ray -> SomeSurface -> Int -> IO Color'
rayColorDiffuse _ _ 0 = pure $ V3 0 0 0
rayColorDiffuse r@Ray{..} (SomeSurface world) depth =
  case hit r world 0.001 infinity of  -- t shouldn't be too close to 0; get rid of the shadow acne problem
    Just HitRecord{..} -> do
      r <- randomUnitVector   -- Correct Lambertian reflection.
      let target = hitPt + hitNormal + r
      diffusion <- rayColorDiffuse (Ray hitPt (target - hitPt)) (SomeSurface world) (depth - 1)
      pure $ 0.5 * diffusion
    Nothing -> pure $ lerp (0.5 * ((signorm rayDir ^. _y) + 1.0)) (V3 1 1 1) (V3 0.5 0.7 1.0)


-- |Apply Gamma correction with gamma = 2.
--
-- Gamma correction: pixel ^ (1/gamma).
encodePixelMSAAGamma :: V3 Double -> Int -> Pixel
encodePixelMSAAGamma (V3 r g b) n_samples =
  let conv = floor . (*256) . clamp1 . sqrt . (/ realToFrac n_samples)
  in PixelSRGB (conv r) (conv g) (conv b)

testDiffusion :: IO ()
testDiffusion = do
  -- Image
  let n_samples = 100 :: Int
      aspectRatio = 16 / 9 :: Rational
      imageWidth = 400 :: Int
      imageHeight = floor (realToFrac imageWidth / realToFrac aspectRatio :: Double)
      maxDepth = 50

  -- World
  let world = SomeSurface [sphere1, sphere2]
      sphere1 = SomeSurface $ Sphere (V3 0 0 (-1)) 0.5 undefined
      sphere2 = SomeSurface $ Sphere (V3 0 (-100.5) (-1)) 100 undefined

  -- Camera
  let cam = defaultCamera

  -- Render
  let msaa :: Scheduler RealWorld () -> MArray RealWorld S Ix2 Pixel -> IO ()
      msaa _sched marr = do
        for_ [imageHeight-1,imageHeight-2..0] $ \j -> do
          for_ [0..imageWidth-1] $ \i -> do
            pixelColorRef <- newIORef (V3 0.0 0.0 0.0)
            for_ [1..n_samples] $ \_ -> do
              du <- randomRIO (0.0, 0.999)
              dv <- randomRIO (0.0, 0.999)
              let u = (realToFrac i + du) / ((realToFrac imageWidth) - 1.0) :: Double
                  v = (realToFrac (imageHeight - j) - dv) / ((realToFrac imageHeight) - 1.0) :: Double
                  r = cam `rayTo` (u, v)
              color <- rayColorDiffuse r world maxDepth
              modifyIORef' pixelColorRef (+ color)
            color <- readIORef pixelColorRef
            A.write_ marr (j :. i) (encodePixelMSAAGamma color n_samples)
  arr <- A.createArray_ Par (Sz (imageHeight :. imageWidth)) msaa
  A.writeImage "output.png" arr

-- Test 5
-- main :: IO ()
-- main = testDiffusion

-- |A uniform interface for ray-object interaction.
type Scatter = Ray -> HitRecord -> IO (Maybe (Color', Ray))

-- |Lambertian reflection.
lambertian :: Color' -> Scatter
lambertian albedo _ HitRecord{..} = do
  rv <- randomUnitVector
  let scatterDir = let d = hitNormal + rv
                   in if nearZero d
                      then hitNormal
                      else d
      scattered = Ray hitPt scatterDir
  pure $ Just (albedo, scattered)

nearZero :: Vec3 -> Bool
nearZero v = v < 1e-8

-- |Mirror reflection.
reflect :: Vec3 -> Vec3 -> Vec3
reflect v n = v - 2 * v `dot` n *^ n

-- |Metals mirror-reflect.
metal :: Color' -> Scatter
metal albedo r_in HitRecord{..} = do
  let reflected = reflect (signorm (rayDir r_in)) hitNormal
      scattered = Ray hitPt reflected
  return $ if reflected `dot` hitNormal > 0
    then Just (albedo, scattered)
    else Nothing

fuzzyMetal :: Color' -> Double -> Scatter
fuzzyMetal albedo fuzz r_in HitRecord{..} = do
  rv <- randomInUnitSphere
  let reflected = reflect (signorm (rayDir r_in)) hitNormal
      fuzz' = fuzz `min` 1
      scattered = Ray hitPt (reflected + fuzz' *^ rv)
  return $ if reflected `dot` hitNormal > 0
    then Just (albedo, scattered)
    else Nothing

rayColorMaterial :: Ray -> SomeSurface -> Int -> IO Color'
rayColorMaterial _ _ 0 = pure $ V3 0 0 0
rayColorMaterial r@Ray{..} (SomeSurface world) depth =
  case hit r world 0.001 infinity of
    Just hr@HitRecord{..} -> do
      hitScatter r hr >>= \case
        Just (attenuation, scattered) -> do
          rc' <- rayColorMaterial scattered (SomeSurface world) (depth-1)
          return $ attenuation * rc'
        Nothing -> return $ V3 0 0 0
    Nothing -> pure $ lerp (0.5 * ((signorm rayDir ^. _y) + 1.0)) (V3 1 1 1) (V3 0.5 0.7 1.0)

testMetal :: IO ()
testMetal = do
  -- Image
  let n_samples = 100 :: Int
      aspectRatio = 16 / 9 :: Rational
      imageWidth = 400 :: Int
      imageHeight = floor (realToFrac imageWidth / realToFrac aspectRatio :: Double)
      maxDepth = 50

  -- World
  let world = SomeSurface [ground, center, left, left', right]
      ground = SomeSurface $ Sphere (V3 0      (-100.5) (-1)  ) 100 (lambertian (V3 0.8 0.8 0.0))
      center = SomeSurface $ Sphere (V3 0            0  (-1)  ) 0.5 (lambertian (V3 0.1 0.2 0.5))
      -- left   = SomeSurface $ Sphere (V3 (-1.0)     0.0  (-1.0)) 0.5 (fuzzyMetal (V3 0.8 0.8 0.8) 0.3)
      -- center = SomeSurface $ Sphere (V3 0            0  (-1)  ) 0.5 (dielectric 1.5)
      left   = SomeSurface $ Sphere (V3 (-1.0)     0.0  (-1.0)) 0.5 (dielectricSchlick 1.5)
      left'  = SomeSurface $ Sphere (V3 (-1.0)     0.0  (-1.0)) (-0.4) (dielectricSchlick 1.5)
      -- ^ Modeling a hollow glass: if radius < 0, then the normal is inverted.
      right  = SomeSurface $ Sphere (V3   1.0      0.0  (-1.0)) 0.5 (fuzzyMetal (V3 0.8 0.6 0.2) 1.0)

  -- Camera
  let cam = defaultCamera

  -- Render
  let msaa :: Scheduler RealWorld () -> MArray RealWorld S Ix2 Pixel -> IO ()
      msaa _sched marr = do
        for_ [imageHeight-1,imageHeight-2..0] $ \j -> do
          for_ [0..imageWidth-1] $ \i -> do
            pixelColorRef <- newIORef (V3 0.0 0.0 0.0)
            for_ [1..n_samples] $ \_ -> do
              du <- randomRIO (0.0, 0.999)
              dv <- randomRIO (0.0, 0.999)
              let u = (realToFrac i + du) / ((realToFrac imageWidth) - 1.0) :: Double
                  v = (realToFrac (imageHeight - j) - dv) / ((realToFrac imageHeight) - 1.0) :: Double
                  r = cam `rayTo` (u, v)
              color <- rayColorMaterial r world maxDepth
              modifyIORef' pixelColorRef (+ color)
            color <- readIORef pixelColorRef
            A.write_ marr (j :. i) (encodePixelMSAAGamma color n_samples)
  arr <- A.createArray_ Par (Sz (imageHeight :. imageWidth)) msaa
  A.writeImage "output.png" arr



-- Test 6
-- main :: IO ()
-- main = testMetal


-- |Refraction is described by Snell's law.
refract :: Vec3 -> Vec3 -> Double -> Vec3
refract uv n etai_over_etat =
  let cosθ           = (-uv `dot` n) `min` 1.0
      r_out_perp     = etai_over_etat *^ (uv + cosθ *^ n)
      r_out_parallel = (-sqrt (abs (1.0 - r_out_perp `dot` r_out_perp))) *^ n
  in r_out_perp + r_out_parallel

dielectric :: Double -> Scatter
dielectric ir {- index of refraction -} r_in HitRecord{..} = do
  let attenuation = V3 1.0 1.0 1.0
      refractionRatio = if hitFrontFace then 1.0/ir else ir
      unitDir = signorm (rayDir r_in)
      cosθ = ((-unitDir) `dot` hitNormal) `min` 1.0
      sinθ = sqrt (1.0 - cosθ ^ (2::Int))
      cannotRefract = refractionRatio * sinθ > 1.0
      direction = if cannotRefract
                  then reflect unitDir hitNormal
                  else refract unitDir hitNormal refractionRatio
      scattered = Ray hitPt direction
  return $ Just (attenuation, scattered)

-- |Schlick Approximation.
dielectricSchlick :: Double -> Scatter
dielectricSchlick ir {- index of refraction -} r_in HitRecord{..} = do
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

-- |Positional cameras.
mkCameraFOV :: Double -> Rational -> Camera
mkCameraFOV vfov cAspectRatio =
  let h = tan (vfov / 2)
      cViewportHeight = 2.0 * h
      cViewportWidth = (realToFrac cAspectRatio) * cViewportHeight
      cFocalLength = 1.0
      cOrigin = V3 0 0 0
      cHorizontal = V3 cViewportWidth 0 0
      cVertical = V3 0 cViewportHeight 0
      cLowerLeft = cOrigin - cHorizontal ^/ 2 - cVertical ^/ 2 - V3 0 0 cFocalLength
      (cu,cv,cw) = undefined
      lensRadius = undefined
  in Camera{..}

mkCamera :: Pt3 -> Pt3 -> Vec3 -> Double -> Rational -> Camera
mkCamera lookfrom lookat vup vfov aspect_ratio =
  let h = tan (vfov / 2)
      cViewportHeight = 2.0 * h :: Double
      cViewportWidth = realToFrac aspect_ratio * cViewportHeight

      w = signorm (lookfrom - lookat)  -- z-axis
      u = (signorm vup) `cross` w      -- x-axis
      v = w `cross` u                  -- y-axis

      cOrigin = lookfrom
      cHorizontal = cViewportWidth *^ u
      cVertical = cViewportHeight *^ v
      cLowerLeft = cOrigin - cHorizontal ^/ 2 - cVertical ^/ 2 - w

      (cu,cv,cw) = (u,v,w)
      lensRadius = undefined
  in Camera{..}

testCamera :: IO ()
testCamera = do
  -- Image
  let n_samples = 100 :: Int
      aspectRatio = 16 / 9 :: Rational
      imageWidth = 400 :: Int
      imageHeight = floor (realToFrac imageWidth / realToFrac aspectRatio :: Double)
      maxDepth = 50

  -- World
  let world = SomeSurface [ground, center, left, left', right]
      ground = SomeSurface $ Sphere (V3 0.0 (-100.5) (-1.0)) 100.0 (lambertian (V3 0.8 0.8 0.0))
      center = SomeSurface $ Sphere (V3 0.0 0.0 (-1.0)) 0.5 (lambertian (V3 0.1 0.2 0.5))
      left  = SomeSurface $ Sphere (V3 (-1.0) 0.0 (-1.0)) 0.5 (dielectric 1.5)
      left'  = SomeSurface $ Sphere (V3 (-1.0) 0.0 (-1.0)) (-0.45) (dielectric 1.5)
      right = SomeSurface $ Sphere (V3 1.0 0.0 (-1.0)) 0.5 (fuzzyMetal (V3 0.8 0.6 0.2) 0.0)

  -- Camera
  let cam = mkCamera (V3 (-2) 2 1) (V3 0 0 (-1)) (V3 0 1 0) (pi/9) aspectRatio

  -- Render
  let msaa :: Scheduler RealWorld () -> MArray RealWorld S Ix2 Pixel -> IO ()
      msaa _sched marr = do
        for_ [imageHeight-1,imageHeight-2..0] $ \j -> do
          print j
          for_ [0..imageWidth-1] $ \i -> do
            pixelColorRef <- newIORef (V3 0.0 0.0 0.0)
            for_ [1..n_samples] $ \_ -> do
              du <- randomRIO (0.0, 0.999)
              dv <- randomRIO (0.0, 0.999)
              let u = (realToFrac i + du) / ((realToFrac imageWidth) - 1.0) :: Double
                  v = (realToFrac (imageHeight - j) - dv) / ((realToFrac imageHeight) - 1.0) :: Double
                  r = cam `rayTo` (u, v)
              color <- rayColorMaterial r world maxDepth
              modifyIORef' pixelColorRef (+ color)
            color <- readIORef pixelColorRef
            A.write_ marr (j :. i) (encodePixelMSAAGamma color n_samples)
  arr <- A.createArray_ Par (Sz (imageHeight :. imageWidth)) msaa
  A.writeImage "output.png" arr

-- Test 8
-- main :: IO ()
-- main = testCamera

randomInUnitDisk :: IO Vec3
randomInUnitDisk = do
  x <- randomRIO (-1, 1)
  y <- randomRIO (-1, 1)
  let p = V3 x y 0
  if p `dot` p < 1 then return p else randomInUnitDisk

mkCameraLens :: Pt3 -> Pt3 -> Vec3 -> Double -> Rational -> Double -> Double -> Camera
mkCameraLens lookfrom lookat vup vfov aspect_ratio aperture focus_dist =
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

getRay :: Camera -> (Double, Double) -> IO Ray
getRay Camera{..} (s, t) = do
  r <- randomInUnitDisk
  let rd = lensRadius *^ r
      offset = cu ^* (rd ^. _x) + cv ^* (rd ^. _y)
  pure $ Ray { rayOrig = cOrigin + offset
             , rayDir  = cLowerLeft + s *^ cHorizontal + t *^ cVertical - cOrigin - offset
             }


testLens :: IO ()
testLens = do
  -- Image
  let n_samples = 100 :: Int
      aspectRatio = 16 / 9 :: Rational
      imageWidth = 400 :: Int
      imageHeight = floor (realToFrac imageWidth / realToFrac aspectRatio :: Double)
      maxDepth = 50

  -- World
  let world = SomeSurface [ground, center, left, left', right]
      ground = SomeSurface $ Sphere (V3 0.0 (-100.5) (-1.0)) 100.0 (lambertian (V3 0.8 0.8 0.0))
      center = SomeSurface $ Sphere (V3 0.0 0.0 (-1.0)) 0.5 (lambertian (V3 0.1 0.2 0.5))
      left  = SomeSurface $ Sphere (V3 (-1.0) 0.0 (-1.0)) 0.5 (dielectric 1.5)
      left'  = SomeSurface $ Sphere (V3 (-1.0) 0.0 (-1.0)) (-0.45) (dielectric 1.5)
      right = SomeSurface $ Sphere (V3 1.0 0.0 (-1.0)) 0.5 (fuzzyMetal (V3 0.8 0.6 0.2) 0.0)

  -- Camera
  let lookfrom = V3 3 3 2
      lookat   = V3 0 0 (-1)
      vup      = V3 0 1 0
      dist_to_focus = distance lookfrom lookat
      aperture = 2.0
  let cam = mkCameraLens lookfrom lookat vup (pi/9) aspectRatio aperture dist_to_focus

  -- Render
  let msaa :: Scheduler RealWorld () -> MArray RealWorld S Ix2 Pixel -> IO ()
      msaa _sched marr = do
        for_ [imageHeight-1,imageHeight-2..0] $ \j -> do
          print j
          for_ [0..imageWidth-1] $ \i -> do
            pixelColorRef <- newIORef (V3 0.0 0.0 0.0)
            for_ [1..n_samples] $ \_ -> do
              du <- randomRIO (0.0, 0.999)
              dv <- randomRIO (0.0, 0.999)
              let u = (realToFrac i + du) / ((realToFrac imageWidth) - 1.0) :: Double
                  v = (realToFrac (imageHeight - j) - dv) / ((realToFrac imageHeight) - 1.0) :: Double
              r <- getRay cam (u, v)
              color <- rayColorMaterial r world maxDepth
              modifyIORef' pixelColorRef (+ color)
            color <- readIORef pixelColorRef
            A.write_ marr (j :. i) (encodePixelMSAAGamma color n_samples)
  arr <- A.createArray_ Par (Sz (imageHeight :. imageWidth)) msaa
  A.writeImage "output.png" arr

randomV3 :: IO Vec3
randomV3 = do
  r1 <- randomIO
  r2 <- randomIO
  r3 <- randomIO
  pure (V3 r1 r2 r3)

randomMaterial :: IO Scatter
randomMaterial = do
  g <- randomRIO (0.0, 1.0) :: IO Double
  if g < 0.8
    then -- diffuse
         do c1 <- randomV3
            c2 <- randomV3
            pure $ lambertian (c1 * c2)
    else if g < 0.95
         then -- metal
              do albedo <- randomRIO (0.5, 1)
                 fuzz   <- randomRIO (0, 0.5)
                 pure $ fuzzyMetal albedo fuzz
         else do -- glass
              pure $ dielectric 1.5

randomScene :: IO SomeSurface
randomScene = do
  objs <- concat <$> (for [-11..11] $ \ a -> do
    catMaybes <$> (for [-11..11] $ \ b -> do
      da <- randomRIO (0, 1.0)
      db <- randomRIO (0, 1.0)
      let center = V3 (a + 0.9*da) 0.2 (b + 0.9*db)
      if (distance center (V3 4 0.2 0) > 0.9)
        then do mat <- randomMaterial
                return . Just . SomeSurface $ Sphere center 0.2 mat
        else return Nothing))

  let s1 = SomeSurface $ Sphere (V3 0 1 0) 1.0 (dielectric 1.5)
      s2 = SomeSurface $ Sphere (V3 (-4) 1 0) 1.0 (lambertian (V3 0.4 0.2 0.1))
      s3 = SomeSurface $ Sphere (V3 4 1 0) 1.0 (fuzzyMetal (V3 0.7 0.6 0.5) 0)
  return (SomeSurface $ [s1, s2, s3] ++ objs)
        
main :: IO ()
main = do
  -- Image
  let aspect_ratio = 3 / 2 :: Rational
      img_width    = 1200  :: Int
      img_height   = floor $ realToFrac img_width / aspect_ratio
      n_samples    = 500
      max_depth    = 50

  -- World
  world <- randomScene

  -- Camera
  let lookfrom = V3 13 2 3
      lookat   = V3 0 0 0
      vup      = V3 0 1 0
      dist_to_focus = 10.0
      aperture     = 0.1
      cam = mkCameraLens lookfrom lookat vup (pi/9) aspect_ratio aperture dist_to_focus

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
              color <- rayColorMaterial r world max_depth
              modifyIORef' pixelColorRef (+ color)
            color <- readIORef pixelColorRef
            A.write_ marr (j :. i) (encodePixelMSAAGamma color n_samples)
  arr <- A.createArray_ Par (Sz (img_height :. img_width)) msaa
  A.writeImage "output.png" arr
