module ColorSound where

import FFT

import Data.WAVE
import Data.Complex
import Graphics.Gloss

import Debug.Trace

windowSize :: Num a => a
windowSize = 1024

edge1Hz :: Num a => a
edge1Hz = 200

edge2Hz :: Num a => a
edge2Hz = 800

split3 :: (Int, Int) -> [a] -> ([a], [a], [a])
split3 (e1, e2) l = (x, y, z)
  where
    (x, xs) = splitAt e1 l
    (y, z)  = splitAt e2 xs

re2cx :: Num a => a -> Complex a
re2cx a = a :+ 0

uncurry3 :: (a -> b -> c -> d) -> ((a,b,c) -> d)
uncurry3 f = (\(a,b,c) -> f a b c)

colorSound :: WAVE -> Float -> Picture
colorSound w t = 
    trace debugStr $
    color rgbSpec $ circleSolid 80
  where
    rate    = fromIntegral $ waveFrameRate $ waveHeader w
    pose    = round $ t * rate

    hz2ix   = round . ((*) (windowSize / rate * 2))
    edge1   = hz2ix edge1Hz 
    edge2   = hz2ix edge2Hz

    mono    = map head
    sample  = (take windowSize) . (drop pose) . mono . waveSamples
    spec    = fft . (map (re2cx . sampleToDouble)) . sample
    spec3   = split3 (edge1, edge2) $ spec w

    energy  = (foldl1 (+)) . (map magnitude)
    eNorm v = energy v / (fromIntegral (length v))
    energy3 (rs, gs, bs) =
                let r = eNorm rs
                    g = eNorm gs
                    b = eNorm bs
                    m = sum [r, g, b]
                in ( realToFrac (r / m)
                   , realToFrac (g / m)
                   , realToFrac (b / m))

    rgbSpec = uncurry3 makeColor (energy3 spec3) 1

    debugStr = show rgbSpec ++ " T:" ++ show t ++ " P:" ++ show pose

