{-# LANGUAGE
  FlexibleContexts
#-}
module Graphics.PUI.Gtk.Image
  ( flowImage
  , fixedImage
  , loadImage
  , loadImageDefault
  , CairoImage
  ) where

import Codec.Picture
import Control.Monad
import Control.Monad.Trans
import Data.Array.MArray
import Data.Bits
import Data.Vector.Storable as V
import Data.Word
import Foreign.Ptr
import Graphics.PUI.Gtk.Widget
import Graphics.PUI.Widget
import Graphics.Rendering.Cairo

data CairoImage = CairoImage Surface

vector8to32 :: Vector Word8 -> Vector Word32
vector8to32 v = V.generate (V.length v `div` 3) f
  where f i = let ix i = fromIntegral (v V.! i)
              in shiftL (ix (i * 3)) 16 + shiftL (ix (i * 3 + 1)) 8 + shiftL (ix (i * 3 + 2)) 0

copyVectorToSurface :: Vector Word8 -> Surface -> IO ()
copyVectorToSurface vec surface = do
  sdata <- imageSurfaceGetPixels surface :: IO (SurfaceData Int Word32)
  let copyWord i x = writeArray sdata i x >> return (i + 1)
  V.foldM' copyWord 0 (vector8to32 vec)
  surfaceMarkDirty surface
  return ()

flowImage :: (MonadIO f) => CairoImage -> CairoWidget (V Dim) (V Dim) f
flowImage (CairoImage surface) = mkFlow $ \w h -> do
  iw <- imageSurfaceGetWidth surface
  ih <- imageSurfaceGetHeight surface
  let scaleX = w / fromIntegral iw
      scaleY = h / fromIntegral ih
      scaleXY = min scaleX scaleY
      drawit = do
        scale scaleXY scaleXY
        setSourceSurface surface 0 0
        paint
  return drawit

fixedImage :: (MonadIO f) => CairoImage -> CairoWidget (F Dim) (F Dim) f
fixedImage (CairoImage surface) = mkFixed $ do
  iw <- imageSurfaceGetWidth surface
  ih <- imageSurfaceGetHeight surface
  let drawit = do
        setSourceSurface surface 0 0
        paint
  return (fromIntegral iw, fromIntegral ih, drawit)

loadImage :: FilePath -> IO (Either String CairoImage)
loadImage fp = do
  r <- readImage fp
  case r of
    Left err -> return (Left err)
    Right img' -> do
      let img = convertRGB8 img'
      surface <- createImageSurface FormatRGB24 (imageWidth img) (imageHeight img)
      copyVectorToSurface (imageData img) surface
      return (Right (CairoImage surface))

loadImageDefault :: FilePath -> IO CairoImage
loadImageDefault fp = do
  img <- loadImage fp
  case img of
    Left err -> CairoImage <$> createImageSurface FormatRGB24 1 1
    Right img' -> return img'
