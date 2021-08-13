import           Codec.Wav          (exportFile, importFile)
import           DSP.Convolution    (conv)
import           Data.Array         as Array (Array, elems, listArray)
import           Data.Array.Unboxed as UArray (elems, listArray)
import           Data.Audio         (Audio (Audio))
import           Data.Int           (Int32)
import           Data.Maybe         (fromMaybe)
import           System.IO          (FilePath)



maxInt32 :: Float
maxInt32 = fromIntegral (maxBound::Int32)

rate :: Int
rate = 44100

outLength = 1000000

-- File input
filename :: FilePath
filename = "sounds/consynth.wav"

ir :: FilePath
ir = "sounds/cave_ir.wav"

fromSF :: FilePath -> IO [Float]
fromSF path = do
  maybeAudio <- importFile path
  case maybeAudio :: Either String (Audio Int32) of
    Left s -> return []
    Right (Audio rate channels samples) ->
      return $ map (\sample -> fromIntegral sample / maxInt32) $ UArray.elems samples


-- DSP processing happens here

convolve samps ir =
    let sampArray ss = Array.listArray ((0, outLength)) ss
    in
      conv (sampArray samps) (sampArray ir)

-- File output

outMain :: FilePath -> [Float] -> IO ()
outMain path samps = do
  let rounder :: Float -> Int32
      rounder = round . (*maxInt32)
  exportFile path ( Audio rate 1 -- 1 channel
                  $ UArray.listArray (0, outLength)
                  $ map rounder
                  $ samps)

-- IO

main = do
  samples <- fromSF filename
  ir <- fromSF ir
  let echo = convolve samples ir
  outMain "output/out.wav" $ Array.elems echo
