import           Codec.Wav                          (exportFile, importFile)
import           DSP.Convolution                    (conv, test)
import           Data.Array                         as Array ()
import           Data.Array.Unboxed                 as UArray (elems, listArray)
import           Data.Audio                         (Audio (Audio))
import           Data.Int                           (Int32)
import           Data.Maybe                         (fromMaybe)
import           Numeric.Transform.Fourier.FFTUtils (write_rfft_info)
import           System.IO                          (FilePath)


-- File input
filename = "sample.wav"

inMain :: FilePath -> IO ()
inMain path = do
  maybeAudio <- importFile path
  case maybeAudio :: Either String (Audio Int32) of
    Left s -> putStrLn $ "wav decoding error: " ++ s
    Right (Audio rate channels samples) -> do
      putStrLn $ "rate = " ++ show rate
      putStrLn $ "channels: " ++ show channels
      print $ UArray.elems samples

-- DSP processing happens here

sinewave :: [Float]
sinewave = map (\x -> sin ((fromInteger x) / 10.0)) [1..10000]


-- File output

outMain :: FilePath -> IO ()
outMain path = do
  let fs :: [Float]
      fs = sinewave
      l = 1000
      rate = 44100
  -- Float is 32bit, so i use Int32 for each sample in the output.
  let maxInt32 :: Float
      maxInt32 = fromIntegral (maxBound::Int32)
      -- actual transformation function
      rounder :: Float -> Int32
      rounder = round . (*maxInt32)
  exportFile path ( Audio rate 1 -- 1 channel
                  $ UArray.listArray (0,l)
                  $ map rounder
                  $ fs)

-- IO

main = do
  putStrLn $ "* Outputting the sound to "++filename
  outMain filename
  putStrLn $ "* Printing the content of "++filename
  inMain filename
