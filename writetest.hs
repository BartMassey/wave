--- Bart Massey 03/2020

--- Write a 1-second 48Ksps 240Hz half-amplitude mono square
--- wave into "square.wav".

import Data.Bits
import Data.WAVE

main :: IO ()
main = do
  let header =
          WAVEHeader {
             waveNumChannels = 1,
             waveFrameRate = 48000,
             waveBitsPerSample = 16,
             waveFrames = Nothing
          }
  let ampl = 1 `shiftL` 30
  let samples = concat $ replicate 240 $
       replicate 100 [-ampl] ++ replicate 100 [ampl]
  let square =
          WAVE {
             waveHeader = header,
             waveSamples = samples
          }
  putWAVEFile "square.wav" square
