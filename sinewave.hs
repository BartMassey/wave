--- Copyright (C) 2007 Bart Massey
--- ALL RIGHTS RESERVED
--- Please see the end of this file for license information.

import System.Environment
import Data.Maybe
import Data.List
import System.Console.ParseArgs
import Data.WAVE

data Options =
    OptionHz |
    OptionAmplitude |
    OptionFrameRate |
    OptionBits |
    OptionDuration |
    OptionOutputFile
    deriving (Ord, Eq, Show)

argd :: [ Arg Options ]
argd = [ Arg { argIndex = OptionHz,
               argName = Just "frequency",
               argAbbr = Just 'f',
               argData = argDataDefaulted "hz" ArgtypeDouble 440,
               argDesc = "Output frequency (default 440)" },
         Arg { argIndex = OptionAmplitude,
               argName = Just "amplitude",
               argAbbr = Just 'a',
               argData = argDataDefaulted "fraction" ArgtypeDouble 0.5,
               argDesc = "Output amplitude (0.0..1.0, default 0.5)" },
         Arg { argIndex = OptionFrameRate,
               argName = Just "rate",
               argAbbr = Just 'r',
               argData = argDataDefaulted "fps" ArgtypeInt 44100,
               argDesc = "Output frame rate (default 44100)" },
         Arg { argIndex = OptionBits,
               argName = Just "bits",
               argAbbr = Just 'b',
               argData = argDataDefaulted "nbits" ArgtypeInt 16,
               argDesc = "Output bit width (default 16)" },
         Arg { argIndex = OptionDuration,
               argName = Just "time",
               argAbbr = Just 't',
               argData = argDataDefaulted "secs" ArgtypeDouble 1.0,
               argDesc = "Output duration  (default 1.0)" },
         Arg { argIndex = OptionOutputFile,
               argName = Nothing,
               argAbbr = Nothing,
               argData = argDataOptional "output" ArgtypeString,
               argDesc = "Output file" }]

gen_sin :: Double -> Double -> Double -> Int -> [Double]
gen_sin a hz secs frame_rate =
    let sr = fromIntegral frame_rate in
    [ a * sin (2 * pi * hz * fromIntegral t / sr) |
      t <- [0 .. floor (sr * secs)] ]

main = do
  args <- parseArgsIO ArgsComplete argd
  let hz = fromJust (getArgDouble args OptionHz)
  let frame_rate = fromJust (getArgInt args OptionFrameRate)
  let bits_per_sample = fromJust (getArgInt args OptionBits)
  let seconds = fromJust (getArgDouble args OptionDuration)
  let frames = floor (fromIntegral frame_rate * seconds)
  let amplitude = fromJust (getArgDouble args OptionAmplitude)
  let sin_samples = map doubleToSample
                        (gen_sin amplitude hz seconds frame_rate)
  let wav = WAVE {
    waveHeader = WAVEHeader {
      waveNumChannels = 1,
      waveFrameRate = frame_rate,
      waveBitsPerSample = bits_per_sample,
      waveFrames = Just frames },
    waveSamples = transpose [sin_samples] }
  h <- getArgStdio args OptionOutputFile WriteMode
  hPutWAVE h wav

--- Redistribution and use in source and binary forms, with or
--- without modification, are permitted provided that the
--- following conditions are met:
---     * Redistributions of source code must retain the above
---       copyright notice, this list of conditions and the following
---       disclaimer.
---     * Redistributions in binary form must reproduce the
---       above copyright notice, this list of conditions and the
---       following disclaimer in the documentation and/or other
---       materials provided with the distribution.
---     * Neither the name of Bart Massey, nor the names
---       of other affiliated organizations, nor the names
---       of other contributors may be used to endorse or promote
---       products derived from this software without specific prior
---       written permission.
--- 
--- THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND
--- CONTRIBUTORS "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES,
--- INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF
--- MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
--- DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT OWNER OR
--- CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
--- SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT
--- NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
--- LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION)
--- HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN
--- CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR
--- OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
--- SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
