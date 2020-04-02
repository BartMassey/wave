--- Bart Massey 2007

--- Show some information about a WAVE file given as an
--- argument.

import System.Environment
import ParseArgs
import WAVE

argd :: [ Arg Int ]
argd = [ Arg { argIndex = 0,
               argName = Nothing,
               argAbbr = Nothing,
               argData = argDataOptional "input" ArgtypeString,
               argDesc = "Input file" }]

main = do
  args <- parseArgsIO ArgsComplete argd
  h <- getArgStdio args 0 ReadMode
  wav <- hGetWAVE h
  let samples = waveSamples wav
  case (waveFrames (waveHeader wav)) of
    Just n -> let n' = length samples in
              if (n /= n') then do
                    putStrLn "WAVE size mismatch"
                    putStrLn ("Header: " ++ (show n))
                    putStrLn ("Samples: " ++ (show n'))
              else do
                  mapM_ (putStrLn . show) samples
