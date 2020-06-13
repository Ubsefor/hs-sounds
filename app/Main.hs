module Main where

import qualified Data.ByteString.Builder as B
import qualified Data.ByteString.Lazy    as B
import           Data.Foldable
import           Data.List
import           Lib
import           System.Process
import           Text.Printf

type Seconds = Float

type Samples = Float

type HZ = Float

type Pulse = Float

type Semitones = Float

file :: FilePath
file = "output.bin"

volume :: Float
volume = 0.5

magicNum :: Float
magicNum = 2 ** (1.0 / 12.0)

sampleRate :: Samples
sampleRate = 48000.0

pitchStandard :: HZ
pitchStandard = 440.0

main :: IO ()
main = someFunc

semitones :: Semitones -> HZ
semitones n = pitchStandard * magicNum ** n

note :: Semitones -> Seconds -> [Pulse]
note n dur = mke_wave (semitones n) dur

mke_wave :: HZ -> Seconds -> [Pulse]
mke_wave hz dur =
  map (* volume) $
  zipWith4 (\x y z e -> x * y * z * e) attack decay release output
  where
    step = (hz * 2 * pi) / sampleRate
    attack :: [Pulse]
    attack = map (min 1.0) [0.0,0.001 ..]
    decay :: [Pulse]
    decay = map (max volume) [1.0,0.999 ..]
    release :: [Pulse]
    release = reverse $ take (length output) attack
    output :: [Pulse]
    output = map sin $ map (* step) [0.0 .. sampleRate * dur]

wave :: [Pulse]
wave = concat [note i duration | i <- [0 .. 10]]
  where
    duration :: Seconds
    duration = 0.5

save :: FilePath -> IO ()
save file_path =
  B.writeFile file_path $ B.toLazyByteString $ fold $ map B.floatLE wave

play :: IO ()
play = do
  save file
  _ <-
    runCommand $ printf "ffplay -showmode 1 -f f32le -ar %f %s" sampleRate file
  return ()
