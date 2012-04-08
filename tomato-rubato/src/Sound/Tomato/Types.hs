{-----------------------------------------------------------------------------
    tomato-rubato
------------------------------------------------------------------------------}
module Sound.Tomato.Types where

type Name = String

-- | Frequency in Hz.
type Frequency = Double

hz, bpm :: Frequency
hz  = 1
bpm = 1/60

-- | Time duration in seconds.
type Time = Double
ms, s :: Time
ms = 1e-3
s  = 1

type Semitones = Double
octave :: Semitones
octave = 12

-- | Volume, measured in decibel.
type Volume = Double
dB :: Volume
dB = 1

-- | Duration of a musical note. (Full, half, quarter,..)
type Duration = Int


-- MIDI types

data KeyDirection = KeyUp | KeyDown deriving (Eq,Ord,Show,Read)
-- | MIDI pitch value
type Pitch        = Int
type Velocity     = Int

data KeyPress = KeyPress
    { direction :: KeyDirection, pitch :: Pitch, velocity :: Velocity }
    deriving (Eq,Ord,Show)

