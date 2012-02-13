{-----------------------------------------------------------------------------
    tomato-rubato
------------------------------------------------------------------------------}
module Sound.Tomato.Types where

type Name = String

type Frequency = Double

-- MIDI types

data KeyDirection = KeyUp | KeyDown deriving (Eq,Ord,Show,Read)
type Pitch        = Int			-- MIDI pitch value
type Velocity     = Int

data KeyPress = KeyPress
    { direction :: KeyDirection, pitch :: Pitch, velocity :: Velocity }
    deriving (Eq,Ord,Show)

