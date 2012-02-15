{-----------------------------------------------------------------------------
    tomato-rubato
------------------------------------------------------------------------------}
module Sound.Tomato.Theory (
    -- * Synopsis
    -- | Music theory. Mapping note names to frequencies, etc.
    
    -- * Pitch names
    c,d,e,f,g,a,b,
    
    -- * Conversions
    pitchToFrequency, letterToPitch,
    ) where

import Data.List
import Data.Functor
import Sound.Tomato.Types

{-----------------------------------------------------------------------------
    Note names and mappings
------------------------------------------------------------------------------}
c,d,e,f,g,a,b :: Int -> Pitch
[c,d,e,f,g,a,b] = map (\n octave -> n + 12*octave) [0,2,4,5,7,9,11]

[c1,c2,c3,c4,c5,c6] = map c [1..6]
[d1,d2,d3,d4,d5,d6] = map d [1..6]
[e1,e2,e3,e4,e5,e6] = map e [1..6]
[f1,f2,f3,f4,f5,f6] = map f [1..6]
[g1,g2,g3,g4,g5,g6] = map g [1..6]
[a1,a2,a3,a4,a5,a6] = map a [1..6]
[b1,b2,b3,b4,b5,b6] = map b [1..6]

-- | Convert a MIDI pitch value into a frequency. Well-tempered tuning.
pitchToFrequency :: Pitch -> Frequency
pitchToFrequency pitch = 440 * 2**(fromIntegral (pitch-a4) / 12)

{-----------------------------------------------------------------------------
    Poor man's keyboard
------------------------------------------------------------------------------}
-- | Poor man's keyboard.
-- Turn an input character from your computer keyboard into a MIDI pitch value.
-- Tries to mimic the layout of white and black keys on a piano keyboard.
-- Uses the standard US layout.
letterToPitch :: Char -> Maybe Pitch
letterToPitch x = (3*12+) <$> elemIndex x englishUSKeyboard

type KeyboardLayout = [Char]
englishUSKeyboard :: KeyboardLayout
englishUSKeyboard = "zsxdcvgbhnjm,l.;/q2w3e4rt6y7ui9o0p-[]"


