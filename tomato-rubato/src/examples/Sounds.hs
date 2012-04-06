{-----------------------------------------------------------------------------
    tomato-rubato
------------------------------------------------------------------------------}
import Demonstrate
import Sound.Tomato

{-----------------------------------------------------------------------------
    Instrument sounds proper
------------------------------------------------------------------------------}
test1 = buzzingBass 0.3 55

buzzingBass vel freq = lowpass (60*freq) $ 
    stereoDetune (pulse pulseWidth) freq
    where
    pulseWidth :: Behavior Double
    pulseWidth = 0.5 + 0.3 * sine 0.3


kickDrum = percussive (0.1*ms) (400*ms) $ sine freqSweep
    where
    baseFreq = 55
    freqSweep =
          xLine 4 1 (5 * ms)                    -- fast decay for the kick
        * xLine (4*baseFreq) baseFreq (30 * ms) -- but keep a fat bump

ms = 0.001 :: Behavior Duration

{-----------------------------------------------------------------------------
    Utilities
------------------------------------------------------------------------------}
type Semitones = Double
octave = 12 :: Semitones

-- Adjust a given frequency by a certain number of semitones
detune :: Semitones -> Behavior Frequency -> Behavior Frequency
detune s freq = constant (2**(s/12)) * freq

-- Pan waveform in stereo space with slightly detuned frequencies
stereoDetune waveform = \freq ->
    mix [ balance (-1) (waveform $ freq * 0.999)
        , balance 1 (waveform $ freq * 1.001)]


