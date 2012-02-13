{-----------------------------------------------------------------------------
    tomato-rubato
------------------------------------------------------------------------------}
{-# LANGUAGE TypeSynonymInstances #-}
module Sound.Tomato.Synthesis (
    -- * Synopsis
    -- | Basic combinators for sound synthesis and processing.
    
    -- * Sound data
    Sound, mix, play, render,
    
    -- * Parameters
    Behavior, parameter, Duration,
    
    -- * Fundamental wave forms and sounds
    sine, sawtooth, brownNoise,
    
    -- * Sound processing and filters
    lowpass, highpass, bandpass, delay, pan,
    
    -- * Envelopes
    Envelope, xLine, percussive
    
    ) where

import Control.Concurrent

import Demonstrate

import qualified Sound.Tomato.Internal.SuperCollider as SC
import Sound.Tomato.Types

{-----------------------------------------------------------------------------
    Sound data
------------------------------------------------------------------------------}
-- | Represents a sound.
-- 
-- Technical details: 44.1kHz, stereo. 
-- TODO: Sometimes mono for unknown reasons.
type Sound = SC.UGen

-- | Combine to sounds by mixing them.
mix :: Sound -> Sound -> Sound
mix = (+)

-- | Play a sound on the speakers.
-- Press any key to interrupt.
play :: Sound -> IO ()
play sound = SC.withSuperCollider $ do
    SC.audition $ SC.out 0 (0.2 * sound)
    getChar
    return ()

instance Demonstrate Sound where
    demo sound = SC.withSuperCollider $ do
        SC.audition $ SC.out 0 (0.2 * sound)
        threadDelay $ 60*10^6 -- wait one minute

-- TODO: Change the duration of an audio Behavior
-- clip :: Duration -> Sound -> Sound
-- clip = undefined

-- | Render an audio Behavior to memory.
-- NOT IMPLEMENTED YET.
render :: Duration -> Sound -> Sample
render = undefined

type Sample = ()

{-----------------------------------------------------------------------------
    Parameters
------------------------------------------------------------------------------}
-- | The type @Behavior a@ represents a value that varies in time.
-- 
-- By using time-varying values as parameters, we can modulate them easily.
-- TODO: Opaque type. Different oscillators for audio and frequencies, etc.
type Behavior a = SC.UGen

-- | Represents a time duration in seconds.
type Duration = Double

-- | Named parameter.
parameter :: Name -> Double -> Behavior Double
parameter = SC.control SC.KR

{-----------------------------------------------------------------------------
    Fundamental wave forms and sounds
------------------------------------------------------------------------------}
-- | Sine wave.
--
-- Sounds like a free telephone line.
sine :: Behavior Frequency -> Sound
sine freq = SC.sinOsc SC.AR freq (SC.constant 0)

-- | Sawtooth wave.
-- 
-- Rasping. Best used with a lowpass to smooth out the harsh high frequencies.
sawtooth :: Behavior Frequency -> Sound
sawtooth freq = SC.saw SC.AR freq

-- | Brown noise (incoherent)
-- 
-- Generates brown noise, i.e. the spectrum falls off in power by 6 dB per octave.
-- Reminds me of rain or a flowing river, but harsher.
brownNoise :: Sound
brownNoise = SC.brownNoise (0 :: Int) SC.AR

{-----------------------------------------------------------------------------
    Sound processing with filters
------------------------------------------------------------------------------}
-- | Lowpass filter.
-- Attenuates frequencies above the given one. 12 dB / octave.
lowpass :: Behavior Frequency -> Sound -> Sound
lowpass  freq sound = SC.lpf sound freq

-- | Highpass filter.
-- Attenuates frequencies below the given one. 12 dB / octave.
highpass :: Behavior Frequency -> Sound -> Sound
highpass freq sound = SC.hpf sound freq

-- | Bandpass filter. (12 dB / octave)
-- 
-- > bandpass frequency rq 
bandpass :: Behavior Frequency -> Behavior Double -> Sound -> Sound
bandpass freq rq sound = SC.bpf sound freq rq

-- | Delay the sound by a specified amount of time.
-- Useful for echo and chorus effects.
-- Maximum delay time is 0.1 seconds.
delay :: Behavior Duration -> Sound -> Sound
delay duration sound = SC.delayN sound (SC.constant 0.1) duration

-- | Equal power stereo pan a mono source.
-- Input from -1 (left speaker) to +1 (right speaker)
pan :: Behavior Double -> Sound -> Sound
pan position sound = SC.pan2 sound position (SC.constant 0.3)

{-----------------------------------------------------------------------------
    Envelopes
------------------------------------------------------------------------------}
-- | An @Envelope@ transforms the sound multiplying it with a
-- loudness profile which indicates how the loudness of the sound varies over time.
type Envelope = Sound -> Sound

-- | Exponential line envelope.
--
-- It's exponential because the human ear is used to a logarithmic scale.
-- For instance, the following produces a uniform decrease in loudness:
--
-- > xLine 1 (1/8) 3 $ sine 220
xLine :: Behavior Double -> Behavior Double -> Behavior Duration -> Envelope
xLine a b dur = (SC.xLine SC.AR a b dur SC.RemoveSynth *)

-- | Percussive envelope.
percussive :: Behavior Duration -> Behavior Duration -> Envelope
percussive attack release = (SC.envGen SC.AR gate 1 0 1 SC.RemoveSynth gens *)
	where
	gate = 1
	gens = SC.envPerc attack release

{-----------------------------------------------------------------------------
    Tests
------------------------------------------------------------------------------}
-- TODO: Modulation! The following actually shouldn't type check
testModulation = lowpass 4000 $ sawtooth $ 440 + 0.5*sine 10

testPan1 = pan (sine 0.2) $ lowpass 800 $ brownNoise
testPan2 = pan (sine 0.2) $ sawtooth 220

testXLine = xLine 1 (1/8) 3 $ sine 220
testPluck = pluck 440
    where
    pluck freq = percussive 0.02 0.9 $ sine (freq+10*sine 20)



