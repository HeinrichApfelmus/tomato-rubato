{-----------------------------------------------------------------------------
    tomato-rubato
------------------------------------------------------------------------------}
{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}
module Sound.Tomato.Synthesis (
    -- * Synopsis
    -- | Basic combinators for sound synthesis and processing.
    
    -- * Sound data
    Sound(..), mix, play, render,
    
    -- * Parameters
    Behavior(..), parameter, Duration,
    sineMod, HasOscillation(..),
    
    -- * Fundamental wave forms and sounds
    sineOsc, sawtooth, brownNoise,
    
    -- * Sound processing and filters
    lowpass, highpass, bandpass, delay, pan,
    
    -- * Envelopes
    Envelope, xLine, percussive,
    
    -- * Internal
    ) where

import Control.Concurrent
import Data.Ratio

import Demonstrate

import qualified Sound.Tomato.Internal.SuperCollider as SC
import qualified Sound.SC3.UGen.UGen as SC
import Sound.Tomato.Types

liftNewtype1 on un f = on . f . un
liftNewtype2 on un f x y = on $ f (un x) (un y)

{-----------------------------------------------------------------------------
    Sound data
------------------------------------------------------------------------------}
-- | Represents a sound.
-- 
-- Technical details: 44.1kHz, stereo. 
newtype Sound = Sound { soundToUGen :: SC.UGen }
-- Represented as a two-channel unit generator

-- | Lifting functions for sounds
liftSound1 = liftNewtype1 Sound soundToUGen
liftSound2 = liftNewtype2 Sound soundToUGen

-- | Combine to sounds by mixing them.
mix :: Sound -> Sound -> Sound
mix = liftSound2 (+)

-- | Play a sound on the speakers.
-- Press any key to interrupt.
play :: Sound -> IO ()
play sound = SC.withSuperCollider $ do
    SC.audition $ SC.out 0 (0.2 * soundToUGen sound)
    getChar
    return ()

instance Demonstrate Sound where
    demo sound = SC.withSuperCollider $ do
        SC.audition $ SC.out 0 (0.2 * soundToUGen sound)
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
newtype Behavior a = B { getUGen :: SC.UGen } deriving (Eq,Show)

liftBehavior1 :: (SC.UGen -> SC.UGen) -> Behavior a -> Behavior a 
liftBehavior1 = liftNewtype1 B getUGen
liftBehavior2 = liftNewtype2 B getUGen

-- | Numeric instances for behaviors
instance Num (Behavior Double) where
    (+) = liftBehavior2 (+)
    (-) = liftBehavior2 (-)
    (*) = liftBehavior2 (*)
    fromInteger = B . SC.constant
    abs    = undefined
    signum = undefined

instance Fractional (Behavior Double) where
    (/) = liftBehavior2 (/)
    fromRational r = fromInteger (numerator r) / fromInteger (denominator r)

-- | Sine wave modulation.
-- Use 'sine' for convenient overloading.
sineMod :: Behavior Frequency -> Behavior Frequency
sineMod = liftBehavior1 $ \freq -> SC.sinOsc SC.AR freq (SC.constant 0)

-- | Convenient overloading for modulation parameters and sound wave forms
class HasOscillation a where
    sine :: Behavior Frequency -> a

instance HasOscillation (Behavior Frequency) where sine = sineMod

-- | Represents a time duration in seconds.
type Duration = Double

-- | Named parameter.
parameter :: Name -> Double -> Behavior Double
parameter name initialValue = B $ SC.control SC.KR name initialValue

{-----------------------------------------------------------------------------
    Fundamental wave forms and sounds
------------------------------------------------------------------------------}
-- | Sine oscillator.
-- Use 'sine' for convenient overloading.
--
-- Sounds like a free telephone line.
sineOsc :: Behavior Frequency -> Sound
sineOsc freq = Sound $ monoToStereo $ SC.sinOsc SC.AR (getUGen freq) (SC.constant 0)

instance HasOscillation Sound where sine = sineOsc

-- | Sawtooth oscillator.
-- 
-- Rasping. Best used with a lowpass to smooth out the harsh high frequencies.
sawtooth :: Behavior Frequency -> Sound
sawtooth freq = Sound $ monoToStereo $ SC.saw SC.AR (getUGen freq)

-- | Brown noise (incoherent)
-- 
-- Generates brown noise, i.e. the spectrum falls off in power by 6 dB per octave.
-- Reminds me of rain or a flowing river, but harsher.
brownNoise :: Sound
brownNoise = Sound $ monoToStereo $ SC.brownNoise (0 :: Int) SC.AR

{-----------------------------------------------------------------------------
    Sound processing with filters
------------------------------------------------------------------------------}
-- | Lowpass filter.
-- Attenuates frequencies above the given one. 12 dB / octave.
lowpass :: Behavior Frequency -> Sound -> Sound
lowpass  freq = liftSound1 $ \ugen -> SC.lpf ugen (getUGen freq)

-- | Highpass filter.
-- Attenuates frequencies below the given one. 12 dB / octave.
highpass :: Behavior Frequency -> Sound -> Sound
highpass freq = liftSound1 $ \ugen -> SC.hpf ugen (getUGen freq)

-- | Bandpass filter. (12 dB / octave)
-- 
-- > bandpass frequency rq 
bandpass :: Behavior Frequency -> Behavior Double -> Sound -> Sound
bandpass freq rq = liftSound1 $ \ugen -> SC.bpf ugen (getUGen freq) (getUGen rq)

-- | Delay the sound by a specified amount of time.
-- Useful for echo and chorus effects.
-- Maximum delay time is 0.1 seconds.
delay :: Behavior Duration -> Sound -> Sound
delay duration = liftSound1 $ \ugen -> SC.delayN ugen (SC.constant 0.1) (getUGen duration)

-- | Balance a stereo sound.
-- Input from -1 (left speaker) to +1 (right speaker)
--
-- Note: This function is currently buggy.
balance :: Behavior Double -> Sound -> Sound
balance position sound = Sound $ SC.balance2 l r (getUGen position) (SC.constant 1)
    where (l,r) = getStereoChannels $ soundToUGen sound

{-----------------------------------------------------------------------------
    SuperCollider utilities
------------------------------------------------------------------------------}
-- | Retrieve the two stereo channels of a multi-channel-expanded sound source
getStereoChannels :: SC.UGen -> (SC.UGen, SC.UGen)
getStereoChannels ugen
    | isStereo ugen = let [u1,u2] = SC.mceExtend 2 ugen in (u1,u2)
    | otherwise     = error "Sound.Tomato.Synthesis: expecting stereo sound"

isStereo ugen = SC.isMCE ugen && SC.mceDegree ugen == 2
isMono   ugen = (SC.isMCE ugen && SC.mceDegree ugen == 1) || (not $ SC.isMCE ugen)

-- | Convert a mono UGen to a stereo UGen by duplicating it
monoToStereo :: SC.UGen -> SC.UGen
monoToStereo ugen
    | isMono ugen   = SC.mce2 ugen ugen
    | otherwise     = error "Sound.Tomato.Synthesis: expecting mono generator"

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
xLine a b dur =
    liftSound1 (SC.xLine SC.AR (getUGen a) (getUGen b) (getUGen dur) SC.RemoveSynth *)

-- | Percussive envelope.
percussive :: Behavior Duration -> Behavior Duration -> Envelope
percussive attack release =
    liftSound1 (SC.envGen SC.AR gate 1 0 1 SC.RemoveSynth gens *)
	where
	gate = 1
	gens = SC.envPerc (getUGen attack) (getUGen release)

{-----------------------------------------------------------------------------
    Tests
------------------------------------------------------------------------------}
testModulation = lowpass 4000 $ sawtooth $ 440 + 0.5*sine 10

testPan1 = balance (sine 0.2) $ lowpass 800 $ brownNoise
testPan2 = balance (sine 0.2) $ sine 220

testXLine = xLine 1 (1/8) 3 $ sine 220
testPluck = pluck 440
    where
    pluck freq = percussive 0.02 0.9 $ sine (freq+10*sine 20)



