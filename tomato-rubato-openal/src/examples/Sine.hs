{-----------------------------------------------------------------------------
    tomato-rubato
    
    Example
------------------------------------------------------------------------------}
import Sound.Tomato.Speakers

-- simple envelope modulated sine wave
sineMod :: Float -> IO ()
sineMod freq = fromPure $
    \t -> 0.4 * sine (freq*t) * sine (15*t) * exp (negate $ t/a)
    where
    sine t = sin (2*pi*t)
    a      = 0.5

type Time = Float -- in seconds

-- | Play wave form given by a pure function
fromPure :: (Time -> Sample) -> IO ()
fromPure f = withSpeakers sampleRate 512 $ \s -> playSamples s sound
    where
    sampleRate  = 22050
    dt          = 1 / sampleRate -- time in seconds of a single sample
    sound       = [f (dt*fromIntegral t) | t <- [0..]]