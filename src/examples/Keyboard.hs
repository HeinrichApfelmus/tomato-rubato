{-----------------------------------------------------------------------------
    tomato-rubato
    
    Example playing from a MIDI keyboard
------------------------------------------------------------------------------}
import Control.Monad

import Sound.Tomato

-- Example sound for the instrument
pluck :: Behavior Frequency -> Sound
pluck freq = percussive 0.02 0.9 $ sawtooth (freq+1*sin 20)

-- Example instrument
exampleMix loop = withSuperCollider $ do
    (addHandler, fire) <- newAddHandler
    
    compileMix $ do
        c1 <- instrument addHandler pluck
        c2 <- effect (bandpass (1000+400*sin 0.5) 1) c1
        speakers c2

    forever $ loop fire

-- Event loop that reads characters from the computer keyboard
mix1 = do
    let loop fire = do
        c <- getChar
        maybe (return ()) (fire . pitchToFrequency) $ letterToPitch c
    exampleMix loop

-- Event loop that reads an external MIDI keyboard
mix2 = withPortMidi $ do
    midi <- getInputDevice
    let loop fire = do
        mkey <- readKey midi
        case mkey of
            Just key | direction key == KeyDown 
                -> fire . pitchToFrequency . pitch $ key
            _   -> return ()
    exampleMix loop

