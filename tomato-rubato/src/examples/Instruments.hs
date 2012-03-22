{-----------------------------------------------------------------------------
    tomato-rubato
    
    Let's synthesize some instruments!
------------------------------------------------------------------------------}

import Control.Applicative
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Trans.Maybe
import System.IO

import Sound.Tomato


mapEvent :: (a -> b) -> Event a -> Event b
mapEvent f addHandler g = addHandler (g . f)

filterJust :: Event (Maybe a) -> Event a
filterJust addHandler g = addHandler (maybe nop g)

-- Test an instrument
auditionMidi sound = withSuperCollider $ keyBoardLoop $
    \keydown -> compileMix $ speakers =<< instrument (toFreq keydown) sound
    
    where
    toFreq = filterJust . mapEvent toKey
    toKey key
        | direction key == KeyDown = Just . pitchToFrequency . pitch $ key
        | otherwise = Nothing

-- | Event loop that queries both the MIDI keyboard and the computer keyboard.
-- The event loop is run after the argument has been called.
keyBoardLoop :: (Event KeyPress -> IO b) -> IO b
keyBoardLoop setup = withPortMidi $ do
    (addHandler, fire) <- newAddHandler
    midi <- getInputDevice
    setup addHandler
    
    let        
        readComputer = runMaybeT $ do
            guard =<< liftIO (liftA2 (&&) (not <$> hIsEOF stdin) (hIsReadable stdin))
            guard =<< liftIO (hReady stdin)
            pitch <- MaybeT $ letterToPitch <$> getChar
            return $ KeyPress { pitch = pitch, direction = KeyDown, velocity = 80 }

    forever $ do
        maybe nop handleKey =<< readKey midi
        maybe nop fire =<< readComputer

nop = return ()

{-----------------------------------------------------------------------------
    Some instrument
------------------------------------------------------------------------------}
testInstrument = auditionMidi instrument1
    where
    instrument1 :: Behavior Frequency -> Sound
    instrument1 freq
        = gain (-20) $ percussive 0.02 0.8
        $ lowpass (10 * freq)
        $ mix [square $ (1 + 8e-3*sine 0.25) * freq]


type Semitones = Double
-- Adjust a given frequency by a certain number of semitones
detune :: Semitones -> Behavior Frequency -> Behavior Frequency
detune s freq = constant (2**(s/12)) * freq





