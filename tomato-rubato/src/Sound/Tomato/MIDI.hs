{-----------------------------------------------------------------------------
    tomato-rubato
------------------------------------------------------------------------------}
module Sound.Tomato.MIDI (
    -- * Synopsis
    -- | Read external MIDI devices, like your keyboard or a controller surface.
    
    -- * Documentation
    withPortMidi, InputDevice, getInputDevice, readKey,
    ) where

import Control.Exception
import Data.Maybe
import Data.IORef

import Sound.PortMidi
import Sound.Tomato.Types
import Sound.Tomato.Music

-- instance Exception PMError

{-----------------------------------------------------------------------------
    Input
------------------------------------------------------------------------------}
-- | Provide a properly initialized MIDI environment.
withPortMidi :: IO a -> IO a
withPortMidi = bracket_ initialize terminate

-- | Represents a MIDI input device.
data InputDevice = InputDevice
    { stream :: PMStream
    , buffer :: IORef [PMEvent]
    } deriving (Eq)

instance Show InputDevice where show = show . stream

-- | Get the default input device. Throws an exception if that fails.
--
-- Note: the \"default\" refers to PortMidi's notion of default device.
getInputDevice :: IO InputDevice
getInputDevice = do
    let err = error "Sound.Tomato.MIDI: Couldn't find input device."
    edev <- openInput . maybe err id =<< getDefaultInputDeviceID
    ref  <- newIORef []
    case edev of
        Left  dev -> do
            -- setFilter dev filterNote
            return $ InputDevice { stream = dev, buffer = ref }
        Right err -> error $ show err

-- | Read the latest MIDI note event if available.
readKey :: InputDevice -> IO (Maybe KeyPress)
readKey dev = do
    ee <- readEvents (stream dev)
    case ee of
        Right NoError -> return ()
        Left  e2      -> modifyIORef (buffer dev) (++e2)
    e  <- readIORef (buffer dev)
    case e of
        []     -> return Nothing
        (x:xs) -> do
            writeIORef (buffer dev) xs
            return . messageToKeyPress $ x

-- | Convert MIDI message into 'KeyPress' event if applicable.
messageToKeyPress :: PMEvent -> Maybe KeyPress
messageToKeyPress e = case status m of
        144 -> Just $
                KeyPress { direction = if data2 m == 0 then KeyUp else KeyDown
                         , pitch    = fromIntegral $ data1 m
                         , velocity = fromIntegral $ data2 m }
        _   -> Nothing
    where
    m = message e
    



    