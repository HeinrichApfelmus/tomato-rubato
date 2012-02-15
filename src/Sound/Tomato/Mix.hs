{-----------------------------------------------------------------------------
    tomato-rubato
------------------------------------------------------------------------------}
module Sound.Tomato.Mix (
    -- * Synopsis
    -- | Sound mixing.
    -- 
    -- While 'Sound.Tomato.Synthesis' is about the creation of individual
    -- instrument sounds, 'Sound.Tomato.Mix' is about putting them together
    -- in an orchestra that can be conducted via, say, MIDI events.
    
    -- * Setting up a Mix
    Audio, Mix, compileMix, withSuperCollider, 
    
    -- * Specifying instruments and effects
    merge, instrument, effect, speakers,
    
    -- * Event Handling
    Event, AddHandler, newAddHandler,
    
    ) where

import Control.Arrow (first, second)
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Trans.State
import Data.Char
import Data.IORef
import Data.Functor
import qualified Data.Map as Map
import Data.Unique

import Demonstrate

import Sound.Tomato.MIDI
import Sound.Tomato.Synthesis
import Sound.Tomato.Theory
import Sound.Tomato.Types

import Sound.SC3 (out, in')
import Sound.SC3.Server
import qualified Sound.Tomato.Internal.SuperCollider as SC
import Sound.OpenSoundControl (OSC)

{- SuperCollider debugging tips:
  Server.default.defaultGroup.queryTree;
-}


{-----------------------------------------------------------------------------
    Supercollider back-end implementation
------------------------------------------------------------------------------}
-- | Provide a properly initialized SuperCollider environment in 
-- which we can compile a mix.
--
-- Note: This function will /not/ start the server, you have to do that yourself.
withSuperCollider :: IO a -> IO a
withSuperCollider = SC.withSuperCollider

-- | Monad for describing a mix.
type Mix a = StateT (GroupID, Audio) IO a
-- instance Monad Mix

-- | A stream of audio data.
type Audio = Int
-- Internally, this is a channel number.

-- | Compile a mix and load it into Supercollider.
-- Note that you have to wrap the result into the 'withSuperCollider' function.
compileMix :: Mix () -> IO ()
compileMix m = flip evalStateT (1000,33) m

-- | Generate a new channel ID
newChannel :: Mix Audio
newChannel = do modify (second (+1)); snd <$> get

stereoOut :: Int -> SC.UGen -> SC.UGen
stereoOut = out . fromIntegral          -- stereo output

stereoIn :: Int -> SC.UGen
stereoIn  = in' 2 SC.AR . fromIntegral  -- stereo input bus

-- | Apply an effect to an audio stream.
effect :: (Sound -> Sound) -> Audio -> Mix Audio
effect f input = do
    output <- newChannel    -- allocate new output channel
    effect' "tomato-effect" f input output  -- apply effect
    return output           -- return output channel

-- | Apply an effect to a channel and send the result to another channel
effect' :: String -> (Sound -> Sound) -> Audio -> Audio -> Mix ()
effect' name f input output = do
    let synthName = name ++ show output
    newSynthDef synthName $ -- create synthesizer definition
        stereoOut output . soundToUGen . f . Sound $ stereoIn input
    group <- newGroup       -- allocate a new group ID
    liftIO $ newNode synthName group [] -- allocate node on the server and add it to group
    return ()

-- | Create a polyphonic instrument that can be triggered via an event.
instrument :: Event Frequency -> (Behavior Frequency -> Sound) -> Mix Audio
instrument addHandler gen = do
    output <- newChannel
    group  <- newGroup
    let synthName = "tomato-instrument" ++ show group
    newSynthDef synthName $
        stereoOut output . soundToUGen . gen $ parameter "freq" 0
    liftIO $ addHandler $ \freq -> do
        -- allocate node on the server and add it to group
        -- this will start the sound
        -- FIXME: we assume that the sound ceases playing by itself
        newNode synthName group [("freq",freq)]
    return output

-- | Route audio stream into loudspeakers.
speakers :: Audio -> Mix ()
speakers input = effect' "tomato-speakers" id input 0 -- write to stereo output channel

-- | Merge several audio streams by mixing them.
merge :: [Audio] -> Mix Audio
merge []  = newChannel -- this channel is silent, but who am I to judge
merge is  = effect (\_ -> foldr1 mix . map (Sound . stereoIn) $ is) undefined
    -- effect that simply mixes the different input channels

{-----------------------------------------------------------------------------
    Supercollider commands
------------------------------------------------------------------------------}
-- send a command to the SuperCollider server
sendCommand :: OSC -> IO ()
sendCommand cmd = withSC3 $ \fd -> send fd cmd

type SynthName = String
-- create a new synthesizer definition
newSynthDef :: SynthName -> SC.UGen -> Mix ()
newSynthDef name gen = liftIO $ do
    withSC3 $ \fd -> async fd $ d_recv (synthdef name gen)
    return ()

type NodeID     = Int
type Parameters = [(String,Double)]
-- create a new node from a synthesizer template
newNode :: SynthName -> GroupID -> Parameters -> IO ()
newNode name group params = sendCommand $
    -- allocate node with fresh ID at the head of  group
    s_new name (-1) AddToHead group params

type GroupID = Int
-- create a new group ID and
-- allocate the group on the SuperCollider server before the previous one
newGroup :: Mix GroupID
newGroup = do
    -- generate new group ID
    current  <- modify (first (+1)) >> fst <$> get
    
    liftIO $ sendCommand $
        -- allocate group on the server
        -- The tree is built forwards.
        g_new [(current,AddToTail,1)]
    return current

{-----------------------------------------------------------------------------
    Utilities
------------------------------------------------------------------------------}
-- | Events are synonymous with facilities where we can register an event handler.
type Event a      = AddHandler a

-- | A facility to register event handlers with. (Taken from reactive-banana)
type AddHandler a = (a -> IO ()) -> IO (IO ())

-- | Build a facility to register and unregister event handlers.
newAddHandler :: IO (AddHandler a, a -> IO ())
newAddHandler = do
    handlers <- newIORef Map.empty
    let addHandler k = do
            key <- newUnique
            modifyIORef handlers $ Map.insert key k
            return $ modifyIORef handlers $ Map.delete key
        runHandlers x =
            mapM_ ($ x) . map snd . Map.toList =<< readIORef handlers
    return (addHandler, runHandlers)
