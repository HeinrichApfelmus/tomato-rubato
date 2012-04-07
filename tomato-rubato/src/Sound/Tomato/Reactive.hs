{-----------------------------------------------------------------------------
    tomato-rubato
------------------------------------------------------------------------------}
module Sound.Tomato.Reactive (
    -- * Synopsis
    -- | Programming with events and behaviors (time-varying values).
    -- 
    -- TODO: Use reactive-banana for proper abstractions.
    
    -- * Event
    Event, newEvent,
    module Data.Functor,

    -- * Timers
    Timer, withTimer, onTimer, setInterval, stopTimer,
    
    -- * Internal
    Event(..)
    ) where

import Control.Applicative
import Control.Concurrent
import Control.Exception (bracket)
import Control.Monad

import Data.IORef
import Data.Functor
import qualified Data.Map as Map
import Data.Unique

import Sound.Tomato.Types

{-----------------------------------------------------------------------------
    Event
------------------------------------------------------------------------------}
-- | Stream of event occurrences.
newtype Event a = Event { addHandler :: AddHandler a }

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

-- | Make an 'Event' from an 'AddHandler'
fromAddHandler :: AddHandler a -> IO (Event a)
fromAddHandler = return . Event

-- | Make a new event and a way to fire it.
newEvent :: IO (Event a, a -> IO ())
newEvent = do
    (a,f) <- newAddHandler 
    e <- fromAddHandler a
    return (e,f)

instance Functor Event where
    fmap f (Event addHandler) = Event $ \g -> addHandler (g . f)

-- | Keep only values of the form 'Just'.
filterJust :: Event (Maybe a) -> Event a
filterJust (Event addHandler) = Event $ \g -> addHandler (maybe (return ()) g)


{-----------------------------------------------------------------------------
    Timer
------------------------------------------------------------------------------}
-- | A timer fires an event in regular intervals
data Timer = Timer (Event (), () -> IO ()) (IORef (Maybe ThreadId))

-- | Create a timer (with automatic deallocation).
withTimer :: (Timer -> IO a) -> IO a
withTimer = bracket init stopTimer
    where init = Timer <$> newEvent <*> newIORef Nothing

-- | Set interval and start the 'Timer'
setInterval :: Timer -> Time -> IO ()
setInterval t@(Timer (_,fire) m) interval = do
    stopTimer t
    threadID <- forkIO $ forever $ do
        fire ()
        threadDelay (ceiling $ 1e6 * interval)
    writeIORef m $ Just threadID

-- | Retrieve event from timer
onTimer :: Timer -> Event ()
onTimer (Timer (e,_) _) = e
    
-- | Stop the timer from doing anything
stopTimer :: Timer -> IO ()
stopTimer (Timer _ m) = maybe (return ()) killThread =<< readIORef m
