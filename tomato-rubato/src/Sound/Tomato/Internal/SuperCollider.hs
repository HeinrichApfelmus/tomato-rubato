{-----------------------------------------------------------------------------
    tomato-rubato
------------------------------------------------------------------------------}
module Sound.Tomato.Internal.SuperCollider (
    -- * Synopsis
    -- | Internal module. Administrative stuff for Supercollider.
    
    initialize,
    withSuperCollider,
    
    module Sound.SC3,
    module Sound.SC3.UGen.Noise.ID,
        
    ) where

import Control.Exception
import System.Process (system)

import Sound.SC3
import Sound.SC3.UGen.Noise.ID


-- | Start the SuperCollider server.
-- TODO: Only works on the default installation for MacOS X right now.
-- TODO: We actually want to run this as a demon.
initialize =
    system "/Applications/SuperCollider/scsynth -u 57110 -U /Applications/SuperCollider/plugins"

-- | Add the default group 1 and remove it afterwards
withSuperCollider :: IO a -> IO a
withSuperCollider = bracket_
    (withSC3 $ \fd -> send fd (g_new [(1, AddToTail, 0)]))
    (withSC3 $ \fd -> send fd (n_free [1]) >> send fd (g_new [(1, AddToTail, 0)]))
