{-----------------------------------------------------------------------------
    tomato-rubato
    
    Example: Formant synthesis
    We generate sounds that (vaguely) resemble human vowels.
    
    See also:
        Principles behind formant synthesis
        http://www.soundonsound.com/sos/mar01/articles/synthsec.asp
        
        Example software
        http://www.speech.kth.se/wavesurfer/formant/
------------------------------------------------------------------------------}

import Sound.Tomato
import Demonstrate

-- Vowel sounds
-- Ok, ok, they don't sound very realistic.
vowel_i = formantFilter 285 2257 $ source
vowel_e = formantFilter 539 1800 $ source
vowel_a = formantFilter 710 1100 $ source
vowel_u = formantFilter 431 1031 $ source

-- Source oscillator
source = lowpass 1200 $ square 77

-- Several bandpass filter that select resonances found in the human throat
formantFilter f1 f2 osc = gain 20 $ mix $
    [ bandpass f1 (f1/50) osc
    , gain (-5) $ bandpass f2 (f2/75) osc
    , gain (-9) $ bandpass 2500 (2500/100) osc
    , gain (-9) $ bandpass 3500 (3500/150) osc ]
