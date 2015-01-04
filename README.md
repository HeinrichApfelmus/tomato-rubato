## What is tomato-rubato?

Tomato-rubato is an easy to use library for live audio programming in Haskell. Instant gratification from your speakers by typing into GHCi!

**WARNING: This project is still in pre-alpha stage. For adventurers only.**

See the **[project homepage][homepage]** for **documentation**, **examples** and so on.

  [homepage]: http://haskell.org/haskellwiki/Tomato-rubato
  [pure]: http://en.wikipedia.org/wiki/Pure_Data

## Compilation and Installation

### tomato-rubato.cabal

Prerequisites:

* The [OpenAL bindings][openal] for Haskell are used to output sound.

* To install the tomato-rubato-openal libary, simply type

        cd tomato-rubato-openal && cabal install

  [openal]: http://hackage.haskell.org/package/OpenAL

### tomato-rubato.cabal

Prerequisites:

* [SuperCollider 3][sc] serves as backend to generate the actual audio. Simply *install* one of the [prepackaged version][sc-downloads] on your computer and *start the server* before using tomato-rubato. (Let me know if you have any problems with this step.) The [corresponding Haskell bindings][sc-haskell] are installed by simply typing

        cabal install hsc3-0.9

* [PortMidi][] is used to read from external MIDI devices. You have to *install* the C library on your system and then install the [corresponding Haskell bindings][portmidi-haskell] by typing

        cabal install PortMidi

* To build the tomato-rubato library, simply type

        cd tomato-rubato && cabal configure && cabal build


  [sc]: http://supercollider.sourceforge.net/
  [sc-downloads]: http://supercollider.sourceforge.net/downloads/
  [sc-haskell]: http://hackage.haskell.org/package/hsc3
  [portmidi]: http://portmedia.sourceforge.net/portmidi/
  [portmidi-haskell]: http://hackage.haskell.org/package/PortMidi

## Contribute

Send me your examples, bindings, problems, suggestions, etc!


