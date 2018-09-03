# ANSI escape sequence parser (also known as ECMA-48 / ANSI X3.64 / ISO/IEC 6429)

## Read this first

The first thing to understand when it comes to terminals is that [terminfo](https://en.wikipedia.org/wiki/Terminfo) (and [termcaps](https://en.wikipedia.org/wiki/Termcap)) exist.

The second thing to understand is that they exist for a reason. Various terminals and terminal emulators come with their own set of escape sequences. For example, `curses`-based software typically uses terminfo (or `termcap`) to output the correct codes for whatever terminal environment it is running in.

This package basically just plows ahead and directly parses things which are directly defined in the main ECMA-48 standard. Never-the-less, it may be helpful to understand that this is not the whole story.

## What is this useful for?

My impetus for creating this package is to use it in automating terminal software (interacting via a pseudo-terminal; see [posix-pty](http://hackage.haskell.org/package/posix-pty)).

The monad transformer `ParsecT e s m`, provided by megaparsec, allows one to mix (caveat emptor) application state, logic and even effects directly into the parser.

This also allows one to branch, altering the future behaviour of the parser based on past parse results. For example, you may care about this if you would like to support some kind of mode switching (e.g. ISO/IEC 2022).

## The landscape

The history of terminal escape sequences is super confusing (at least, to me). Standards are called by various names; have various revisions; they overlap and interact in non-trivial ways; they have subtly different raisons d'être... and so on.

I'll make no attempt to go into historical details, but it is helpful to at least have a rough sort of mental map of how
the standards relate to each other in order to orient you.

What follows is my best attempt at providing a little orientation and should not be taken as authoritative.
In fact, it is probably wrong on several points. (I personally have no great passion for arcane control codes.
Never-the-less, I am grateful to anyone with patience to find and point out inaccuracies.)

### Standards

* [**Unicode**](https://en.wikipedia.org/wiki/ASCII#Character_set)
* [**ASCII**](https://en.wikipedia.org/wiki/ASCII#Character_set)
  * A 7-bit code
  * Includes graphical as well as control characters (but no sequences)
  * Revisions:
    * ASA X3.4-1963
    * ASA X3.4-1965
    * USAS X3.4-1967
    * USAS X3.4-1968
    * ANSI X3.4-1977
    * ANSI X3.4-1986
    * ANSI X3.4-1986 (R1992)
    * ANSI X3.4-1986 (R1997)
    * ANSI INCITS 4-1986 (R2002)
    * ANSI INCITS 4-1986 (R2007)
    * ANSI INCITS 4-1986 (R2012)
  * Also known as: ECMA-6, ISO/IEC 646, and ITU T.50
* UTF-7
  * An encoding of unicode text using a (somewhat human readable) stream of 7-bit ASCII characters
* Extended ASCII
  * An 8-bit code
  * Sometimes confused with true ASCII
  * Adds additional characters in the range 128 to 255
  * Multiple codepages with different characters in the extended region
    * Typical code pages: ([1](https://en.wikipedia.org/wiki/Extended_ASCII#/media/File:Table_ascii_extended.png), [2]())
  * Also known as: ISO/IEC 8859, ECMA-94
* [**UTF-8**](https://en.wikipedia.org/wiki/UTF-8#Codepage_layout)
  * An 8-bit code
  * Modern and popular encoding of unicode text
  * Fully backward compatible with 7-bit ASCII
  * Somewhat backward compatible with Extended ASCII via either fall back or translation
  * Used in many 
* **ECMA-48**
  * A 7-bit code
  * Compatible with 7-bit ASCII
  * Also known as: ISO/IEC 6429
  * An evolution from ECMA-6, a ratified form of ISO/IEC 646.
* ISO/IEC 2022
  * An encoding for switching between character sets
  * Equivalent to ECMA-35. According to "someone on the internet":
    * ECMA-35, 1st Edition (1971) apparently described the character set facilities of the VT100 exactly, but was lost (!).
    * ECMA-35, 2nd edition, closer to VT220
  * Has disadvantages compared to UTF-8
* ANSI X3.64
  * Appears to have combined (earlier versions perhaps?) of ECMA-48 and ECMA-35 into one standard (?)
  * Finally withdrawn and replaced by ISO/IEC 6429 (equivalent to ECMA-48 5th edition)

    ¯\_(ツ)_/¯

Honestly. I don't know. The standards to probably pay attention to has been bolded.
That is to say, everything else is probably safe to ignore and included only for context.

### Terminology

* [Terminal](https://en.wikipedia.org/wiki/Computer_terminal)
* [Terminal emulator](https://en.wikipedia.org/wiki/Terminal_emulator)
* [Pseudoterminal](https://en.wikipedia.org/wiki/Pseudoterminal)
* [Character encoding](https://en.wikipedia.org/wiki/Character_encoding)
* [Code point](https://en.wikipedia.org/wiki/Code_point)
* [Code page](https://en.wikipedia.org/wiki/Code_page)
* **Control function**
* **Control character**
* **Control sequence**
* **Escape sequence**
* **Control string**

## Coverage

| ISO-2022

## Reference

* [Linux console codes](https://linux.die.net/man/4/console_codes)

