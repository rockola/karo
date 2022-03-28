# Karo Engine
## Sample run
An example of a Karo Engine session, with annotations.
```
CL-USER> (load "karo.lisp")

; file: /home/ola/src/karo/karo.lisp
...
Karo Engine v1.106 2013-05-29 12:05:00
(c) 2001-2013 Daniel Schell and Ola Rinta-Koski
http://ola.rinta-koski.net/karo/  for more info
T
```
<p>
A Karo Engine session is started by loading `karo.lisp`.

```
CL-USER> (in-package "KARO")
#<PACKAGE "KARO">
```
All Karo Engine definitions are in the package <tt>KARO</tt>.

```
KARO> #[8556]
#<Karo 8556 (0 4 8 1 5 9 2 6 10 3 7 11) 4,4(0) 4,4(1) 4,4(2) 4,4(3)>
KARO> (get-karo 8556)
#<Karo 8556 (0 4 8 1 5 9 2 6 10 3 7 11) 4,4(0) 4,4(1) 4,4(2) 4,4(3)>
```
`#[i]` is a reader macro which returns the _i_th Karo (when working in Z<sub>12</sub>, 
_i_ can range from 1 to 15400). `#[i]` simply calls the function `GET-KARO`.

```
KARO> (as-chords #[8556])
((0 4 8) (1 5 9) (2 6 10) (3 7 11))
```
`(AS-CHORDS karo)` returns the notes of `karo` grouped as chords.

```
KARO> (name-chords #[8556])
("4,4(0)" "4,4(1)" "4,4(2)" "4,4(3)")
```
`(NAME-CHORDS karo)` returns the chords of `karo` using the naming convention
of _i,j(k)_, where _i_ = 1st interval, _j_ = 2nd interval and _k_ = base note.

```
KARO> (find-karos '(4 4 4 4 4 4 4 4))
(#<Karo 8556 (0 4 8 1 5 9 2 6 10 3 7 11) 4,4(0) 4,4(1) 4,4(2) 4,4(3)>)
KARO> (find-karos '(4 3 3 4 4 3 3 4))
(<Karo 6326 (0 3 7 1 5 10 2 6 9 4 8 11) 3,4(0) 3,4(10) 4,3(2) 4,3(4)>
 #<Karo 6356 (0 3 7 1 6 9 2 5 10 4 8 11) 3,4(0) 3,4(6) 4,3(10) 4,3(4)>
 ...
 #<Karo 10518 (0 5 9 1 4 8 2 7 10 3 6 11) 4,3(5) 3,4(1) 3,4(7) 4,3(11)>
 #<Karo 10519 (0 5 9 1 4 8 2 7 11 3 6 10) 4,3(5) 3,4(1) 4,3(7) 3,4(3)>)
```
`(FIND-KAROS model)` returns a list of the Karos that match `model`.
A match is found when the intervals of the Karo notes written as chords
match pairwise with the intervals specified in `model`. 
Chord order is not important.

```
KARO> (karo-class #[8556])
:ROTATIONAL
KARO> (karo-class #[10519])
:SYMMETRIC
```
`(KARO-CLASS karo)` returns one of three values, depending on the symmetricity of _karo_:
* `:ROTATIONAL` - all intervals are equal
* `:SYMMETRIC` - the interval list is symmetric eg. 34 43 43 34
* `:COMMON` - all other cases 

```
KARO> (connect #[10519])
(11 ((1 4 8) (0 5 9) (3 6 10) (2 7 11)) ((1 4 8) (0 5 9) (3 6 10) (2 7 11))
 (3 5 3) ((-1 1 1) (3 1 1) (-1 1 1)))
KARO> (connect #[10519] :reject-translation nil)
(11 ((1 4 8) (0 5 9) (3 6 10) (2 7 11)) ((1 4 8) (0 5 9) (3 6 10) (2 7 11))
 (3 5 3) ((-1 1 1) (3 1 1) (-1 1 1)))
KARO> (connect #[10519] :fixed-order t)
(11 ((0 5 9) (1 4 8) (-1 2 7) (-2 3 6)) ((0 5 9) (1 4 8) (11 2 7) (10 3 6))
 (3 5 3) ((1 -1 -1) (-2 -2 -1) (-1 1 -1)))
```
`(CONNECT karo &KEY (REJECT-TRANSLATION T) (FIXED-ORDER NIL))` returns the
best connection for the chords in _karo_.

The return value is a list of the form `(a b c d)`:
* _a_ = total of intervals
* _b_ = chords of the connection
* _c_ = chords of the connection with all notes in the positive range
* _d_ = intervals of the connection

```
KARO> (draw #[10519])

"/tmp/karo-10519.xml" 
"/tmp/karo-10519.eps" 
"/tmp/karo-10519.svg" 
NIL
```
`(DRAW karo &KEY (DIRECTORY "/tmp"))` outputs various diagrams of _karo_ to
_directory_.

```
KARO> (midi-output #[10519])
; Evaluation aborted on #<UNDEFINED-FUNCTION MIDI-OUTPUT {1006B919A3}>.
KARO> (load "midi.lisp")
STYLE-WARNING: Implicitly creating new generic function MIDI::WRITE-EVENT.
...
T
KARO> (midi-output #[10519])
; Evaluation aborted on #&lt;UNDEFINED-FUNCTION MIDI-OUTPUT {1006D476E3}>.
KARO> (load "karo.lisp")

; file: /home/ola/src/karo/karo.lisp
...
Karo Engine v1.106 2013-05-29 12:05:00
(c) 2001-2013 Daniel Schell and Ola Rinta-Koski
http://ola.rinta-koski.net/karo/  for more info
T
KARO> (midi-output #[10519])
"/tmp/karo-10519.mid"
```
`(MIDI-OUTPUT karo &KEY (DIRECTORY "/tmp"))` outputs _karo_ as a MIDI file to
_directory_. Note that in order to use this feature, `midi.lisp` has to be loaded
before `karo.lisp`.

```
KARO> (csound-file "/tmp/karo-10519.csd" #[10519])
"/tmp/karo-10519.csd"
```
`(CSOUND-FILE filename karo))` outputs `karo` as a Csound file named by `filename`.
This is mainly useful when working outside Z<sub>12</sub>.


&copy; 2013 Daniel Schell & Ola Rinta-Koski


