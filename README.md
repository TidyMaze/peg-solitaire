# Peg-solitaire

A peg solitaire solver in scala.

Run it, it will solve the english board and print all solutions (see [output.txt](output.txt) for the first 1000 solutions).

```
SOLUTION 1: 3x1-3x3,1x2-3x2,2x0-2x2,4x0-2x0,3x2-1x2,0x2-2x2,4x2-4x0,6x2-4x2,2x3-2x1,2x0-2x2,0x3-2x3,2x3-2x1,4x3-4x1,4x0-4x2,6x3-4x3,4x3-4x1,2x5-2x3,0x4-2x4,2x4-2x2,2x1-2x3,2x3-4x3,4x4-4x2,4x1-4x3,6x4-4x4,3x4-5x4,4x6-4x4,4x3-4x5,2x6-4x6,4x6-4x4,5x4-3x4,3x5-3x3

##ooo## ##ooo## ##ooo## ##.oo## ##o..## ##o..## ##o..## ##o.o## ##o.o## ##o.o## ##..o## ##..o## ##..o## ##..o## ##...## ##...## ##...## ##...## ##...## ##...## ##...## ##...## ##...## ##...## ##...## ##...## ##...## ##...## ##...## ##...## ##...## ##...##
##ooo## ##o.o## ##o.o## ##..o## ##..o## ##..o## ##..o## ##...## ##...## ##o..## ##...## ##...## ##o..## ##o.o## ##o..## ##o..## ##o.o## ##o.o## ##o.o## ##o.o## ##..o## ##..o## ##..o## ##...## ##...## ##...## ##...## ##...## ##...## ##...## ##...## ##...##
ooooooo ooo.ooo o..oooo o.ooooo o.ooooo oo..ooo ..o.ooo ..o..oo ..o.o.. ....o.. ..o.o.. ..o.o.. ....o.. ....... ....o.. ....o.. ....... ....... ....... ..o.... ....... ....... ....o.. ....... ....... ....... ....... ....... ....... ....... ....... .......
ooo.ooo ooooooo ooooooo ooooooo ooooooo ooooooo ooooooo ooooooo ooooooo oo.oooo oo.oooo ..ooooo ...oooo ...o.oo ...o.oo ...oo.. ...o... ..oo... ..oo... ...o... ..oo... ....o.. ....... ....o.. ....o.. ....o.. ....o.. ....... ....... ....... ....... ...o...
ooooooo ooooooo ooooooo ooooooo ooooooo ooooooo ooooooo ooooooo ooooooo ooooooo ooooooo ooooooo ooooooo ooooooo ooooooo ooooooo ooooooo oo.oooo ..ooooo ...oooo ...oooo ...oooo ...o.oo ...o.oo ...oo.. .....o. ....oo. .....o. .....o. ....oo. ...o... .......
##ooo## ##ooo## ##ooo## ##ooo## ##ooo## ##ooo## ##ooo## ##ooo## ##ooo## ##ooo## ##ooo## ##ooo## ##ooo## ##ooo## ##ooo## ##ooo## ##ooo## ##.oo## ##.oo## ##.oo## ##.oo## ##.oo## ##.oo## ##.oo## ##.oo## ##.oo## ##.o.## ##.oo## ##.oo## ##.o.## ##.o.## ##...##
##ooo## ##ooo## ##ooo## ##ooo## ##ooo## ##ooo## ##ooo## ##ooo## ##ooo## ##ooo## ##ooo## ##ooo## ##ooo## ##ooo## ##ooo## ##ooo## ##ooo## ##ooo## ##ooo## ##ooo## ##ooo## ##ooo## ##ooo## ##ooo## ##ooo## ##ooo## ##oo.## ##oo.## ##..o## ##...## ##...## ##...##
Speed: 1729K op/s out of 55311000K nodes. 1759778 solutions so far.
```

Current speed after some optimisationm: 2 000 000 state/s (one state is a game node, can be leaf as well).
