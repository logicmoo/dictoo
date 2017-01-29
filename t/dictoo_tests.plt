
:- use_module(library(gvar_syntax)).

:- $foo.value = 1.

test(0):- \+ $foo.value = 2.

test(1):- writeln($foo.value).

test(2):- writeln($foo.get()).

test(3):- writeln($foo.clear()).

test(4):- $bar.set(2), $foo.set($bar.get()), test(1).

test(5):- writeln($foo.set(33).set(34).value).

test(6):- $baz.set(point{x:vx,y:vy,z:vz}).

test(7):- writeln($baz.get().z).

test(8):- $baz.set(point{ x: ($foo.get()) , y:vy, z:vz}).

test(9):- writeln($baz.value.x).

test(10):- $baz.set($baz.value.put(y,yYYYY)).

test(11):- $baz.value.y == yYYYY.

all_tests:- forall(test(_),true).


