# SWI-Prolog Pack that adds a new Global Variable syntax to Prolog


# Installation

Using SWI-Prolog 7.1 or later:

    ?- pack_install(gvar_syntax).

This module uses [semantic versioning](http://semver.org/).

Source code available and pull requests accepted at
http://github.com/logicmoo/gvar_syntax

```prolog
?- use_module(library(gvar_syntax)).
true.

?- $foo.value = 1.
true.

?- $foo.value = 2.
false.

?- writeln($foo.value).
1
true.

?- writeln($foo.get()).
1
true.

?- $foo.clear().
true.


?- writeln($foo.value).
_8350

?- writeln($bar.set(2).value).
2

?- $foo.value = xxxxxxxx.
true.

?- $baz.set(point{ x: ($foo.get()) , y:vy, z:vz}).
true.

?- writeln($baz.value.x).
xxxxxxxx
true.

?- writeln($baz.x). % will error as you havented acceed the value

```
Another Pack  called [udt](https://github.com/logicmoo/udt) add better OO API on these values


@author Douglas Miles <logicmoo@gmail.com>
```
@license None
```
