# Dict-like OO Syntax


Installation using SWI-Prolog 7.1 or later:

    ?- pack_install(dictoo).

  or

    ?- pack_install('https://github.com/logicmoo/dictoo'). 

This module uses [semantic versioning](http://semver.org/).

Source code available and pull requests accepted at
http://github.com/logicmoo/dictoo

```prolog
?- use_module(library(dictoo)).
true.

?- use_module(library(jpl)).
true.

?- 
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

Another Pack  called [dictoo](https://github.com/logicmoo/dictoo) 
adds better OO API on these values



[BSD 2-Clause License](LICENSE.md)

Copyright (c) 2017, 
Douglas Miles <logicmoo@gmail.com>
All rights reserved.


