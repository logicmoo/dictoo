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

?- jpl_get('java.awt.Cursor', 'NE_RESIZE_CURSOR', $cursor.value ).
true.

?- $cursor.value == 7.
true.

?- jpl_new(array(class([java,lang],['String'])), [for,while,do,if,then,else,try,catch,finally], $my_array.value).
true.

?- writeln($my_array.value.3 = then).
if=then
true.

?- writeln(3-5 = $my_array.value.(3-5)).
3-5=[if, then, else]
true.

?- writeln(length = $my_array.value.length).
length=9
true.


```

Another Pack  called [dictoo](https://github.com/logicmoo/dictoo) 
adds better OO API on these values



[BSD 2-Clause License](LICENSE.md)

Copyright (c) 2017, 
Douglas Miles <logicmoo@gmail.com>
All rights reserved.


