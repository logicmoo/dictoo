:- module(dictoo_tests, []).

:- if(exists_source(library(jpl))).
:- use_module(library(jpl)).
:- endif.

:- if(exists_source(library(must_trace))).
:- use_module(library(must_trace)).
:- endif.

:- autoload.

:- use_module(library(dictoo)).
:- use_module(library(dictoo_declarations)).

test(0):- jpl_get('java.awt.Cursor', 'NE_RESIZE_CURSOR', $cursor.value ).

test(1):- $cursor.value == 7.

test(1.1):- $cursor.value = 5.

test(1.2):- $cursor.value == 5.

test(2):- jpl_new(array(class([java,lang],['String'])), [for,while,do,if,then,else,try,catch,finally], $my_array.value).

test(3):- writeln($my_array.value.3 = then).

test(4):- writeln(3-5 = $my_array.value.(3-5)).

test(5):- writeln(length = $my_array.value.length).

all_tests:- forall(clause(test(X),Body),(dmsg(test(X)),must(Body))).

:- listing(test(_)).

:- user: $foo.set(v3{ x: -1.0 , y:0.0, z:1.0}).

