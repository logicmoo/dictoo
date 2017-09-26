:- module(example, []).

:- use_module(library(must_trace)).
:- use_module(library(dictoo_declarations)).

:- set_prolog_flag(must_saftey,3).
:- set_prolog_flag(must_debug,0).
:- set_prolog_flag(must_speed,0).


:- set_prolog_flag(access_level,system).
:- set_prolog_flag(dictoo_syntax,true).

% :- debug(dictoo(decl)).

$current_file.value = X :- prolog_load_context(file,X).

modfoo1: $ foo1.value = X :- bar1 = X.

% :- rtrace,trace.
$modfoo2:foo2.value = X :- bar2 = X.
:- notrace.

$flags.Name = X :- current_prolog_flag(Name,X).

:- writeln($current_file.value).
:- nodebug(dictoo(decl)).

:- listing(dot_cache:dictoo_decl/8).

all_tests:- forall(test1(_),true).
test1(0):- $test.set("this is a test1").
test1(1):- writeln($test.current()).


:- fixup_exports.

:- listing(test1(_)).
:- user:use_module(library(dictoo)).
:- user:use_module(library(gvar_syntax)).
%:- debug(dictoo(goal_expand)).
%:- debug(dictoo(decl)).


