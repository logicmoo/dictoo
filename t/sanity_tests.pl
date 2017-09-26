:- module(mpred_gvars, []).

:- use_module(library(must_trace)).
:- use_module(library(dictoo_declarations)).
:- use_module(library(dictoo)).
:- use_module(library(bugger)).
:- ensure_loaded(library(ansimesg)).

:- set_prolog_flag(must_saftey,3).
:- set_prolog_flag(must_debug,0).
:- set_prolog_flag(must_speed,0).

:- set_prolog_flag(must_type,keep_going).

test(0):- must(\+ fail).

test(1):- must_once(fail).




$current_file.value = X :- prolog_load_context(file,X).

:- writeln($current_file.value).

:- listing(dictoo_decl/7).

all_tests:- forall(test(_),true).


:- fixup_exports.

:- listing(test(_)).
