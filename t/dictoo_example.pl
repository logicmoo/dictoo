:- module(example, [test/1]).

:- use_module(library(yall)).
:- user:use_module(library(gvar_syntax)).
:- use_module(library(hook_database)).
:- use_module(library(must_trace)).
:- autoload.
:- use_module(library(dictoo_declarations)).


:- set_prolog_flag(must_saftey,3).
:- set_prolog_flag(must_debug,0).
:- set_prolog_flag(must_speed,0).


:- set_prolog_flag(access_level,system).
%:- set_prolog_flag(dictoo_syntax,true).


$current_file.value = X :- prolog_load_context(file,X).

modfoo1: $ foo1.value = X :- bar1 = X.

% :- rtrace,trace.
$modfoo2:foo2.value = X :- bar2 = X.
:- notrace.

$flags.Name = X :- current_prolog_flag(Name,X).

:- writeln($current_file.value).

:- listing(dot_cache:dictoo_decl/8).

:-  nb_setval('$mud_prefs',_{realname:dug}).

% :- meta_predicate(ain(0)).

:- dynamic(call_after_logon/1).
:- meta_predicate(call_after_logon(0)).




test(0):- $test.set("this is a test").
test(1):- writeln($test.current()).
test(2):- dict_create(Dict,foo,[key-value]) , X = Dict.key, writeln(X).
test(3):- asserta((call_after_logon((nb_current('$mud_prefs',Prefs),set_player_option(realname, Prefs.realname))))).
test(4):- ain(system_autoexec:(==>(==>(mpred_unload_option(never, $current_file.value))))).

:- fixup_exports.

all_tests:- forall(clause(test(X),Body),(dmsg(test(X)),must(Body))).

:- set_prolog_flag(scope_functions,true).


:- user:use_module(library(dictoo)).
%:- debug(dictoo(goal_expand)).
%:- debug(dictoo(decl)).

foo :- ain(system_autoexec:(==>(==>(mpred_unload_option(never, $current_file.value))))).


:- expand_goal((asserta((call_after_logon((nb_current('$mud_prefs',Prefs),set_player_option(realname, Prefs.realname)))))),OO),
  nl,writeq(OO),nl.

% :- asserta((call_after_logon((nb_current('$mud_prefs',Prefs),set_player_option(realname, Prefs.realname))))).

:- listing(foo).
:- listing(test(_)).


end_of_file.
:- expand_goal(ain(system_autoexec:(==>(mpred_unload_option(never, $current_file.value)))),OO).
:- expand_goal(mpred_unload_option(never, $current_file.value),OO).

:- expand_term(foo :- ain(system_autoexec:(==> (==>mpred_unload_option(never, $current_file.value)))),OO).

% :- debug(dictoo(decl)).


