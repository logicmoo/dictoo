:- module(example, [test/1]).


/*
:- set_prolog_flag(access_level,system).
:- use_module(library(yall)).
:- user:use_module(library(gvar_syntax)).
:- user:use_module(library(qvars)).
:- user:use_module(library(dictoo)).
:- use_module(library(hook_database)).
:- use_module(library(must_trace)).
*/

:- use_module(library(dictoo_declarations)).


:- set_prolog_flag(must_saftey,3).
:- set_prolog_flag(must_debug,0).
:- set_prolog_flag(must_speed,0).




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
% :- debug(gvar(syntax)).
test(3):- asserta((call_after_logon((nb_current('$mud_prefs',Prefs),set_player_option(realname, Prefs.realname))))).
test(4):- get_xy(X,Y), X.unify()=Y.unify().
:- nodebug(gvar(syntax)).
test(5):- ain(system_autoexec:(==>(==>(mpred_unload_option(never, $current_file.value))))).


all_tests:- forall(clause(test(X),Body),(dmsg(test(X)),must(Body))).



:- user:use_module(library(dictoo)).
%:- debug(dictoo(goal_expand)).
%:- debug(dictoo(decl)).

foo :- ain(system_autoexec:(==>(==>(mpred_unload_option(never, $current_file.value))))).


:- freeze(Prefs,throw(nonvar(Prefs))), % rtrace,trace,
   expand_goal((asserta((call_after_logon((nb_current('$mud_prefs',Prefs),set_player_option(realname, Prefs.realname)))))),OO),
  nl,writeq(:- OO),nl.

% :- asserta((call_after_logon((nb_current('$mud_prefs',Prefs),set_player_option(realname, Prefs.realname))))).

:- listing(foo).
:- listing(test(_)).


:- fixup_exports.

:- 
  set_prolog_flag(toplevel_goal_expansion,true).
% :- set_prolog_flag(toplevel_goal_expansion,false).

end_of_file.



















:- expand_goal(ain(system_autoexec:(==>(mpred_unload_option(never, $current_file.value)))),OO).
:- expand_goal(mpred_unload_option(never, $current_file.value),OO).

:- expand_term(foo :- ain(system_autoexec:(==> (==>mpred_unload_option(never, $current_file.value)))),OO).

% :- debug(dictoo(decl)).

