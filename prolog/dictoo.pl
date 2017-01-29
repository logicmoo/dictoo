:- module(dictoo, [
  oo/1,
  oo/2,
  is_oo/1,
  oo_call/3,
  jpl_call/3,
  oo_deref/2,
  oo_inner_class_begin/1,
  oo_inner_class_end/1,
  oo_class_field/1,
  oo_class_begin/1,
  oo_class_end/1]).


/** <module> dictoo - Dict-like OO Syntax Pack

    Author:        Douglas R. Miles
    E-mail:        logicmoo@gmail.com
    WWW:           http://www.logicmoo.org
    Copyright (C): 2017
                       
    This program is free software; you can redistribute it and/or
    modify it.

*/

:- system:multifile(system:'.'(_,_)).
:- Head=..['.',Name, Func], system:assert(( Head :- notrace(is_oo(Name)), oo_call(Name,Func,_))).

:- clause('$dicts':'.'(Dict, Func, Value),BODY),
   asserta(('$dictoo_dot3'(Dict, Func, Value):- '$dicts':BODY)).

:- redefine_system_predicate('system':'.'(_Dict, _Func, _Value)).
:- 'system':abolish('$dicts':'.'/3).
'system':'.'(Dict, Func, Value) :- dictoo(Dict,Func,Value).

dictoo(Self,Memb,Value):- notrace(is_oo(Self,Name)) -> oo_call(Name,Memb,Value);
   % Call Previous Method
   '$dict_dot3'(Self, Memb, Value).


% :- use_module(library(jpl),[jpl_set/3,jpl_get/3,jpl_call/4]).
% :- use_module(atts).
% :- use_module(multivar).

:- meta_predicate(fail_on_missing(0)).
% fail_on_missing(G):-current_predicate(G),!,call(G).
fail_on_missing(G):-catch(G,error(existence_error(_,_),_),fail).

is_oo(O):- (attvar(O);is_dict(O);fail_on_missing(jpl_is_ref(O));fail_on_missing(cli_is_object(O));fail_on_missing(cli_is_struct(O))),!.


oo(O):- multivar(O).
oo(O,Value):-multivar(O),put_attr(O,oo,binding(O,Value)).
oo:attr_unify_hook(B,Value):- B = binding(_Var,Prev),Prev.equals(Value).


oo_set(UDT,Key, Value):- attvar(UDT),!,put_attr(UDT,Key, Value).
oo_set(UDT,Key, Value):- jpl_set(UDT,Key,Value).


put_oo(Key, UDT, Value, NewUDT):- is_dict(UDT),!,put_dict(Key, UDT, Value, NewUDT).
put_oo(Key, UDT, Value, NewUDT):- oo_copy_term(UDT,NewUDT),put_oo(NewUDT,Key, Value).

oo_copy_term(UDT,NewUDT):- copy_term(UDT,NewUDT).

put_oo(Key, UDT, Value):- is_dict(UDT),!,put_dict(Key, UDT, Value).
put_oo(Key, UDT, Value):- oo_set(UDT,Key, Value).


get_oo(Key, UDT, Value):- oo_call(UDT,Key, Value).


jpl_call(A,B,C):- B=..[H|L], fail_on_missing(jpl_call(A,H,L,C)),!.
jpl_call(A,B,C):- jpl_get(A,B,C).


oo_call(Self,Memb,Value):- notrace(is_dict(Self)) ,!, '$dictoo_dot3'(Self, Memb, Value).
oo_call(Self,Memb,Value):- notrace((oo_deref(Self,NewSelf)-> NewSelf\=Self)),!,oo_call(NewSelf,Memb,Value).
oo_call(Self,Memb,Value):- notrace((atom(Memb),get_attr(Self, Memb, Value);var(Self))),!,freeze(Value,set_ref(Self, Memb, Value)).
oo_call(Self,Memb,Value):- notrace((jpl_is_ref(Self))),!,jpl_call(Self, Memb, Value).
oo_call(Self,Memb,Value):-  notrace(fail_on_missing(cli_is_object(Self))),!,fail_on_missing(cli_call(Self, Memb, Value)).
oo_call(Self,Memb,Value):-  notrace(fail_on_missing(cli_is_struct(Self))),!,fail_on_missing(cli_call(Self, Memb, Value)).
oo_call(Class,Inner,Value):- notrace(is_oo_class(inner(Class,Inner))),!,oo_call(inner(Class,Inner),deref,Value).


%oo_call(Self,deref,Value):- var(Self),nonvar(Value),!,oo_call(Value,deref,Self).
%oo_call(Self,deref,Self):-!.
oo_call(Self,Memb,Value):- var(Value),!,freeze(Value,set_ref(Self, Memb, Value)).
oo_call(Self,Memb,Var):-var(Var),!,Var=oo_ref(Self,Memb).
oo_call(Self,Memb,Value):- throw(oo_call(Self,Memb,Value)).

/*
oo_call(Self,Memb,Value):- nb_linkval(Self,construct(Self,Memb,Value)),!,oo_call(Self,Memb,Value).
oo_call(Self,Memb,Value):- to_member_path(Memb,[F|Path]),append(Path,[Value],PathWValue),
   Call =.. [F,Self|PathWValue],
   oo_call(Call).

to_member_path(C,[F|ARGS]):-compound(C),!,compound_name_args(C,F,ARGS).
to_member_path(C,[C]).

*/

oo_deref(Obj,RObj):- var(Obj),!,once(get_attr(Obj,oo,binding(_,RObj));Obj=RObj),!.
oo_deref(GVar,Value):- atom(GVar),nb_current(GVar,ValueM),!,oo_deref(ValueM,Value).
oo_deref(Self,NewSelf):- get_attr(Self, oo, NewSelf),!.
oo_deref(Value,Value):- \+ compound(Value),!.
oo_deref(cl_eval(Call),Result):-is_list(Call),!,cl_eval(Call,Result).
oo_deref(cl_eval(Call),Result):-!,nonvar(Call),oo_deref(Call,CallE),!,call(CallE,Result).
oo_deref(Value,Value):- jpl_is_ref(Value),!.
%%oo_deref([A|B],Result):-!, maplist(oo_deref,[A|B],Result).
%%oo_deref(Call,Result):- call(Call,Result),!.
oo_deref(Head,HeadE):- Head=..B,maplist(oo_deref,B,A),HeadE=..A,!.
oo_deref(Value,Value).




get_oo(Key, Dict, Value, NewDict, NewDict) :- is_dict(Dict),!,
   get_dict(Key, Dict, Value, NewDict, NewDict).
get_oo(Key, Dict, Value, NewDict, NewDict) :-
        get_oo(Key, Dict, Value),
        put_oo(Key, Dict, NewDict, NewDict).






%!  eval_oo_function(+Func, +Tag, +UDT, -Value)
%
%   Test for predefined functions on Objects or evaluate a user-defined
%   function.

eval_oo_function(Func, Tag, UDT, Value) :- is_dict(Tag),!,
   '$dicts':eval_dict_function(Func, Tag, UDT, Value).

eval_oo_function(get(Key), _, UDT, Value) :-
    !,
    get_oo(Key, UDT, Value).
eval_oo_function(put(Key, Value), _, UDT, NewUDT) :-
    !,
    (   atomic(Key)
    ->  put_oo(Key, UDT, Value, NewUDT)
    ;   put_oo_path(Key, UDT, Value, NewUDT)
    ).
eval_oo_function(put(New), _, UDT, NewUDT) :-
    !,
    put_oo(New, UDT, NewUDT).
eval_oo_function(Func, Tag, UDT, Value) :-
    call(Tag:Func, UDT, Value).


%!  put_oo_path(+KeyPath, +UDT, +Value, -NewUDT)
%
%   Add/replace  a  value  according  to  a  path  definition.  Path
%   segments are separated using '/'.

put_oo_path(Key, UDT, Value, NewUDT) :-
    atom(Key),
    !,
    put_oo(Key, UDT, Value, NewUDT).
put_oo_path(Path, UDT, Value, NewUDT) :-
    get_oo_path(Path, UDT, _Old, NewUDT, Value).

get_oo_path(Path, _, _, _, _) :-
    var(Path),
    !,
    '$instantiation_error'(Path).
get_oo_path(Path/Key, UDT, Old, NewUDT, New) :-
    !,
    get_oo_path(Path, UDT, OldD, NewUDT, NewD),
    (   get_oo(Key, OldD, Old, NewD, New),
        is_oo(Old)
    ->  true
    ;   Old = _{},
        put_oo(Key, OldD, New, NewD)
    ).
get_oo_path(Key, UDT, Old, NewUDT, New) :-
    get_oo(Key, UDT, Old, NewUDT, New),
    is_oo(Old),
    !.
get_oo_path(Key, UDT, _{}, NewUDT, New) :-
    put_oo(Key, UDT, New, NewUDT).



:- dynamic(is_oo_class/1).
:- dynamic(is_oo_class_field/2).

oo_class_begin(Name):-asserta(is_oo_class(Name)).
oo_class_end(Name):- is_oo_class(Name),!,retract(is_oo_class(Name)),assertz(is_oo_class(Name)).

oo_inner_class(Name,Inner):-asserta(is_oo_class(inner(Name,Inner))).

oo_inner_class_begin(Inner):- is_oo_class(Name),!,oo_class_begin(inner(Name,Inner)).
oo_inner_class_end(Inner):- is_oo_class(inner(Name,Inner)),!,oo_class_end(inner(Name,Inner)).

oo_class_field(Inner):- is_oo_class(Name),!,asserta(is_oo_class_field(Name,Inner)).



