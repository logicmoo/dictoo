:- module(dictoo, [
  oo/1,
  oo_bind/2,
  is_oo/1,
  is_oo/2,
  '$was_dictoo'/2,
  dictoo_decl/5,
  is_oo_invokable/2,
  oo_call/4,
  oo_call_dot_hook/4,
  oo_jpl_call/3,
  oo_deref/3,
  oo_put_attr/3,
  oo_put_attrs/2,               
  oo_get_attr/3,
  oo_get_attrs/2,
  oo_inner_class_begin/1,
  oo_inner_class_end/1,
  oo_class_field/1,
  oo_class_begin/1,
  oo_class_end/1,  
  oo_get_attr/3,
          oo_put_attr/3,
          oo_get_attrs/2,
          oo_put_attrs/2
  ]).

/** <module> dictoo - Dict-like OO Syntax Pack

    Author:        Douglas R. Miles
    E-mail:        logicmoo@gmail.com
    WWW:           http://www.logicmoo.org
    Copyright (C): 2017
                       
    This program is free software; you can redistribute it and/or
    modify it.

*/

:- use_module(library(gvar_syntax)).
:- use_module(library(dicts)).

:- set_module(class(library)).




% :- use_module(library(jpl),[jpl_set/3,jpl_get/3,jpl_call/4]).
% :- use_module(atts).
% :- use_module(multivar).

:- meta_predicate(fail_on_missing(*)).
% fail_on_missing(G):-current_predicate(G),!,call(G).
fail_on_missing(G):- quietly(catch(G,error(existence_error(_,_),_),fail)).

'$was_dictoo'(_,_).

%% is_oo(+Self) is det.
%% is_oo(+Module,+Self) is det.
%
%  Tests to see if Self
%   Has  Member Functions
%    
is_oo(S):- strip_module(S,M,O),is_oo(M,O).
is_oo(_,_):- !. % assume we''ll take care of dicts as well
is_oo(M,O):-  
 quietly((((var(O),!,attvar(O));
  ((((O='$was_dictoo'(MM,_),ignore(M=MM));
     O=jclass(_);
     O=jpl(_);
     O= class(_);
     O='&'(_) ;
     O='&'(_,_) ;
     functor(O,'.',2) ;
     O='$'(_) ;
     is_dict(O) ;
     is_logtalk_object(O),
  fail_on_missing(jpl_is_ref(O));
  fail_on_missing(cli_is_object(O));
  fail_on_missing(cli_is_struct(O)))))))),!.


oo(O):- call(ignore,multivar(O)).
oo_bind(O,Value):- oo(O),put_attr(O,oo,binding(O,Value)).
oo:attr_unify_hook(B,Value):- B = binding(_Var,Prev)->Prev.equals(Value);true.

logtalk_ready :- current_predicate(logtalk:current_logtalk_flag/2).

is_logtalk_object(O):- logtalk_ready, call(logtalk:current_object(O)).



oo_set(M,UDT,Key, Value):- attvar(UDT),!,M:put_attr(UDT,Key, Value).
oo_set(M,UDT,Key, Value):- M:fail_on_missing(jpl_is_ref(UDT)),M:jpl_set(UDT,Key,Value).


put_oo(M,Key, UDT, Value, NewUDT):- is_dict(UDT),!,M:put_dict(Key, UDT, Value, NewUDT).
put_oo(M,Key, UDT, Value, NewUDT):- oo_copy_term(UDT,NewUDT),put_oo(M,NewUDT,Key, Value).

oo_copy_term(UDT,NewUDT):- copy_term(UDT,NewUDT).

put_oo(M,Key, UDT, Value):- is_dict(UDT),!,M:put_dict(Key, UDT, Value).
put_oo(M,Key, UDT, Value):- oo_set(M,UDT,Key, Value).


get_oo(M,Key, UDT, Value):- strip_module(UDT,M,Self), oo_call(M,Self,Key, Value).

oo_jpl_call(A,B,C):- (integer(B);B==length; B= (_-_)),!,jpl_get(A,B,C).
oo_jpl_call(A,B,C):- B=..[H|L], fail_on_missing(jpl_call(A,H,L,C)),!.
oo_jpl_call(A,B,C):- fail_on_missing(jpl_get(A,B,C)).

to_atomic_name(DMemb,Memb):- compound(DMemb),!,compound_name_arity(DMemb,Memb,0).
to_atomic_name(Memb,Memb):- var(Memb),!,fail.
to_atomic_name(SMemb,Memb):- string_to_atom(SMemb,Memb).

member_func_unify(X,Y):- to_atomic_name(X,X1),to_atomic_name(Y,Y1),X1=Y1.

%% is_oo_invokable(+Self,-DeRef) is det.
%
%  DeRef''s an OO Whatnot and ckecks if invokable
%    

is_oo_invokable(Was,Was):- is_oo(Was),!.
% is_oo_invokable(Was,Was):- M:nb_current('$oo_stack',[Was|_])
is_oo_invokable(Was,Ref):- strip_module(Was,M,Self),oo_deref(M,Self,Ref),!,((Self\==Ref,M:Self\==Ref,Self\==M:Ref);is_oo(M,Ref)).

:- module_transparent(oo_call/4).
:- module_transparent(oo_call_dot_hook/4).

:- nb_setval('$oo_stack',[]).
oo_call_dot_hook(M,Self, Func, Value):- is_dict(Self),M:dot_dict(Self, Func, Value),!.
oo_call_dot_hook(M,A,B,C):-  (M:nb_current('$oo_stack',Was)->true;Was=[]),b_setval('$oo_stack',['.'(A,B,C)|Was]),oo_call(M,A,B,C).

oo_call(M,Self,Memb,Value):- quietly((atom(Memb),var(Self),(get_attr(Self, Memb, Value);var(Self)))),!,freeze(Value,put_oo(M,M:Self, Memb, Value)).

oo_call(_,'$'(NameSpace), Memb,Value):-  dictoo:dictoo_decl(From,NameSpace,DMemb,Value,Call),member_func_unify(DMemb,Memb),!,show_call( From:Call).
oo_call(M,'$'(Self),Memb,Value):-  is_gvar(M,Self,_Name),gvar_call(M,Self,Memb,Value),!.
oo_call(M,'$'(GVar),add(Memb,V),'$was_dictoo'(M,$GVar)):- atom(GVar),M:nb_current(GVar,Self),!,M:put_dict(Memb,Self,V,NewSelf),M:nb_setval(GVar,NewSelf).
oo_call(M,'$'(GVar),Memb,Value):- atom(GVar),M:nb_current(GVar,Self),!,oo_call(M,Self,Memb,Value),M:nb_setval(GVar,Self).
oo_call(M,'&'(Self,_),Memb,Value):- gvar_call(M,Self,Memb,Value),!.
oo_call(M,'&'(_,Self),Memb,Value):- oo_call(M,Self,Memb,Value),!.
oo_call(M,Self,set(Memb,Value),'&'(Self)):- is_dict(Self),!,M:nb_set_dict(Memb,Self,Value).

 

oo_call(M,Self,Memb,Value):- is_dict(Self),!, M:dot_dict(Self, Memb, Value).
oo_call(M,'&'(Self),Memb,Value):- !,oo_call(M,Self,Memb,Value).
oo_call(M,jpl(Self),Memb,Value):- !, M:oo_jpl_call(Self, Memb, Value).
oo_call(M,jclass(Self),Memb,Value):- !, M:oo_jpl_call(Self, Memb, Value).
oo_call(M,class(Self),Memb,Value):- M:fail_on_missing(cli_call(Self, Memb, Value)),!.
oo_call(M,Self,Memb,Value):- quietly((oo_deref(M,Self,NewSelf)-> NewSelf\=Self)),!,oo_call(M,NewSelf,Memb,Value).

oo_call(M,Self,Memb,Value):- fail_on_missing(jpl_is_ref(Self)),!,M:oo_jpl_call(Self, Memb, Value).

oo_call(M,Self,Memb,Value):-  fail_on_missing(cli_is_object(Self)),!,fail_on_missing(M:cli_call(Self, Memb, Value)).
oo_call(M,Self,Memb,Value):-  fail_on_missing(cli_is_struct(Self)),!,fail_on_missing(M:cli_call(Self, Memb, Value)).
oo_call(M,Class,Inner,Value):- quietly(is_oo_class(inner(Class,Inner))),!,oo_call(M,inner(Class,Inner),Value,_).

oo_call(M,'&'(_,DeRef),Memb,Value):- oo_call(M,DeRef,Memb,Value).

%oo_call(M,Self,deref,Value):- var(Self),nonvar(Value),!,oo_call(M,Value,deref,Self).
%oo_call(M,Self,deref,Self):-!.

oo_call(M,Self,Memb,Value):- nonvar(Value),throw(oo_call(M,Self,Memb,Value)).
% oo_call(M,Self,Memb,Value):- gvar_interp(M,Self,Self,Memb,Value).
oo_call(M,'$'(NameSpace), Memb,Value):-  nonvar(NameSpace),dictoo:dictoo_decl(From,NameSpace,Unk,Value,Call),!,
   throw(M:dictoo_decl(From,NameSpace,Memb-->Unk,Value,Call)),fail.
oo_call(M,Self,Memb,Value):- var(Value),!,freeze(Value,put_oo(M,Self, Memb, Value)).
oo_call(M,Self,Memb,Value):- var(Value),!,freeze(Value,put_oo(M,Self, Memb, Value)).
oo_call(M,Self,Memb,Value):- var(Value),!,Value='&'(M:Self,Memb).
oo_call(M,Self,Memb,Value):- throw(oo_call(M,Self,Memb,Value)).

/*
oo_call(M,Self,Memb,Value):- nb_linkval(Self,construct(Self,Memb,Value)),!,oo_call(M,Self,Memb,Value).
oo_call(M,Self,Memb,Value):- to_member_path(Memb,[F|Path]),append(Path,[Value],PathWValue),
   Call =.. [F,Self|PathWValue],
   oo_call(M,Call).

to_member_path(C,[F|ARGS]):-compound(C),!,compound_name_args(C,F,ARGS).
to_member_path(C,[C]).

*/

oo_deref(M,Obj,RObj):- var(Obj),!,M:once(get_attr(Obj,oo,binding(_,RObj));Obj=RObj),!.
% oo_deref(M,'$'(GVar),'&'(GVar,Value)):- atom(GVar),M:nb_current(GVar,Value),!.
oo_deref(M,'&'(GVar),Value):- atom(GVar),M:nb_current(GVar,ValueM),!,oo_deref(M,ValueM,Value).
oo_deref(_,Value,Value):- \+ compound(Value),!.
oo_deref(M,cl_eval(Call),Result):-is_list(Call),!,M:fail_on_missing(cl_eval(Call,Result)).
oo_deref(M,cl_eval(Call),Result):-!,nonvar(Call),oo_deref(M,Call,CallE),!,M:call(CallE,Result).
oo_deref(M,Value,Value):- M:fail_on_missing(jpl_is_ref(Value)),!.
% %oo_deref(M,[A|B],Result):-!, maplist(oo_deref(M),[A|B],Result).
% %oo_deref(M,Call,Result):- call(Call,Result),!.
oo_deref(_,Value,Value):- is_logtalk_object(Value).
%oo_deref(M,Head,HeadE):- Head=..B,maplist(oo_deref(M),B,A),HeadE=..A,!.
oo_deref(_,Value,Value).


get_oo(M,Key, Dict, Value, NewDict, NewDict) :- is_dict(Dict),!,
   M:get_dict(Key, Dict, Value, NewDict, NewDict).
get_oo(M,Key, Dict, Value, NewDict, NewDict) :-
        get_oo(M,Key, Dict, Value),
        put_oo(M,Key, Dict, NewDict, NewDict).



%!  eval_oo_function(M,+Func, +Tag, +UDT, -Value)
%
%   Test for predefined functions on Objects or evaluate a user-defined
%   function.

eval_oo_function(_M,Func, Tag, UDT, Value) :- is_dict(UDT),!,
   '$dicts':eval_dict_function(Func, Tag, UDT, Value).

eval_oo_function(M,get(Key), _, UDT, Value) :-
    !,
    get_oo(M,Key, UDT, Value).
eval_oo_function(M,put(Key, Value), _, UDT, NewUDT) :-
    !,
    (   atomic(Key)
    ->  put_oo(M,Key, UDT, Value, NewUDT)
    ;   put_oo_path(M,Key, UDT, Value, NewUDT)
    ).
eval_oo_function(M,put(New), _, UDT, NewUDT) :-
    !,
    put_oo(M,New, UDT, NewUDT).
eval_oo_function(M,Func, Tag, UDT, Value) :-
    M:call(Tag:Func, UDT, Value).


%!  put_oo_path(M,+KeyPath, +UDT, +Value, -NewUDT)
%
%   Add/replace  a  value  according  to  a  path  definition.  Path
%   segments are separated using '/'.

put_oo_path(M,Key, UDT, Value, NewUDT) :-
    atom(Key),
    !,
    put_oo(M,Key, UDT, Value, NewUDT).
put_oo_path(M,Path, UDT, Value, NewUDT) :-
    get_oo_path(M,Path, UDT, _Old, NewUDT, Value).

get_oo_path(_M,Path, _, _, _, _) :-
    var(Path),
    !,
    '$instantiation_error'(Path).
get_oo_path(M,Path/Key, UDT, Old, NewUDT, New) :-
    !,
    get_oo_path(M,Path, UDT, OldD, NewUDT, NewD),
    (   get_oo(M,Key, OldD, Old, NewD, New),
        is_oo(M,Old)
    ->  true
    ;   Old = _{},
        put_oo(M,Key, OldD, New, NewD)
    ).
get_oo_path(M,Key, UDT, Old, NewUDT, New) :-
    get_oo(M,Key, UDT, Old, NewUDT, New),
    is_oo(M,Old),
    !.
get_oo_path(M,Key, UDT, _{}, NewUDT, New) :-
    put_oo(M,Key, UDT, New, NewUDT).



:- dynamic(is_oo_class/1).
:- dynamic(is_oo_class_field/2).
:- multifile(dictoo_decl/5).
:- dynamic(dictoo_decl/5).

% is_oo_hooked(M,Self,_Func,_Value):- M:is_oo(M,Self),!.
is_oo_hooked(M,Self,_Func,_Value):- M:is_oo_invokable(Self,_).

% $current_file.value = X :- prolog_load_context(file,X).
% dictoo:dictoo_decl(mpred_gvars,current_file,value,A,prolog_load_context(file, A)).
is_oo_hooked(M, IVar, value, Value):- compound(IVar), IVar = ($(Var)),
   dictoo_decl(M,Var,value, Value,_Body).

% is_oo_hooked(M, IVar, value, Ref):- atom(IVar),IVar=Var,dictoo_decl(M,IVar,value,_Value,_Body),!,must(Ref=IVar).

%is_oo_hooked(M,Var,_,Ref):- dictoo_decl(M,Var,value,_Value,_Body),Ref=Var.


oo_class_begin(Name):-asserta(is_oo_class(Name)).
oo_class_end(Name):- is_oo_class(Name),!,retract(is_oo_class(Name)),assertz(is_oo_class(Name)).

oo_inner_class(Name,Inner):-asserta(is_oo_class(inner(Name,Inner))).

oo_inner_class_begin(Inner):- is_oo_class(Name),!,oo_class_begin(inner(Name,Inner)).
oo_inner_class_end(Inner):- is_oo_class(inner(Name,Inner)),!,oo_class_end(inner(Name,Inner)).

oo_class_field(Inner):- is_oo_class(Name),!,asserta(is_oo_class_field(Name,Inner)).



oo_get_attr(V,A,Value):- var(V),!,get_attr(V,A,Value),!.
oo_get_attr('$VAR'(Name),N,V):- atom(Name),!,N=vn,V=Name.
oo_get_attr('$VAR'(Att3),A,Value):- !, oo_put_attrs(NewVar,Att3),get_attr(NewVar,A,Value).
oo_get_attr('avar'(Att3),A,Value):- !, oo_put_attrs(NewVar,Att3),get_attr(NewVar,A,Value).
oo_get_attr('avar'(_,Att3),A,Value):- !, oo_put_attrs(NewVar,Att3),get_attr(NewVar,A,Value).
oo_get_attr(Self,Memb,Value):- strip_module(Memb,M,Prop),oo_call(M,Self,Prop,Value).
% oo_get_attr(V,A,Value):- trace_or_throw(oo_get_attr(V,A,Value)).


put_atts_list([N=V|Attrs],Var):- oo_put_attr(Var,N,V),!,put_atts_list(Attrs,Var).
put_atts_list([NV|Attrs],Var):- !,NV=..[N,V],oo_put_attr(Var,N,V),!,put_atts_list(Attrs,Var).
put_atts_list([],_):-!.

%:- if(exists_source(library(atts))).
%:- user:use_module(library(atts)).
oo_put_attrs(V,Attrs):- must_be(nonvar,Attrs),is_list(Attrs),!,put_atts_list(Attrs,V).
%:- endif.
oo_put_attrs(V,Att3s):- var(V),!,put_attrs(V,Att3s),!.
oo_put_attrs(VAR,Att3s):- VAR='$VAR'(Name), atom(Name),!,setarg(1,VAR, att(vn, Name, Att3s)).
oo_put_attrs(VAR,Att3s):- VAR='$VAR'(_Att3),!,setarg(1,VAR, Att3s).
oo_put_attrs(VAR,Att3s):- VAR='avar'(_Att3),!,setarg(1,VAR, Att3s).
oo_put_attrs(VAR,Att3s):- VAR='avar'(_,_),!,setarg(2,VAR, Att3s).
oo_put_attrs(V,Att3s):- trace_or_throw(oo_put_attrs(V,Att3s)).

oo_get_attrs(V,Att3s):- var(V),!,get_attrs(V,Att3s),!.
oo_get_attrs('$VAR'(Name),_):- atom(Name),!,fail.
oo_get_attrs('$VAR'(Att3s),Att3):-!,Att3s=Att3.
oo_get_attrs('avar'(Att3s),Att3):-!,Att3s=Att3.
oo_get_attrs('avar'(_,Att3s),Att3):-!,Att3s=Att3.
% oo_get_attrs(V,Value):- trace_or_throw(oo_get_attrs(V,Value)).

oo_put_attr(V,A,Value):- var(V),!,put_attr(V,A,Value),!.
oo_put_attr(VAR,A,Value):- VAR='$VAR'(Name), atom(Name),!,setarg(1,VAR, att(vn, Name, att(A,Value, []))).
oo_put_attr(VAR,A,Value):- VAR='$VAR'(Att3),!,setarg(1,VAR, att(A,Value,Att3)).
oo_put_attr(VAR,A,Value):- VAR='avar'(Att3),!,setarg(1,VAR, att(A,Value,Att3)).
oo_put_attr(VAR,A,Value):- VAR='avar'(_,Att3),!,setarg(2,VAR, att(A,Value,Att3)).
oo_put_attr(V,A,Value):- trace_or_throw(oo_put_attr(V,A,Value)).





:- multifile(gvar_syntax:dot_syntax_hook/4).
:- dynamic(gvar_syntax:dot_syntax_hook/4).
:- module_transparent(gvar_syntax:dot_syntax_hook/4).
gvar_syntax:dot_syntax_hook(M,NewName, Memb, Value):-show_call(oo_call_dot_hook(M,NewName, Memb, Value)).

:- multifile(gvar_syntax:is_dot_hook/4).
:- dynamic(gvar_syntax:is_dot_hook/4).
:- module_transparent(gvar_syntax:is_dot_hook/4).
gvar_syntax:is_dot_hook(M,Self,Func,Value):- is_oo_hooked(M,Self,Func,Value).




:- 
   gvar_file_predicates_are_exported,
   gvar_file_predicates_are_transparent.

