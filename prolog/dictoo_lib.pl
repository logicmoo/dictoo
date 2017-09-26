:- module(dictoo_lib, [
  oo/1,
  oo_bind/2,
  is_oo/1,
  is_oo/2,
  % '$was_dictoo'/2,
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

/** <module> dictoo_lib - Dict-like OO Syntax Pack

    Author:        Douglas R. Miles
    E-mail:        logicmoo@gmail.com
    WWW:           http://www.logicmoo.org
    Copyright (C): 2017
                       
    This program is free software; you can redistribute it and/or
    modify it.

*/



:- set_module(class(library)).
:- reexport(library(gvar_lib)).
:- use_module(library(dicts)).
:- use_module(library(attvar_serializer)).

:- multifile(dot_cache:using_dot_type/2).
:- dynamic(dot_cache:using_dot_type/2).

:- nodebug(dictoo(core)).
:- nodebug(dictoo(decl)).
:- nodebug(dictoo(syntax)).
:- nodebug(dictoo(goal_expand)).

% :- use_module(library(jpl),[jpl_set/3,jpl_get/3,jpl_call/4]).
% :- use_module(atts).
% :- use_module(multivar).

:- meta_predicate(fail_on_missing(*)).
% fail_on_missing(G):-current_predicate(G),!,call(G).
fail_on_missing(G):- notrace(catch(G,error(existence_error(_,_),_),fail)).

% '$was_dictoo'(_,_).

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

:- use_module(library(multivar)).

oo(O):- call(ignore,multivar(O)).
oo_bind(O,Value):- oo(O),put_attr(O,oo,binding(O,Value)).
oo:attr_unify_hook(B,Value):- B = binding(_Var,Prev)->Prev.equals(Value);true.


new_oo(_M,Self,NewSelf):- oo(Self),NewSelf=Self.

logtalk_ready :- current_predicate(logtalk:current_logtalk_flag/2).

is_logtalk_object(O):- logtalk_ready, call(logtalk:current_object(O)).



oo_set(M,UDT,Key, Value):- attvar(UDT),!,M:put_attr(UDT,Key, Value).                           
oo_set(M,UDT,Key, Value):- M:fail_on_missing(jpl_is_ref(UDT)),M:jpl_set(UDT,Key,Value).


put_oo(M,Key, UDT, Value, NewUDT):- is_dict(UDT),!,M:put_dict(Key, UDT, Value, NewUDT).
put_oo(M,Key, UDT, Value, NewUDT):- oo_copy_term(UDT,NewUDT),put_oo(M,Key,NewUDT, Value).

oo_copy_term(UDT,NewUDT):- copy_term(UDT,NewUDT).

put_oo(M,Key, UDT, Value):- is_dict(UDT),!,M:put_dict(Key, UDT, Value).
put_oo(M,Key, UDT, Value):- oo_set(M,UDT,Key, Value).


get_oo(M,Key, UDT, Value):- strip_module(UDT,M,Self), oo_call(M,Self,Key, Value).

oo_jpl_call(A,B,C):- (integer(B);B==length; B= (_-_)),!,jpl_get(A,B,C).
oo_jpl_call(A,B,C):- B=..[H|L], fail_on_missing(jpl_call(A,H,L,C)),!.
oo_jpl_call(A,B,C):- fail_on_missing(jpl_get(A,B,C)).

to_atomic_name(DMemb,Memb):- compound(DMemb),!,compound_name_arity(DMemb,Memb,0).
to_atomic_name(Memb,Memb):- var(Memb),!.
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


dict_op_call(=, M, GVar, Memb, Value):- oo_call(M,GVar,Memb,Value).

:- nb_setval('$oo_stack',[]).
oo_call_dot_hook(M,Self, Func, Value):- is_dict(Self), M:dot_dict(Self, Func, Value),!.
oo_call_dot_hook(M,A,B,C):-  (M:nb_current('$oo_stack',Was)->true;Was=[]),b_setval('$oo_stack',['.'(A,B,C)|Was]),oo_call(M,A,B,C).

oo_call(_M,Self,Memb,Value):- notrace((atom(Memb),attvar(Self))),get_attr(Self, Memb, Value),!.
   

oo_call(M,Self,Memb,Value):- notrace((atom(Memb),var(Self),var(Value))),freeze(Self,freeze(Value,put_oo(M,Memb,Self, Value))).

oo_call(M,'$was_dictoo'(CM,Self),Memb,Value):- var(Self),new_oo(M,Self,NewSelf),!,M:oo_call(CM,NewSelf,Memb,Value).

oo_call(_,'$'(NameSpace), Memb,Value):-  dot_cache:dictoo_decl(= ,_SM,_CM,From,'$'(NameSpace),DMemb,Value,Call),member_func_unify(DMemb,Memb),!,
   show_call(dictoo(core), From:Call).

oo_call(M,'$'(GVar),Memb,Value):- Memb==value,atom(GVar), M:nb_linkval(GVar,Value), freeze(Value,gvar_put(M, GVar, Value)),!.

oo_call(M,'$'(Self),Memb,Value):-  is_gvar(M,Self,_Name),gvar_call(M,Self,Memb,Value),!.
oo_call(M,'$'(GVar),add(Memb,V),'$was_dictoo'(M,$GVar)):- atom(GVar),M:nb_current(GVar,Self),!,
  M:put_dict(Memb,Self,V,NewSelf),M:nb_setval(GVar,NewSelf).

oo_call(M,'$'(GVar),Memb,Value):- gvar_call(M, GVar, Memb, Value),!.
oo_call(M,'$'(GVar),Memb,Value):- atom(GVar),M:nb_current(GVar,Self),oo_call(M,Self,Memb,Value),M:nb_setval(GVar,Self).

oo_call(M,'&'(Self,_),Memb,Value):- gvar_call(M,Self,Memb,Value),!.
oo_call(M,'&'(_,Self),Memb,Value):- oo_call(M,Self,Memb,Value),!.
oo_call(M,Self,set(Memb,Value),'&'(Self)):- is_dict(Self),!,M:nb_set_dict(Memb,Self,Value).

 

oo_call(M,Self,Memb,Value):- notrace(is_dict(Self)),!, M:dot_dict(Self, Memb, Value).
oo_call(M,'&'(Self),Memb,Value):- !,oo_call(M,Self,Memb,Value).
oo_call(M,jpl(Self),Memb,Value):- !, M:oo_jpl_call(Self, Memb, Value).
oo_call(M,jclass(Self),Memb,Value):- !, M:oo_jpl_call(Self, Memb, Value).
oo_call(M,class(Self),Memb,Value):- M:fail_on_missing(cli_call(Self, Memb, Value)),!.
oo_call(M,Self,Memb,Value):- notrace((oo_deref(M,Self,NewSelf)-> NewSelf\==Self)),!,oo_call(M,NewSelf,Memb,Value).

oo_call(M,Self,Memb,Value):- fail_on_missing(jpl_is_ref(Self)),!,M:oo_jpl_call(Self, Memb, Value).

oo_call(M,Self,Memb,Value):-  notrace(fail_on_missing(cli_is_object(Self))),!,fail_on_missing(M:cli_call(Self, Memb, Value)).
oo_call(M,Self,Memb,Value):-  notrace(fail_on_missing(cli_is_struct(Self))),!,fail_on_missing(M:cli_call(Self, Memb, Value)).
oo_call(M,Class,Inner,Value):- show_success(is_oo_class(inner(Class,Inner))),!,oo_call(M,inner(Class,Inner),Value,_).

oo_call(M,'&'(_,DeRef),Memb,Value):- oo_call(M,DeRef,Memb,Value).

%oo_call(M,Self,deref,Value):- var(Self),nonvar(Value),!,oo_call(M,Value,deref,Self).
%oo_call(M,Self,deref,Self):-!.

oo_call(M,Self,Memb,Value):- nonvar(Value),throw(oo_call(M,Self,Memb,Value)).
% oo_call(M,Self,Memb,Value):- gvar_interp(M,Self,Self,Memb,Value).

oo_call(M,'$'(NameSpace), Memb,Value):-  nonvar(NameSpace),dot_cache:dictoo_decl(= ,_SM,_CM,From,'$'(NameSpace),Unk,Value,Call),!,
  throw(M:dot_cache:dictoo_decl(= ,From,NameSpace,Memb-->Unk,Value,Call)),fail.

oo_call(M,Self,Memb,Value):- throw(oo_call(M,Self,Memb,Value)).
oo_call(M,Self,Memb,Value):- var(Value),!,freeze(Value, put_oo(M,Memb,Self, Value)).
oo_call(M,Self,Memb,Value):- var(Value),!,freeze(Value, put_oo(M,Memb,Self, Value)).

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


dictoo_expanded_op( = ).
dictoo_expanded_op( := ).

:- module_transparent(expand_dictoo_head/6).

expand_dictoo_head(Head,_OP,_M,_VarO,_Memb,_Value):- \+ compound(Head),!,fail.
  
% $mod:var.memb = value.                            
expand_dictoo_head(Head,OP,M,VarO,Memb,Value):-
  Head =.. [OP , :(DM,VarMemb),Value],
  compound(DM),
  DM =..[F,M],
  dictoo_expanded_op(OP),
  VarMemb=..['.',Var,Memb],!,
  VarO = [F,Var].

% mod: $var.memb = value.
expand_dictoo_head(Head,OP,M,Var,Memb,Value):-
  Head =.. [OP , :(M,VarMemb),Value],
  compound(VarMemb),
  dictoo_expanded_op(OP),
  VarMemb=..['.',Var,Memb],!.
  
% $var.memb = value.
expand_dictoo_head(Head,OP,M,Var,Memb,Value):-
  Head =.. [OP , MVarMemb,Value],
  compound(MVarMemb),
  dictoo_expanded_op(OP),
  strip_module(MVarMemb,M,VarMemb),
  VarMemb=..['.',Var,Memb],!.

% $var.memb mod:= value.
expand_dictoo_head(M:Head,OP,M,Var,Memb,Value):- 
  Head =.. [OP , VarMemb,Value],
  compound(VarMemb),
  dictoo_expanded_op(OP),
  VarMemb=..['.',Var,Memb],!.



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
:- multifile(dot_cache:dictoo_decl/8).
:- dynamic(dot_cache:dictoo_decl/8).
:- discontiguous(dot_cache:dictoo_decl/8).

% is_oo_hooked(M,Self,_Func,_Value):- M:is_oo(M,Self),!.
is_oo_hooked(M,Self,_Func,_Value):- M:is_oo_invokable(Self,_).

% $current_file.value = X :- prolog_load_context(file,X).
% dot_cache:dictoo_decl(= ,mpred_gvars,current_file,value,A,prolog_load_context(file, A)).
is_oo_hooked(M, IVar, Memb, Value):- compound(IVar), IVar = ($(Var)),
   dot_cache:dictoo_decl(= ,_SM,_CM,M,Var, Memb, Value,_Body).

% is_oo_hooked(M, IVar, value, Ref):- atom(IVar),IVar=Var,dot_cache:dictoo_decl(= ,SM,CM,M,IVar,value,_Value,_Body),!,must(Ref=IVar).

%is_oo_hooked(M,Var,_,Ref):- dot_cache:dictoo_decl(= ,SM,CM,M,Var,value,_Value,_Body),Ref=Var.


oo_class_begin(Name):-asserta(is_oo_class(Name)).
oo_class_end(Name):- is_oo_class(Name),!,retract(is_oo_class(Name)),assertz(is_oo_class(Name)).

oo_inner_class(Name,Inner):-asserta(is_oo_class(inner(Name,Inner))).

oo_inner_class_begin(Inner):- is_oo_class(Name),!,oo_class_begin(inner(Name,Inner)).
oo_inner_class_end(Inner):- is_oo_class(inner(Name,Inner)),!,oo_class_end(inner(Name,Inner)).

oo_class_field(Inner):- is_oo_class(Name),!,asserta(is_oo_class_field(Name,Inner)).



dictoo_expand_query(Goal, Goal, Bindings, Bindings):- nb_setval('$query_term',Goal-Bindings).


:- multifile(gvs:dot_overload_hook/4).
:- dynamic(gvs:dot_overload_hook/4).
:- module_transparent(gvs:dot_overload_hook/4).
gvs:dot_overload_hook(M,NewName, Memb, Value):- dot_cache:using_dot_type(_,M)
  -> show_call(dictoo(overload),oo_call_dot_hook(M,NewName, Memb, Value)).

:- multifile(gvs:is_dot_hook/4).
:- dynamic(gvs:is_dot_hook/4).
:- module_transparent(gvs:is_dot_hook/4).
gvs:is_dot_hook(M,Self,Func,Value):- dot_cache:using_dot_type(_,M) 
  -> is_oo_hooked(M,Self,Func,Value).


show_gvar(Name):-
 (nb_current(Name,Value)->true;Value='$missing'),
 format('~w =~t~12|~p~n', [Name, Value]).

show_dictoo:- 
   listing(dot_cache:dictoo_decl/8),
   listing(dot_cache:using_dot_type/2),   
   maplist(show_gvar,['$goal_term','$term','$variable_names','$query_term']),
   forall(prolog_debug:debugging(dictoo(Name), Value, _),
    format('~w =~t~12|~p~n', [dictoo(Name), Value])),
   ignore(print_toplevel_variables),
   !.


dictoo_ge(Goal, P, _):- nop(var(P)),%  \+ source_location(_,_), 
    % notrace(use_dot(_Type)), 
   debugging(dictoo(syntax)),
   show_call(dictoo(syntax),nb_setval('$goal_term',Goal)),fail.
dictoo_ge(Goal, _P, dict_op_call(OP,M,Var,Memb,Value) ):-
   prolog_load_context(module, M),
   show_success(dictoo(goal_expansion),
   M:(expand_dictoo_head(Goal,OP,M,Var,Memb,Value),use_dot(_Type,M))),!.



:- 
   gvar_file_predicates_are_exported,
   gvar_file_predicates_are_transparent.



:- user:dynamic(expand_query/4).
:- user:multifile(expand_query/4).

user:expand_query(Goal, _, Bindings, _ ):- notrace(use_dot(_Type)), nb_setval('$query_term',Goal-Bindings),fail.

user:expand_query(Goal, Expanded, Bindings, ExpandedBindings):- % notrace(use_dot(_Type)),
    % Have vars to expand and varnames are empty
    notrace((Bindings\==[],prolog_load_context(variable_names,Vs), Vs ==[])),
    b_setval('$variable_names', Bindings),  % this prevents the loop
    % debug(expand_query,'~q',[b_setval('$variable_names', Bindings)]),
    (toplevel_variables_expand_query(Goal, Expanded0, Bindings, ExpandedBindings0) -> true; 
      (Goal = Expanded0, Bindings = ExpandedBindings0)),
    (user:expand_query(Expanded0, Expanded, ExpandedBindings0, ExpandedBindings) -> true ; 
     (Expanded0 = Expanded, ExpandedBindings0 = ExpandedBindings)).

make_top_var(Name,Var,Value):- prolog_load_context(module, M),
   Value = Var,
   use_dot(_Core,M),!,
   % '$was_dictoo'(M,Var),
   oo(Var),
   add_var_to_env(Name,Var),
   toplevel_variables:assert_binding(Name,Value).

expand_vars(_, Var, Var) -->
    { var(Var) },
    !.
expand_vars(_, Atomic, Atomic) -->
    { atomic(Atomic) },
    !.
expand_vars(Bindings, $(Var), Value) -->
    { toplevel_variables:name_var(Var, Bindings, Name),
      (   toplevel_variables:toplevel_var(Name, Value)
      ->  !
      ;   (show_call(make_top_var(Name,Var,Value))-> true ; throw(error(existence_error(answer_variable, Name), _)))
      )
    },
    [ Name = Value ].
expand_vars(Bindings, Term, Expanded) -->
    { compound_name_arity(Term, Name, Arity),
      !,
      compound_name_arity(Expanded, Name, Arity),
      End is Arity + 1
    },
    expand_args(1, End, Bindings, Term, Expanded).


expand_args(End, End, _, _, _) --> !.
expand_args(Arg0, End, Bindings, T0, T) -->
    { arg(Arg0, T0, V0),
      arg(Arg0, T, V1),
      Arg1 is Arg0 + 1
    },
    expand_vars(Bindings, V0, V1),
    expand_args(Arg1, End, Bindings, T0, T).


toplevel_variables_expand_query(Query, Expanded, Bindings, ExpandedBindings) :- !,
    phrase(expand_vars(Bindings, Query, Expanded), NewBindings),
    term_variables(Expanded, Free),
    toplevel_variables:delete_bound_vars(Bindings, Free, ExpandedBindings0),
    '$append'(ExpandedBindings0, NewBindings, ExpandedBindings),
    (   toplevel_variables:verbose,
        Query \=@= Expanded
    ->  toplevel_variables:print_query(Expanded, ExpandedBindings)
    ;   true
    ).

toplevel_variables_expand_query(Goal, Expanded, Bindings, ExpandedBindings):- 
   \+ current_prolog_flag(toplevel_mode, recursive),
   use_dot(core),!,
  catch(toplevel_variables:expand_query(Goal, Expanded, Bindings, ExpandedBindings), Warn,
   ((dmsg(Warn), Goal = Expanded, Bindings = ExpandedBindings))).

toplevel_variables_expand_query(Goal, Expanded, Bindings, ExpandedBindings):-
  toplevel_variables:expand_query(Goal, Expanded, Bindings, ExpandedBindings).


/*
user:expand_query(Goal, Expanded, Bindings, ExpandedBindings):- current_prolog_flag(dictoo_expand_query,toplevel_variables),
  dictoo_expand_query(Goal, Expanded1, Bindings, Bindings1),
  (toplevel_variables:expand_query(Expanded1, Expanded, Bindings1, ExpandedBindings)
    ->true; (Expanded=Expanded1, Bindings1=ExpandedBindings)).

user:expand_query(Goal, Expanded, Bindings, ExpandedBindings):-   use_dot(_Type), current_prolog_flag(dictoo_expand_query,true),!,
  dictoo_expand_query(Goal, Expanded, Bindings, ExpandedBindings).
*/

:- system:dynamic(goal_expansion/4).
:- system:multifile(goal_expansion/4).

system:goal_expansion(Goal, _, _, _ ):- notrace(use_dot(_Type)), nb_setval('$goal_term',Goal),fail.
system:goal_expansion(Goal, P, NewGoal, PO):- dictoo_ge(Goal, P, NewGoal),P=PO.



