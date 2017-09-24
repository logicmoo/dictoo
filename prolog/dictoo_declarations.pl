:- module(dictoo_declarations, []).


/** <module> dictoo_declarations - OO Overloads term_expansion in files

    Author:        Douglas R. Miles
    E-mail:        logicmoo@gmail.com
    WWW:           http://www.logicmoo.org
    Copyright (C): 2017
                       
    This program is free software; you can redistribute it and/or
    modify it.

*/

:- use_module(library(gvar_syntax)).
:- use_module(library(dicts)).
:- use_module(library(dictoo)).

:- set_module(class(library)).

% foo:($current_file.value) = X :- prolog_load_context(file,X).
dictoo_expand_decl(EMDVARMEMB,Body,dictoo:dictoo_decl(M,Var,Memb,Value,Body)):-
  strip_module(EMDVARMEMB,M,(DVARMEMB=Value)),DVARMEMB=..['.',$Var,Memb],!.

% $current_file.value = X :- prolog_load_context(file,X).
dictoo_expand_decl(MDVARMEMB=Value,Body,dictoo:dictoo_decl(M,Var,Memb,Value,Body)):-
  strip_module(MDVARMEMB,M,DVARMEMB), compound(DVARMEMB),DVARMEMB=..['.',$Var,Memb],!.

  
 

term_expansion((EMDVARMEMB:-Body),OUT):- dictoo_expand_decl(EMDVARMEMB,Body,OUT).
term_expansion((EMDVARMEMB),OUT):- dictoo_expand_decl(EMDVARMEMB,true,OUT).

                  
:- 
   gvar_file_predicates_are_exported,
   gvar_file_predicates_are_transparent.

