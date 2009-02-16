%% =====================================================================
%% This library is free software; you can redistribute it and/or modify
%% it under the terms of the GNU Lesser General Public License as
%% published by the Free Software Foundation; either version 2 of the
%% License, or (at your option) any later version.
%%
%% This library is distributed in the hope that it will be useful, but
%% WITHOUT ANY WARRANTY; without even the implied warranty of
%% MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
%% Lesser General Public License for more details.
%%
%% You should have received a copy of the GNU Lesser General Public
%% License along with this library; if not, write to the Free Software
%% Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307
%% USA
%%
%% $Id: esense_edoc_layout.erl,v 1.1.1.1 2005/11/14 11:49:21 locpata Exp $
%%
%% @author Richard Carlsson <richardc@csd.uu.se>
%% @copyright 2001-2002 Richard Carlsson
%% @see edoc
%% @end
%% =====================================================================

%% TODO: generate navigation links, at least back to index.html
%% TODO: mark deprecated functions in function index.

%% @doc The standard HTML layout module for EDoc. See the {@link edoc}
%% module for details on usage.

%% Note that this is written so that it is *not* depending on edoc.hrl!

%% This file is derived from original edoc_layout.erl file and slightly
%% modified to use it in esense

-module(esense_edoc_layout).

-export([layout_module/1]).
-compile([debug_info, export_all]).
-include_lib("xmerl/include/xmerl.hrl").

%% Maximum number of parameters which could be fully described (i.e.
%% with types) in function header
-define(MAX_PARAM_NUMBER, 3).

%% =====================================================================
%% XML-BASED LAYOUT ENGINE
%% =====================================================================

%% We assume that we have expanded XML data.

%% <!ELEMENT module (behaviour*, description?, author*, copyright?,
%%                   version?, since?, deprecated?, see*, reference*,
%%                   typedecls?, functions)>
%% <!ATTLIST module
%%   name CDATA #REQUIRED
%%   private NMTOKEN(yes | no) #IMPLIED
%%   root CDATA #IMPLIED>
%% <!ELEMENT behaviour (#PCDATA)>
%% <!ATTLIST behaviour
%%   href CDATA #IMPLIED>
%% <!ELEMENT description (briefDescription, fullDescription?)>
%% <!ELEMENT briefDescription (#PCDATA)>
%% <!ELEMENT fullDescription (#PCDATA)>
%% <!ELEMENT author EMPTY>
%% <!ATTLIST author
%%   name CDATA #REQUIRED
%%   email CDATA #IMPLIED
%%   website CDATA #IMPLIED>
%% <!ELEMENT version (#PCDATA)>
%% <!ELEMENT since (#PCDATA)>
%% <!ELEMENT copyright (#PCDATA)>
%% <!ELEMENT deprecated (description)>
%% <!ELEMENT see (#PCDATA)>
%% <!ATTLIST see
%%   name CDATA #REQUIRED
%%   href CDATA #IMPLIED>
%% <!ELEMENT reference (#PCDATA)>
%% <!ELEMENT typedecls (typedecl+)>
%% <!ELEMENT functions (function+)>


layout_module(#xmlElement{name = module, content = Es}) ->
    Functions = [E || E <- get_content(functions, Es)],
    SortedFs = lists:sort([{function_name(E), E} || E <- Functions]),
    {ok, functions(SortedFs)}.

%% <!ELEMENT function (args, typespec?, equiv?, description?, since?,
%%                     deprecated?, see*)>
%% <!ATTLIST function
%%   name CDATA #REQUIRED
%%   arity CDATA #REQUIRED
%%   exported NMTOKEN(yes | no) #REQUIRED
%%   label CDATA #IMPLIED>
%% <!ELEMENT args (arg*)>
%% <!ELEMENT arg description?>
%% <!ATTLIST arg name CDATA #REQUIRED>
%% <!ELEMENT equiv (expr, see?)>
%% <!ELEMENT expr (#PCDATA)>

functions(Fs) ->
    lists:map(fun ({Name, E}) -> function(Name, E) end, Fs).

function(Name, E=#xmlElement{content = Es}) ->
    {function_header(Name, E, private),
     case typespec(get_content(typespec, Es)) of
	    [] ->
             signature(get_content(args, Es),
                       get_attrval(name, E));
	    Spec -> 
             Spec
	end,
    fulldesc(Es)}.


function_name(E) ->
    get_attrval(name, E) ++ "/" ++ get_attrval(arity, E).

function_header(Name, E, Private) ->
    case is_exported(E) of
	true  -> Name;
	false -> {Name, Private}
    end.

is_exported(E) ->
    case get_attrval(exported, E) of
 	"yes" -> true;
 	_     -> false
    end.

%% <!ELEMENT args (arg*)>
%% <!ELEMENT arg (argName, description?)>
%% <!ELEMENT argName (#PCDATA)>

%% This is currently only done for functions without type spec.

signature(Es, Name) -> 
    lists:flatten([Name, "("] ++ seq(fun arg/1, Es) ++ [") -> term()"]).

arg(#xmlElement{content = Es}) ->
    [get_text(argName, Es)].

%% <!ELEMENT typespec (erlangName, type, localdef*)>

typespec([]) -> undefined;
typespec(Es) ->
    FuncName = t_name(get_elem(erlangName, Es)),
    Type = get_elem(type, Es),
    {FuncType, DerivedLocalDefs} = t_utype1(Type),
    L = local_defs(get_elem(localdef, Es)),
    {FuncName ++ lists:flatten(FuncType), DerivedLocalDefs ++ L}.

local_defs([]) -> [];
local_defs(Es) ->
    [lists:append(localdef(E)) || E <- Es].

localdef(#xmlElement{content = Es}) ->
    case get_elem(typevar, Es) of
	 [] -> 
	     t_abstype(get_content(abstype, Es));
	 [V] ->
	     t_var(V)
     end
     ++ [" = "] ++ t_utype(get_elem(type, Es)).

fulldesc(Es) ->
    case get_content(fullDescription, get_content(description, Es)) of
	[] -> [];
	Desc -> 
            lists:flatten(xmerl:export_simple(Desc, xmerl_text, []))
    end.


t_name([E]) ->
    N = get_attrval(name, E),
    case get_attrval(module, E) of
	"" -> N;
	M ->
	    S = M ++ ":" ++ N,
	    case get_attrval(app, E) of
		"" -> S;
		A -> "//" ++ A ++ "/" ++ S
	    end
    end.

t_utype1([E]) ->
    t_utype_elem1(E).

t_utype_elem1(E=#xmlElement{content = Es}) ->
    case get_attrval(name, E) of
        "" -> 
            t_type1(Es);
        Name ->
            T = t_type(Es),
            case T of
                [Name] -> {T, []};    % avoid generating "Foo::Foo"
                T -> {[Name], lists:flatten([[Name ++ " = "] ++ T])}
            end
    end.

t_utype([E]) ->
    t_utype_elem(E).

t_utype_elem(E=#xmlElement{content = Es}) ->
    case get_attrval(name, E) of
        "" -> 
            t_type(Es);
        Name ->
            T = t_type(Es),
            case T of
                [Name] -> T;    % avoid generating "Foo::Foo"
                T -> [Name] ++ ["::"] ++ T
            end
    end.

t_type([E=#xmlElement{name = typevar}]) ->
    t_var(E);
t_type([E=#xmlElement{name = atom}]) ->
    t_atom(E);
t_type([E=#xmlElement{name = integer}]) ->
    t_integer(E);
t_type([E=#xmlElement{name = float}]) ->
    t_float(E);
t_type([#xmlElement{name = nil}]) ->
    t_nil();
t_type([#xmlElement{name = list, content = Es}]) ->
    t_list(Es);
t_type([#xmlElement{name = tuple, content = Es}]) ->
    t_tuple(Es);
t_type([#xmlElement{name = 'fun', content = Es}]) ->
    t_fun(Es);
t_type([#xmlElement{name = abstype, content = Es}]) ->
    t_abstype(Es);
t_type([#xmlElement{name = union, content = Es}]) ->
    t_union(Es).

t_type1([#xmlElement{name = 'fun', content = Es}]) ->
    t_fun1(Es).

t_fun1(Es) ->
    ArgTypes = get_content(argtypes, Es),
    L = length(ArgTypes),
    if
        L =< ?MAX_PARAM_NUMBER ->
            {t_fun(Es), []};
        true ->
            RetType = t_utype(get_elem(type, Es)),
            TypeInfo = lists:map(fun t_utype_elem1/1, ArgTypes),
            ArgNames = lists:map(fun({A, _}) ->
                                         A
                                 end, TypeInfo),
            LocalDefs = lists:map(fun({_, A}) ->
                                          A
                                  end, TypeInfo),
            NonEmptyLocalDefs = [LD || LD <- LocalDefs, LD /= ""],
            
            Sig = ["("] ++ seq(fun(X) -> X end, ArgNames) ++  
                [") -> "] ++ 
                RetType,
            {Sig, NonEmptyLocalDefs}
    end.

t_var(E) ->
    [get_attrval(name, E)].

t_atom(E) ->
    [get_attrval(value, E)].

t_integer(E) ->
    [get_attrval(value, E)].

t_float(E) ->
    [get_attrval(value, E)].

t_nil() ->
    ["[]"].

t_list(Es) ->
    ["["] ++ t_utype(get_elem(type, Es)) ++ ["]"].

t_tuple(Es) ->
    ["{"] ++ seq(fun t_utype_elem/1, Es, ["}"]).

t_fun(Es) ->
    ["("] ++ seq(fun t_utype_elem/1, 
                 get_content(argtypes, Es),
                 [") -> "] ++ t_utype(get_elem(type, Es))).

t_abstype(Es) ->
    [t_name(get_elem(erlangName, Es)), "("]
     ++ seq(fun t_utype_elem/1, 
            get_elem(type, Es), 
            [")"]).

t_union(Es) ->
    seq(fun t_utype_elem/1, Es, " | ", []).

%% F = fun(E) -> [Item] 
%% Es = [E]
%% Result = [Item1, ", ", Item2, ", ", ...]
seq(F, Es) ->
    seq(F, Es, []).

%% F = fun(E) -> [Item] 
%% Es = [E]
%% Result = [Item1, ", ", Item2, ", ", ..., Tail]
seq(F, Es, Tail) ->
    seq(F, Es, ", ", Tail).

seq(F, [E], _Sep, Tail) ->
    F(E) ++ Tail;
seq(F, [E | Es], Sep, Tail) ->
    F(E) ++ [Sep] ++ seq(F, Es, Sep, Tail);
seq(_F, [], _Sep, Tail) ->
    Tail.


%% Returns all XML sub-elements named `Name'
get_elem(Name, Es) ->
    lists:filter(fun(X) ->
                         X#xmlElement.name == Name
                 end, Es).

%% Returns all XML attributes named `Name'
get_attr(Name, As) ->
    lists:filter(fun(X) ->
                        X#xmlAttribute.name == Name
                 end, As).

%% Extract value of XML attribute named `Name' from XML element
get_attrval(Name, #xmlElement{attributes = As}) ->
    case get_attr(Name, As) of
        [#xmlAttribute{value = V}] ->
            V;
        [] -> 
            ""
    end.

%% Extract content of XML sub-element named `Name' from list of XML elements
get_content(Name, Es) ->
    case get_elem(Name, Es) of
        [#xmlElement{content = Es1}] ->
            Es1;
        [] -> 
            []
    end.

%% Extract text value from XML sub-element named `Name' from list of XML elements
get_text(Name, Es) ->
    case get_content(Name, Es) of
        [#xmlText{value = Text}] ->
            Text;
        [] -> 
            ""
    end.

