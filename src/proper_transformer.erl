%%% Copyright 2010 Manolis Papadakis (manopapad@gmail.com)
%%%            and Kostis Sagonas (kostis@cs.ntua.gr)
%%%
%%% This file is part of PropEr.
%%%
%%% PropEr is free software: you can redistribute it and/or modify
%%% it under the terms of the GNU General Public License as published by
%%% the Free Software Foundation, either version 3 of the License, or
%%% (at your option) any later version.
%%%
%%% PropEr is distributed in the hope that it will be useful,
%%% but WITHOUT ANY WARRANTY; without even the implied warranty of
%%% MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
%%% GNU General Public License for more details.
%%%
%%% You should have received a copy of the GNU General Public License
%%% along with PropEr.  If not, see <http://www.gnu.org/licenses/>.

%%% @author Manolis Papadakis <manopapad@gmail.com>
%%% @copyright 2010 Manolis Papadakis and Kostis Sagonas
%%% @version {@version}
%%% @doc This module contains PropEr's main parse transformer. It is
%%%	 automatically applied to modules when including the main PropEr header,
%%%	 unless PROPER_NOAUTO is defined. Applying this transform has the
%%%	 following effects:
%%%	 <ul>
%%%	 <li>All prop_* functions of arity 0 in the module are automatically
%%%	     exported.</li>
%%%	 <li>Type declarations in ?FORALLs that correspond to built-in types
%%%	     are properly substituted (with some limitations, see the README
%%%	     for details).</li>
%%%	 </ul>

-module(proper_transformer).
-export([parse_transform/2]).

-export_type([]).

-include("proper_internal.hrl").


%%------------------------------------------------------------------------------
%% Macros
%%------------------------------------------------------------------------------

-define(PROPERTY_PREFIX, "prop_").
-define(SRC_FILE_EXT, ".erl").


%%------------------------------------------------------------------------------
%% Types
%%------------------------------------------------------------------------------

-type fun_set() :: set(). %% set({fun_name(),arity()})
-record(mod_info, {name                    :: mod_name(),
		   helper_pid              :: pid(),
		   props      = sets:new() :: fun_set(),
		   in_scope   = sets:new() :: fun_set(),
		   exported   = sets:new() :: fun_set()}).
-type mod_info() :: #mod_info{}.
-type exp_dict() :: dict(). %% dict(mod_name(),{'data',fun_set()} | 'no_data')


%%------------------------------------------------------------------------------
%% Top-level functions
%%------------------------------------------------------------------------------

-spec parse_transform([abs_form()], _) -> [abs_form()].
parse_transform(Forms, _Options) ->
    RawModInfo = collect_info(Forms),
    #mod_info{name = ModName, props = AllProps, exported = Exported}
	= RawModInfo,
    HelperPid = helper_start(ModName, Exported),
    ModInfo = RawModInfo#mod_info{helper_pid = HelperPid},
    PropsToExport = sets:to_list(sets:subtract(AllProps, Exported)),
    NewForms = [rewrite_form(F,ModInfo) || F <- Forms],
    helper_stop(HelperPid),
    add_exports(NewForms, PropsToExport).

-spec collect_info([abs_form()]) -> mod_info().
collect_info(Forms) ->
    lists:foldl(fun add_info/2, #mod_info{}, Forms).

-spec add_info(abs_form(), mod_info()) -> mod_info().
add_info({attribute,_Line,module,ModName}, ModInfo) ->
    ModInfo#mod_info{name = ModName};
add_info({attribute,_Line,export,MoreExported},
	 #mod_info{exported = Exported} = ModInfo) ->
    NewExported = sets:union(sets:from_list(MoreExported), Exported),
    ModInfo#mod_info{exported = NewExported};
add_info({attribute,_Line,import,{_FromMod,MoreImported}},
	 #mod_info{in_scope = InScope} = ModInfo) ->
    NewInScope = sets:union(sets:from_list(MoreImported), InScope),
    ModInfo#mod_info{in_scope = NewInScope};
add_info({function,_Line,Name,Arity,_Clauses},
	 #mod_info{props = Props, in_scope = InScope} = ModInfo) ->
    NewInScope = sets:add_element({Name,Arity}, InScope),
    NewProps = case Arity =:= 0 andalso
		    lists:prefix(?PROPERTY_PREFIX, atom_to_list(Name)) of
		   true  -> sets:add_element({Name,Arity}, Props);
		   false -> Props
	       end,
    ModInfo#mod_info{props = NewProps, in_scope = NewInScope};
add_info(_Form, ModInfo) ->
    ModInfo.

-spec add_exports([abs_form()], [{fun_name(),arity()}]) -> [abs_form()].
add_exports(Forms, ToExport) ->
    add_exports_tr(Forms, [], ToExport).

-spec add_exports_tr([abs_form()], [abs_form()], [{fun_name(),arity()}]) ->
	  [abs_form()].
add_exports_tr([], Acc, _ToExport) ->
    lists:reverse(Acc);
add_exports_tr([{attribute,_,module,_} = ModAttr | Rest], Acc, ToExport) ->
    ExpAttr = {attribute,0,export,ToExport},
    lists:reverse(Acc) ++ [ModAttr, ExpAttr | Rest];
add_exports_tr([Form | Rest], Acc, ToExport) ->
    add_exports_tr(Rest, [Form | Acc], ToExport).


%%------------------------------------------------------------------------------
%% Helper server interface
%%------------------------------------------------------------------------------

-spec helper_start(mod_name(), fun_set()) -> pid().
helper_start(Mod, ModExported) ->
    spawn(fun() -> helper_init(Mod,ModExported) end).

-spec helper_stop(pid()) -> 'ok'.
helper_stop(HelperPid) ->
    HelperPid ! stop,
    ok.

-spec is_exported_fun(mod_name(), atom(), arity(), pid()) -> boolean().
is_exported_fun(Mod, Call, Arity, HelperPid) ->
    HelperPid ! {is_exported_fun,self(),Mod,Call,Arity},
    receive
	Answer -> Answer
    end.

-spec helper_init(mod_name(), fun_set()) -> 'ok'.
helper_init(Mod, ModExported) ->
    ExpDict = dict:from_list([{Mod,{data,ModExported}}]),
    helper_loop(ExpDict).

-spec helper_loop(exp_dict()) -> 'ok'.
helper_loop(ExpDict) ->
    receive
	stop ->
	    ok;
	{is_exported_fun,From,Mod,Call,Arity} ->
	    NewExpDict = add_module(Mod, ExpDict),
	    Answer = case dict:fetch(Mod, NewExpDict) of
			 {data,ModExported} ->
			     sets:is_element({Call,Arity}, ModExported);
			 nodata ->
			     true
		     end,
	    From ! Answer,
	    helper_loop(NewExpDict)
    end.

-spec add_module(mod_name(), exp_dict()) -> exp_dict().
add_module(Mod, ExpDict) ->
    case dict:is_key(Mod, ExpDict) of
	true ->
	    ExpDict;
	false ->
	    case code:which(Mod) of
		ObjFilename when is_list(ObjFilename) ->
		    read_exports(Mod, ObjFilename, ExpDict);
		_Error when is_atom(_Error) ->
		    SrcFilename = atom_to_list(Mod) ++ ?SRC_FILE_EXT,
		    case code:where_is_file(SrcFilename) of
			FullSrcFilename when is_list(FullSrcFilename) ->
			    case compile:file(FullSrcFilename,
					      [binary,{d,'PROPER_NOAUTO'}]) of
				{ok,Mod,Binary} ->
				    read_exports(Mod, Binary, ExpDict);
				error ->
				    dict:store(Mod, nodata, ExpDict)
			    end;
			non_existing ->
			    dict:store(Mod, nodata, ExpDict)
		    end
	    end
    end.

-spec read_exports(mod_name(), string() | binary(), exp_dict()) -> exp_dict().
read_exports(Mod, ObjFile, ExpDict) ->
    case beam_lib:chunks(ObjFile, [exports]) of
	{ok,{Mod,[ModExported]}} ->
	    dict:store(Mod, {data,sets:from_list(ModExported)}, ExpDict);
	{error,beam_lib,_Reason} ->
	    dict:store(Mod, nodata, ExpDict)
    end.


%%------------------------------------------------------------------------------
%% ?FORALL detection functions
%%------------------------------------------------------------------------------

-spec rewrite_form(abs_form(), mod_info()) -> abs_form().
rewrite_form({attribute,Line,record,{RecName,FieldInits}}, ModInfo) ->
    NewFieldInits = [rewrite_field_init(F,ModInfo) || F <- FieldInits],
    {attribute,Line,record,{RecName,NewFieldInits}};
rewrite_form({function,Line,Name,Arity,Clauses}, ModInfo) ->
    NewClauses = [rewrite_clause(C,ModInfo) || C <- Clauses],
    {function,Line,Name,Arity,NewClauses};
rewrite_form(Form, _ModInfo) ->
    Form.

-spec rewrite_field_init(abs_rec_field(), mod_info()) -> abs_rec_field().
rewrite_field_init({record_field,_Line,_FieldName} = FieldInit, _ModInfo) ->
    FieldInit;
rewrite_field_init({record_field,Line,FieldName,InitExpr}, ModInfo) ->
    {record_field,Line,FieldName,rewrite_expr(InitExpr,ModInfo)}.

-spec rewrite_clause(abs_clause(), mod_info()) -> abs_clause().
rewrite_clause({clause,Line,Pattern,Guards,Body}, ModInfo) ->
    NewBody = [rewrite_expr(E,ModInfo) || E <- Body],
    {clause,Line,Pattern,Guards,NewBody}.

%% We haven't covered some constructs that are not expected to contain property
%% declarations.
-spec rewrite_expr(abs_expr(), mod_info()) -> abs_expr().
rewrite_expr({match,Line,Pattern,Expr}, ModInfo) ->
    {match,Line,Pattern,rewrite_expr(Expr,ModInfo)};
rewrite_expr({tuple,Line,FieldExprs}, ModInfo) ->
    NewFieldExprs = [rewrite_expr(F,ModInfo) || F <- FieldExprs],
    {tuple,Line,NewFieldExprs};
rewrite_expr({cons,Line,HeadExpr,TailExpr}, ModInfo) ->
    NewHeadExpr = rewrite_expr(HeadExpr, ModInfo),
    NewTailExpr = rewrite_expr(TailExpr, ModInfo),
    {cons,Line,NewHeadExpr,NewTailExpr};
rewrite_expr({op,Line,Op,LeftExpr,RightExpr}, ModInfo) ->
    NewLeftExpr = rewrite_expr(LeftExpr, ModInfo),
    NewRightExpr = rewrite_expr(RightExpr, ModInfo),
    {op,Line,Op,NewLeftExpr,NewRightExpr};
rewrite_expr({op,Line,Op,Expr}, ModInfo) ->
    {op,Line,Op,rewrite_expr(Expr,ModInfo)};
rewrite_expr({record,Line,RecName,FieldInits}, ModInfo) ->
    NewFieldInits = [rewrite_field_init(F,ModInfo) || F <- FieldInits],
    {record,Line,RecName,NewFieldInits};
rewrite_expr({record,Line,RecExpr,RecName,FieldInits}, ModInfo) ->
    NewRecExpr = rewrite_expr(RecExpr, ModInfo),
    NewFieldInits = [rewrite_field_init(F,ModInfo) || F <- FieldInits],
    {record,Line,NewRecExpr,RecName,NewFieldInits};
rewrite_expr({record_field,Line,RecExpr,RecName,FieldName}, ModInfo) ->
    {record_field,Line,rewrite_expr(RecExpr,ModInfo),RecName,FieldName};
rewrite_expr({'catch',Line,Expr}, ModInfo) ->
    {'catch',Line,rewrite_expr(Expr,ModInfo)};
rewrite_expr({call,Line,
	      {remote,_,{atom,_,proper},{atom,_,forall}} = FunRef,
	      [RawType,Prop]}, ModInfo) ->
    NewRawType = rewrite_type(RawType, ModInfo),
    NewProp = rewrite_expr(Prop, ModInfo),
    {call,Line,FunRef,[NewRawType,NewProp]};
rewrite_expr({call,Line,FunRef,Args}, ModInfo) ->
    NewArgs = [rewrite_expr(A,ModInfo) || A <- Args],
    {call,Line,FunRef,NewArgs};
rewrite_expr({lc,Line,Expr,GensAndFilters}, ModInfo) ->
    NewExpr = rewrite_expr(Expr, ModInfo),
    NewGensAndFilters = [rewrite_expr(W,ModInfo) || W <- GensAndFilters],
    {lc,Line,NewExpr,NewGensAndFilters};
rewrite_expr({generate,Line,Pattern,Expr}, ModInfo) ->
    {generate,Line,Pattern,rewrite_expr(Expr,ModInfo)};
rewrite_expr({block,Line,Body}, ModInfo) ->
    NewBody = [rewrite_expr(E,ModInfo) || E <- Body],
    {block,Line,NewBody};
rewrite_expr({'if',Line,Clauses}, ModInfo) ->
    NewClauses = [rewrite_clause(C,ModInfo) || C <- Clauses],
    {'if',Line,NewClauses};
rewrite_expr({'case',Line,Expr,Clauses}, ModInfo) ->
    NewExpr = rewrite_expr(Expr, ModInfo),
    NewClauses = [rewrite_clause(C,ModInfo) || C <- Clauses],
    {'case',Line,NewExpr,NewClauses};
rewrite_expr({'try',Line,Body1,Clauses1,Clauses2,Body2}, ModInfo) ->
    NewBody1 = [rewrite_expr(E,ModInfo) || E <- Body1],
    NewClauses1 = [rewrite_clause(C,ModInfo) || C <- Clauses1],
    NewClauses2 = [rewrite_clause(C,ModInfo) || C <- Clauses2],
    NewBody2 = [rewrite_expr(E,ModInfo) || E <- Body2],
    {'try',Line,NewBody1,NewClauses1,NewClauses2,NewBody2};
rewrite_expr({'fun',Line,{clauses,Clauses}}, ModInfo) ->
    NewClauses = [rewrite_clause(C,ModInfo) || C <- Clauses],
    {'fun',Line,{clauses,NewClauses}};
rewrite_expr(Expr, _ModInfo) ->
    Expr.


%%------------------------------------------------------------------------------
%% Type rewriting functions
%%------------------------------------------------------------------------------

-spec rewrite_type(abs_expr(), mod_info()) -> abs_expr().
rewrite_type({tuple,Line,FieldExprs}, ModInfo) ->
    NewFieldExprs = [rewrite_type(F,ModInfo) || F <- FieldExprs],
    {tuple,Line,NewFieldExprs};
rewrite_type({cons,Line,HeadExpr,TailExpr}, ModInfo) ->
    NewHeadExpr = rewrite_type(HeadExpr, ModInfo),
    NewTailExpr = rewrite_type(TailExpr, ModInfo),
    {cons,Line,NewHeadExpr,NewTailExpr};
rewrite_type({op,Line,'++',LeftExpr,RightExpr}, ModInfo) ->
    NewLeftExpr = rewrite_type(LeftExpr, ModInfo),
    NewRightExpr = rewrite_type(RightExpr, ModInfo),
    {op,Line,'++',NewLeftExpr,NewRightExpr};
rewrite_type({call,Line,{remote,_,{atom,_,Mod},{atom,_,Call}} = FunRef,
	      Args} = Expr,
	      #mod_info{name = ModName, helper_pid = HelperPid} = ModInfo) ->
    case is_exported_fun(Mod, Call, length(Args), HelperPid) of
	true ->
	    NewArgs = [rewrite_type(A,ModInfo) || A <- Args],
	    {call,Line,FunRef,NewArgs};
	false ->
	    builtin_type_call(ModName, Expr)
    end;
rewrite_type({call,Line,{atom,_,Fun} = FunRef,Args} = Expr,
	     #mod_info{name = ModName, in_scope = InScope} = ModInfo) ->
    case sets:is_element({Fun,length(Args)}, InScope) of
	true ->
	    NewArgs = [rewrite_type(A,ModInfo) || A <- Args],
	    {call,Line,FunRef,NewArgs};
	false ->
	    builtin_type_call(ModName, Expr)
    end;
rewrite_type(Expr, _ModInfo) ->
    Expr.

-spec builtin_type_call(mod_name(), abs_expr()) -> abs_expr().
builtin_type_call(ModName, Expr) ->
    AbsModName = {atom,0,ModName},
    AbsTypeStr = {string,0,lists:flatten(erl_pp:expr(Expr))},
    FunRef = {remote,0,{atom,0,proper_types},{atom,0,builtin_type}},
    {call,0,FunRef,[AbsModName,AbsTypeStr]}.
