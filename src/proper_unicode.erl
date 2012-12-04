%% @doc This module provides basic unicode generators.
%% @end
%% @todo add `unicode_characters'.
-module(proper_unicode).
-export([unicode_char/0,
         unicode_string/0,
         unicode_string/1,
         unicode_binary/0,
         unicode_binary/1]).

-define(PROPER_NO_IMPORTS, true).
-import(proper_types, [char/0, list/1, vector/2]).
-include_lib("proper/include/proper.hrl").


-type generator() :: term().

-spec unicode_char() -> generator().
unicode_char() ->
    ?SUCHTHAT(C, char(), (C < 16#D800 orelse C > 16#DFFF) andalso C =/= 16#FFFF 
                                                          andalso C =/= 16#FFFE).

-spec unicode_string() -> generator().
unicode_string() ->
    list(unicode_char()).


%% @doc Generate `Len' code points as a list.
-spec unicode_string(Len) -> generator()
    when Len :: non_neg_integer().

unicode_string(Len) ->
    vector(Len, unicode_char()).


%% @doc Generate `Len' code points as a binary.
-spec unicode_binary() -> generator().
unicode_binary() ->
    ?LET(S, unicode_string(), unicode:characters_to_binary(S)).


%% @doc Generate a unicode characters of length `Len'.
-spec unicode_binary(Len) -> generator() 
    when Len :: non_neg_integer().

unicode_binary(Len) ->
    ?LET(S, unicode_string(Len), unicode:characters_to_binary(S)).

