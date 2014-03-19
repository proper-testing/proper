%% @doc This module provides basic unicode generators.
%% @end
%%
%% @author Uvarov Michael <arcusfelis@gmail.com>
-module(proper_unicode).
-export([unicode_char/0,
         unicode_string/0,
         unicode_string/1,
         unicode_binary/0,
         unicode_binary/1,
         unicode_binary/2,
         unicode_characters/0,
         unicode_characters/1]).

-define(PROPER_NO_IMPORTS, true).
-import(proper_types, [char/0, list/1, vector/2,  
                       maybe_improper_list/2, resize/2, frequency/1]).
-include_lib("proper/include/proper.hrl").


-type generator() :: term().

-spec unicode_char() -> generator().
unicode_char() ->
    ?SIZED(Size, begin
%   io:format(user, "Call ~p~n", [Size]),
    frequency([{10, low_char()},
               {4,  high_char()},
               {2,  max_char()}]) end).

low_char() ->
    proper_char().

high_char() ->
    ?SIZED(Size, resize(Size * Size, proper_char())).

max_char() ->
    ?SIZED(Size, resize(Size * Size * Size * Size, proper_char())).

proper_char() ->
    ?SUCHTHAT(C, char(), (C < 16#D800 orelse C > 16#DFFF) andalso C =/= 16#FFFF 
              andalso C =/= 16#FFFE).


%% @doc Generate a list of unicode code points.
-spec unicode_string() -> generator().
unicode_string() ->
    list(unicode_char()).


%% @doc Generate a list of unicode code points of length `Size'.
-spec unicode_string(non_neg_integer()) -> generator().

unicode_string(Size) ->
    vector(Size, unicode_char()).


%% @doc Generate an unicode binary.
-spec unicode_binary() -> generator().
unicode_binary() ->
    unicode_binary(undefined, unicode).


%% @doc Generate an unicode binary binary.
-spec unicode_binary(Size | Encoding) -> generator() when
    Size :: non_neg_integer(),
    Encoding :: unicode:encoding().

unicode_binary(Size) when is_integer(Size) ->
    unicode_binary(Size, unicode);

unicode_binary(Encoding) ->
    unicode_binary(undefined, Encoding).


%% @doc Generate an unicode binary.
-spec unicode_binary(Size, Encoding) -> generator() when
    Size :: non_neg_integer() | undefined,
    Encoding :: unicode:encoding().

unicode_binary(undefined, Encoding) ->
    ?LET(Str, unicode_string(),
         unicode:characters_to_binary(Str, unicode, Encoding));

unicode_binary(Size, Encoding) ->
    ?LET(Str, unicode_string(Size),
         unicode:characters_to_binary(Str, unicode, Encoding)).


-spec unicode_characters() -> generator().

unicode_characters() ->
    unicode_characters(unicode).


%% `unicode_characters()' should not return a single `unicode_char()'.
-spec unicode_characters(Encoding) -> generator() when
    Encoding :: unicode:encoding().

unicode_characters(Encoding) ->
    ?SIZED(Size,
           frequency([{1, unicode_string()}, 
                      {1, unicode_binary(Encoding)}, 
                      {5, ?LAZY(resize(Size div 2, 
                                       unicode_characters1(Encoding)))}
                     ])).


unicode_characters1(Encoding) ->
    ?SIZED(Size, unicode_characters1(Size, Encoding)).


unicode_characters1(0, _Encoding) ->
    list(unicode_char());
unicode_characters1(Size, Encoding) ->
    Chars = ?LAZY(resize(Size, unicode_characters(Encoding))),
    maybe_improper_list(frequency([{10,unicode_char()}, {1, Chars}]),
                        unicode_binary(Encoding)).

