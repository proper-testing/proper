%%% -*- coding: utf-8; erlang-indent-level: 2 -*-
%%% -------------------------------------------------------------------
%%% Copyright 2014 Motiejus Jakstys <desired.mta@gmail.com>
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

%%% @copyright 2014 Motiejus Jakstys
%%% @version {@version}
%%% @author Motiejus Jakstys

%%% @doc Unicode generators for PropEr
%%%
%%% This module exposes utf8 binary and string generators.
%%%
%%% Makes it easy to create custom-encoded unicode binaries and strings.
%%% For example, utf16 binary generator:
%%%
%%% ```
%%% utf16() ->
%%%     ?LET(S, utf8(), unicode:characters_to_binary(S, utf8, utf16)).
%%% '''
%%%
%%% Verify it has at least twice as many bytes as codepoints:
%%%
%%% ```
%%% ?FORALL(S, utf16(),
%%%         size(S) >= 2*length(unicode:characters_to_list(S, utf16))).
%%% '''

%%% Only utf8 generation is supported: see {@link utf8/0}, {@link utf8/1},
%%% {@link utf8/2} which generate binaries and the corresponding functions
%%% generating strings. Unicode codepoints and other encodings are trivial
%%% to get with utf8 generators and the {@link unicode} module in OTP.
-module(proper_unicode).

-export([utf8/0, utf8/1, utf8/2, utf8_string/0, utf8_string/1, utf8_string/2]).

-include("proper_common.hrl").

%% @private_type
%% @alias
-type nonnegextint() :: non_neg_integer() | 'inf'.


%% @doc utf8-encoded unbounded size binary.
-spec utf8() -> proper_types:type().
utf8() ->
    utf8(inf, 4).

%% @doc utf8-encoded bounded upper size binary.
-spec utf8(nonnegextint()) -> proper_types:type().
utf8(N) ->
    utf8(N, 4).

%% @doc Bounded upper size utf8 binary, `codepoint length =< MaxCodePointSize'.
%%
%% Limiting codepoint size can be useful when applications do not accept full
%% unicode range. For example, MySQL in utf8 encoding accepts only 3-byte
%% unicode codepoints in VARCHAR fields.
%%
%% If unbounded length is needed, use `inf' as first argument.
-spec utf8(nonnegextint(), 1..4) -> proper_types:type().
utf8(N, MaxCodePointSize) ->
    ?LET(Str,
         vector_upto(N, unicode_codepoint_upto(MaxCodePointSize)),
         unicode:characters_to_binary(Str)).


%% @doc utf8-encoded unbounded size string.
-spec utf8_string() -> proper_types:type().
utf8_string() ->
    utf8_string(inf, 4).

%% @doc utf8-encoded bounded upper size string.
-spec utf8_string(nonnegextint()) -> proper_types:type().
utf8_string(N) ->
    utf8_string(N, 4).

%% @doc Bounded upper size utf8 string, `codepoint length =< MaxCodePointSize'.
-spec utf8_string(nonnegextint(), 1..4) -> proper_types:type().
utf8_string(N, MaxCodePointSize) ->
    vector_upto(N, unicode_codepoint_upto(MaxCodePointSize)).

%% =============================================================================
%% Internal functions
%% =============================================================================

%% @doc codepoint up to N bytes in utf8
-spec unicode_codepoint_upto(1..4) -> proper_types:type().
unicode_codepoint_upto(N) ->
    proper_types:union([unicode_codepoint(X) || X <- lists:seq(1, N)]).

%% @doc Codepoint which is no more than N bytes in utf8
-spec unicode_codepoint(1..4) -> proper_types:type().
unicode_codepoint(1) ->
    proper_types:integer(0, 16#7F);
unicode_codepoint(2) ->
    proper_types:integer(16#80, 16#7FF);
unicode_codepoint(3) ->
    proper_types:union([proper_types:integer(16#800, 16#D7FF),
			proper_types:integer(16#E000, 16#FFFD)]);
unicode_codepoint(4) ->
    proper_types:integer(16#10000, 16#10FFFF).

%% =============================================================================
%% Helpers
%% =============================================================================

%% @doc List of no more than N elements
vector_upto(N, What) ->
    ?LET(X, proper_types:integer(0, N), proper_types:vector(X, What)).
