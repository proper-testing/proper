-module(proper_unicode_tests).

%% ------------------------------------------------------------------
%% Tests
%% ------------------------------------------------------------------

-include_lib("proper/include/proper.hrl").
-include_lib("eunit/include/eunit.hrl").


%% ------------------------------------------------------------------
%% Call test generators
%% ------------------------------------------------------------------

prop_unicode_char() ->
    ?FORALL(Char, unicode_char(),
        begin
%           io:format(user, "~p~n", [Char]),
            true
        end).

prop_unicode_binary() ->
    ?FORALL(Bin, unicode_binary(),
        begin
        equals(Bin, unicode:characters_to_binary(
                    unicode:characters_to_list(Bin)))
        end).

%% Check a binary generator with fixed length.
prop_sized_unicode_binary() ->
    ?FORALL({Len, Bin}, ?LET(Len, byte(), {Len, unicode_binary(Len)}),
        begin
            equals(Len, length(unicode:characters_to_list(Bin)))
        end).

prop_unicode_string() ->
    ?FORALL(Str, unicode_string(),
        begin
%           io:format(user, "~p~n", [Str]),
        equals(Str, unicode:characters_to_list(
                    unicode:characters_to_binary(Str)))
        end).


prop_unicode_characters() ->
    ?FORALL(Chars, unicode_characters(),
        begin
%           io:format(user, "~p~n", [Chars]),
            is_binary(unicode:characters_to_binary(Chars))
        end).


encoding() ->
    [unicode, utf8, utf16, {utf16, little}, {utf16, big}, utf32, 
     {utf32, little}, {utf32, big}].


prop_unicode_external_characters() ->
    ?FORALL({Encoding, Chars}, 
         oneof([{Encoding, unicode_characters(Encoding)} 
                || Encoding <- encoding()]),
            begin
                List = unicode:characters_to_list(Chars, Encoding),
%               [erlang:error(1) || 
%                length(List) > 2, hd(tl(List)) > 1000000],
%               io:format(user, "~p~n", [Chars]),
                is_binary(unicode:characters_to_binary(Chars, Encoding))
            end).


%% -------------------------------------------------------------------
%% Property Testing
%% -------------------------------------------------------------------

run_property_testing_test_() ->
    {timeout, 60, fun run_property_testing_case/0}.
    
run_property_testing_case() ->
    EunitLeader = erlang:group_leader(),
    erlang:group_leader(whereis(user), self()),
    Res = proper:module(?MODULE, []),
    erlang:group_leader(EunitLeader, self()),
    ?assertEqual([], Res). 
