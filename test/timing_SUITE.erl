-module(timing_SUITE).

-include_lib("common_test/include/ct.hrl").

-compile([export_all]).

suite() ->
    [
     {timetrap, {seconds, 30}}
    ].

all() ->
    [
     {group, all}
    ].

groups() ->
    [
     {all, [], [
                {group, line1}
               ]},
     {line1, [], [
                  bunch1
                 ]}
    ].

init_per_suite(Config) ->
    Config.

end_per_suite(_Config) ->
    ok.

bunch1(Config) ->
    Dat = stub,
    ct:pal("dat: ~p", [Dat]),
    ok.

