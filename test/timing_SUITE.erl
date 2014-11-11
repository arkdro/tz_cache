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
    Local = get_local_config(Config),
    Dat = stub,
    ct:pal("dat: ~p", [Dat]),
    ok.

get_local_config(Config) ->
    File = "local.conf",
    Dir = ?config(data_dir, Config),
    Path = filename:join([Dir, File]),
    %% ct:pal("path: ~p", [Path]),
    {ok, [Local]} = file:consult(Path),
    %% ct:pal("local: ~p", [Local]),
    Local.

