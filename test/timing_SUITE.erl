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
    start_ezic(Config),
    Config.

end_per_suite(_Config) ->
    stop_ezic(),
    ok.

bunch1(Config) ->
    Local = get_local_config(Config),
    case is_bunch1_enabled(Local) of
        true ->
            bunch1_test(Config, Local);
        false ->
            skip
    end.

%% ===================================================================
%% Internal functions
%% ===================================================================

start_ezic(Config) ->
    App = ezic,
    Dir = build_abs_data_dir(Config),
    ok = application:load(App),
    ok = application:set_env(App, tzdata_dir, Dir),
    ok = application:start(App).

build_abs_data_dir(Config) ->
    Local = get_local_config(Config),
    Tzdata_dir = get_tzdata_dir(Local),
    Dir = ?config(data_dir, Config),
    filename:join([Dir, Tzdata_dir]).

get_tzdata_dir(Local) ->
    proplists:get_value(tzdata_dir, Local).

stop_ezic() ->
    application:stop(App).

is_bunch1_enabled(Local) ->
    Bunch = proplists:get_value(bunch1, Local, []),
    is_enabled(Bunch).

is_enabled(L) ->
    proplists:get_bool(enabled, L).

bunch1_test(_Config, Local) ->
    Reqs = create_bunch1_requests(Local),
    T1 = os:timestamp(),
    do_requests(Reqs),
    T2 = os:timestamp(),
    Dur = timer:now_diff(T2, T1),
    ct:pal("bunch1 dur: ~p", [Dur]),
    ok.

create_bunch1_requests(Local) ->
    erlang:error(not_implemented).

do_requests(Reqs) ->
    ezic:local_to_utc(local_datetime(), TimeZone)
    erlang:error(not_implemented).

get_local_config(Config) ->
    File = "local.conf",
    Dir = ?config(data_dir, Config),
    Path = filename:join([Dir, File]),
    %% ct:pal("path: ~p", [Path]),
    {ok, [Local]} = file:consult(Path),
    %% ct:pal("local: ~p", [Local]),
    Local.

