-module(timing_SUITE).

-include_lib("common_test/include/ct.hrl").

-compile([export_all]).

suite() ->
    [
     {timetrap, {seconds, 180}}
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
                  compare,
                  bunch2,
                  bunch1
                 ]}
    ].

init_per_suite(Config) ->
    start_tz_cache(Config),
    start_ezic(Config),
    Config.

end_per_suite(_Config) ->
    stop_tz_cache(),
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

bunch2(Config) ->
    Local = get_local_config(Config),
    case is_bunch2_enabled(Local) of
        true ->
            bunch2_test(Config, Local);
        false ->
            skip
    end.

compare(Config) ->
    ok = compare_one_item({{2014,10,15}, {1,5,3}}, "Rome"), %% no zone
    ok = compare_one_item({{2014,10,15}, {2,5,3}}, "Europe/Rome"),
    ok = compare_one_item({{2013,11,25}, {21,56,13}}, "PCT"), %% no zone
    ok = compare_one_item({{2013,11,25}, {22,56,13}}, "America/Phoenix"),
    ok = compare_one_item({{2011,6,1}, {15,35,55}}, "Pacific/Norfolk"),
    ok.

%% ===================================================================
%% Internal functions
%% ===================================================================


start_ezic(Config) ->
    App = ezic,
    Tz_dir = build_abs_data_dir(Config),
    Db_dir = build_abs_db_dir(Config),
    ok = application:load(App),
    ok = application:set_env(App, tzdata_dir, Tz_dir),
    ok = application:set_env(App, db_dir, Db_dir),
    ok = application:start(App).

start_tz_cache(_Config) ->
    ok = application:start(tz_cache).

build_abs_data_dir(Config) ->
    Local = get_local_config(Config),
    Tzdata_dir = get_tzdata_dir(Local),
    Dir = ?config(data_dir, Config),
    filename:join([Dir, Tzdata_dir]).

build_abs_db_dir(Config) ->
    Local = get_local_config(Config),
    Db_dir = get_db_dir(Local),
    Dir = ?config(data_dir, Config),
    filename:join([Dir, Db_dir]).

get_tzdata_dir(Local) ->
    proplists:get_value(tzdata_dir, Local).

get_db_dir(Local) ->
    proplists:get_value(db_dir, Local).

stop_ezic() ->
    application:stop(ezic).

stop_tz_cache() ->
    application:stop(tz_cache).

is_bunch1_enabled(Local) ->
    Bunch = proplists:get_value(bunch1, Local, []),
    is_enabled(Bunch).

is_bunch2_enabled(Local) ->
    Bunch = proplists:get_value(bunch2, Local, []),
    is_enabled(Bunch).

is_enabled(L) ->
    proplists:get_bool(enabled, L).

bunch2_test(_, Local) ->
    Config = get_bunch2_config(Local),
    Reqs = create_bunch1_requests(Config),
    T1 = os:timestamp(),
    cache_warmup(Reqs),
    T2 = os:timestamp(),
    do_requests_with_cache(Reqs),
    T3 = os:timestamp(),
    Total = timer:now_diff(T3, T1),
    Dur = timer:now_diff(T3, T2),
    ct:pal("bunch2 dur: ~p, total: ~p", [Dur, Total]),
    ct:pal("bunch2 info: ~p", [ets:info(tz_cache_srv)]),
    ok.

bunch1_test(_, Local) ->
    Config = get_bunch1_config(Local),
    Reqs = create_bunch1_requests(Config),
    T1 = os:timestamp(),
    do_requests(Reqs),
    T2 = os:timestamp(),
    Dur = timer:now_diff(T2, T1),
    ct:pal("bunch1 dur: ~p", [Dur]),
    ok.

get_bunch2_config(L) ->
    proplists:get_value(bunch2, L).

get_bunch1_config(L) ->
    proplists:get_value(bunch1, L).

create_bunch1_requests(Config) ->
    Start = get_start(Config),
    Stop = get_stop(Config),
    Step = get_step(Config),
    Zone = get_zone(Config),
    create_bunch1_requests(Start, Stop, Step, Zone).

create_bunch1_requests(Start, Stop, Step, Zone) ->
    T1 = calendar:datetime_to_gregorian_seconds(Start),
    T2 = calendar:datetime_to_gregorian_seconds(Stop),
    Dt = calendar:time_to_seconds(Step),
    create_bunch1_requests(T1, T2, Dt, Zone, []).

create_bunch1_requests(Cur, Stop, _, _, Acc) when Cur > Stop ->
    lists:reverse(Acc);
create_bunch1_requests(Cur, Stop, Step, Zone, Acc) ->
    D = calendar:gregorian_seconds_to_datetime(Cur),
    Req = {D, Zone},
    create_bunch1_requests(Cur + Step, Stop, Step, Zone, [Req | Acc]).

get_zone(L) ->
    proplists:get_value(tz, L).

get_start(L) ->
    proplists:get_value(start, L).

get_stop(L) ->
    proplists:get_value(stop, L).

get_step(L) ->
    proplists:get_value(step, L).

cache_warmup(Reqs) ->
    do_requests_with_cache(Reqs).

do_requests(Reqs) ->
    [ezic:local_to_utc(Datetime, TimeZone) || {Datetime, TimeZone} <- Reqs].

do_requests_with_cache(Reqs) ->
    [tz_cache:local_to_utc(Datetime, TimeZone) || {Datetime, TimeZone} <- Reqs].

get_local_config(Config) ->
    File = "local.conf",
    Dir = ?config(data_dir, Config),
    Path = filename:join([Dir, File]),
    %% ct:pal("path: ~p", [Path]),
    {ok, [Local]} = file:consult(Path),
    %% ct:pal("local: ~p", [Local]),
    Local.

compare_one_item(Datetime, Timezone) ->
    compare_one_item_local_to_utc(Datetime, Timezone),
    compare_one_item_utc_to_local(Datetime, Timezone),
    ok.

compare_one_item_local_to_utc(Datetime, Timezone) ->
    Res1 = ezic:local_to_utc(Datetime, Timezone),
    ct:pal("res1l: ~p, ~p, ~p", [Datetime, Timezone, Res1]),
    Res2 = tz_cache:local_to_utc(Datetime, Timezone),
    ct:pal("res2l: ~p, ~p, ~p", [Datetime, Timezone, Res2]),
    Res1 = Res2,
    ok.

compare_one_item_utc_to_local(Datetime, Timezone) ->
    Res1 = ezic:utc_to_local(Datetime, Timezone),
    ct:pal("res1u: ~p, ~p, ~p", [Datetime, Timezone, Res1]),
    Res2 = tz_cache:utc_to_local(Datetime, Timezone),
    ct:pal("res2u: ~p, ~p, ~p", [Datetime, Timezone, Res2]),
    Res1 = Res2,
    ok.

