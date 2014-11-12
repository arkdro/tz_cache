%%%-------------------------------------------------------------------
%%% @doc
%%% Cache for ezic.
%%%-------------------------------------------------------------------
-module(tz_cache_srv).

-behaviour(gen_server).

%% API
-export([
         utc_to_local/2,
         local_to_utc/2,
         start_link/0
        ]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-define(SERVER, ?MODULE).

-record(state, {}).

%%%===================================================================
%%% API
%%%===================================================================

start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

utc_to_local(UTC_datetime, Tz_name) ->
    Tab = tab(),
    Req = make_req_utc_to_local(UTC_datetime, Tz_name),
    case ets:lookup(Tab, Req) of
        [] ->
            gen_server:call(?SERVER, Req);
        [{_, Val, _}] ->
            Val
    end.

local_to_utc(Local_datetime, Tz_name) ->
    Tab = tab(),
    Req = make_req_local_to_utc(Local_datetime, Tz_name),
    case ets:lookup(Tab, Req) of
        [] ->
            gen_server:call(?SERVER, Req);
        [{_, Val, _}] ->
            Val
    end.

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

init([]) ->
    ets:new(tab(), [named_table, protected]),
    set_clean_timer(),
    start_delayed_ezic(),
    {ok, #state{}}.

handle_call({utc_to_local, _, _} = Req, From, State) ->
    get_data_common(Req, From),
    {noreply, State};
handle_call({local_to_utc, _, _} = Req, From, State) ->
    get_data_common(Req, From),
    {noreply, State};
handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(start_ezic, State) ->
    start_ezic(),
    {noreply, State};
handle_info(clean, State) ->
    N = clean_old_data(),
    error_logger:info_report({cleaned, N}),
    set_clean_timer(),
    {noreply, State};
handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

tab() ->
    ?SERVER.

default_clean_interval() ->
    60.

default_ttl() ->
    1800.

make_req_utc_to_local(UTC_datetime, Tz_name) ->
    {utc_to_local, UTC_datetime, Tz_name}.

make_req_local_to_utc(Local_datetime, Tz_name) ->
    {local_to_utc, Local_datetime, Tz_name}.

get_data_common(Req, From) ->
    Truncated = truncate_request(Req),
    Reply = get_data_from_ezic(Truncated),
    gen_server:reply(From, Reply),
    store_to_cache(Truncated, Reply).

get_data_from_ezic({utc_to_local, UTC_datetime, Tz_name}) ->
    ezic:utc_to_local(UTC_datetime, Tz_name);
get_data_from_ezic({local_to_utc, Local_datetime, Tz_name}) ->
    ezic:local_to_utc(Local_datetime, Tz_name).

store_to_cache(Req, Reply) ->
    Ts = get_timestamp(),
    Item = {Req, Reply, Ts},
    ets:insert(tab(), Item).

get_timestamp() ->
    {MS, S, _} = os:timestamp(),
    MS * 1000000 + S.

set_clean_timer() ->
    Interval = get_clean_interval(),
    erlang:send_after(timer:seconds(Interval), self(), clean).

get_clean_interval() ->
    case application:get_env(clean_interval) of
        undefined ->
            default_clean_interval();
        {ok, Val} ->
            Val
    end.

get_ttl() ->
    case application:get_env(ttl) of
        undefined ->
            default_ttl();
        {ok, Val} ->
            Val
    end.

clean_old_data() ->
    Cur = get_timestamp(),
    Ttl = get_ttl(),
    Item = {'_', '_', '$1'},
    Guards = [
              {'>',
               {'-', Cur, '$1'},
               Ttl
              }
             ],
    Spec = [{Item, Guards, [true]}],
    ets:select_delete(tab(), Spec).

start_delayed_ezic() ->
    case application:get_env(start_ezic) of
        {ok, true} ->
            erlang:send_after(0, self(), start_ezic);
        _ ->
            skip
    end.

start_ezic() ->
    {ok, Tz_dir} = application:get_env(tzdata_dir),
    {ok, Db_dir} = application:get_env(db_dir),
    App = ezic,
    ok = application:load(App),
    ok = application:set_env(App, tzdata_dir, Tz_dir),
    ok = application:set_env(App, db_dir, Db_dir),
    ok = application:start(App).

truncate_request(Req) ->
    case application:get_env(truncate) of
        undefined ->
            Req;
        {ok, Val} ->
            truncate_request(Val, Req)
    end.

truncate_request(second, {Type, {Date, {Hour, Min, _}}, Zone}) ->
    {Type, {Date, {Hour, Min, 0}}, Zone};
truncate_request(minute, {Type, {Date, {Hour, _, _}}, Zone}) ->
    {Type, {Date, {Hour, 0, 0}}, Zone};
truncate_request(hour, {Type, {Date, _}, Zone}) ->
    {Type, {Date, zero_time()}, Zone};
truncate_request(day, {Type, {{Y, M, _}, _}, Zone}) ->
    {Type, {{Y, M, 1}, zero_time()}, Zone};
truncate_request(month, {Type, {{Y, _, _}, _}, Zone}) ->
    {Type, {{Y, 1, 1}, zero_time()}, Zone};
truncate_request(year, {Type, _, Zone}) ->
    {Type, {{0, 1, 1}, zero_time()}, Zone}.

zero_time() ->
    {0, 0, 0}.

