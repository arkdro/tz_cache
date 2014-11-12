%%%
%%% API for tz_cache
%%%

-module(tz_cache).

-export([
         utc_to_local/2,
         local_to_utc/2
        ]).

utc_to_local(UTC_datetime, Tz_name) ->
    tz_cache_srv:utc_to_local(UTC_datetime, Tz_name).

local_to_utc(Local_datetime, Tz_name) ->
    tz_cache_srv:local_to_utc(Local_datetime, Tz_name).

