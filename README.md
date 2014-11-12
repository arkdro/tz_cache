Simple cache for [ezic] (https://github.com/drfloob/ezic).

Start
=====
In case of a release, it is unnecessary to start ezic from tz_cache. So
set {start_ezic, false} in app file.

When start ezic from tz_cache is necessary, set {start_ezic, true}.
Also configure dirs for tzdata and db files. The dirs must exist
before the application started.
Tzdata dir must contain time zone source files.

Test scenario
=============
- requests for 1 day with 5 minute steps. Totally 288 items.

Without cache:
- it takes 60s

With cache:
- warmup takes 60s
- further requests (with repeated keys, of course) take 2ms

