Simple cache for [ezic] (https://github.com/drfloob/ezic).

Test scenario:
- requests for 1 day with 5 minute steps. Totally 288 items.

Without cache:
- it takes 60s

With cache:
- warmup takes 60s
- further requests (with repeated keys, of course) take 2ms

