-ifndef(MCC_HRL).
-define(MCC_HRL, true).
-record(mcc, {name, key, value, old_value}).

-define(cfg(N, K, D), mcc:get(N, K, D)).
-endif.
