-include("mcc.hrl").

-record(mcc_state, {os_env=[], app_env=[], config=[], redis, redis_sub, overlay, overlay_every, overlay_last, tref}).
-record(mcc_event_state, {mod, state}).

-define(MCC_OVERLAY_DIR, mcc_util:app_env(mcc, overlay_dir, code:root_dir() ++ "/etc/")).
-define(MCC_OVERLAY_FILE, mcc_util:app_env(mcc, overlay_file, "mcc_overlay.config")).
-define(MCC_OVERLAY_EVERY, mcc_util:app_env(mcc, overlay_every, undefined)).
-define(MCC_NAMESPACES, mcc_util:app_env(mcc, namespaces, undefined)).
-define(MCC_REDIS, mcc_util:app_env(mcc, redis, false)).
-define(MCC_REDIS_SERVER, mcc_util:app_env(mcc, redis_server, "127.0.0.1")).
-define(MCC_REDIS_PORT, mcc_util:app_env(mcc, redis_port, 6379)).

-define(info(F, A), error_logger:info_msg("[mcc] " ++ F, A)).

-ifdef(build_vsn).
-vsn(?build_vsn).
-endif. 
