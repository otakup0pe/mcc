-include("mcc.hrl").

-record(mcc_state, {os_env=[], app_env=[], override=[], config=[], overlay, overlay_every, overlay_last, tref, yaml_overlay}).
-record(mcc_event_state, {mod, state}).

-define(MCC_OVERLAY_FILE, mcc_util:app_env(mcc, overlay_file, code:root_dir() ++ "/etc/overlay.config")).
-define(MCC_YAML_FILE, mcc_util:app_env(mcc, yaml_file, code:root_dir() ++ "/etc/overlay.yml")).
-define(MCC_OVERLAY_EVERY, mcc_util:app_env(mcc, overlay_every, undefined)).
-define(MCC_NAMESPACES, mcc_util:app_env(mcc, namespaces, undefined)).

-define(info(F, A), error_logger:info_msg("[mcc] " ++ F, A)).
-define(error(F, A), error_logger:error_msg("[mcc] " ++ F, A)).

-ifdef(build_vsn).
-vsn(?build_vsn).
-endif. 
