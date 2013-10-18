-define(DEBUG(Input, Args), 
        error_logger:info_msg(Input ++ "~n", Args)).

-define(ERROR(Input, Args), 
        error_logger:error_msg(Input ++ "~n", Args)).

-record(legal_apns_token, {token, msg_id, alert}).

