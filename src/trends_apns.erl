%%----------------------------------------------------------------------
%% File     :trends_apns.erl
%% Autho    :langxianzhe@163.com
%% Description: public fuction
%% Created : 2012-10-25
%%----------------------------------------------------------------------
-module(trends_apns).

%% API
-export([get_connect/0,
         close_connect/1,
         send/2,
         send/3,
         send_with_extra/4,
         send_with_extra/3,
         error_fun/2,
         resend/1,
         feedback_fun/1
        ]).

-record(apns_msg, {id = apns:message_id()       :: binary(), 
                   expiry = apns:expiry(86400)  :: non_neg_integer(), %% default = 1 day
                   device_token                 :: string(),
                   alert = none                 :: none | apns:alert(),
                   badge = none                 :: none | integer(),               
                   sound = none                 :: none | string(),                
                   extra = []                   :: [apns_mochijson2:json_property()]}).


-record(loc_alert, {body    = none  :: none | string(), 
                    action  = none  :: none | string(),
                    key     = ""    :: string(),
                    args    = []    :: [string()],
                    image   = none  :: none | string()}).



-include("eapns.hrl").


close_connect(ConnId) ->
    apns:disconnect(ConnId).

get_connect() ->
    case  whereis(trends_apns)  of
        undefined ->
            case apns:connect(?MODULE, fun error_fun/2, fun feedback_fun/1) of
                {already_started, Pid} -> Pid;
                {ok, Pid}  -> Pid
            end;
        Pid -> Pid
    end.

error_fun(MsgId,  Result) ->
    ?DEBUG("~p:error_fun ~p MsgId=~p, Result=~p ", [?MODULE, ?LINE, MsgId, Result]),
    case mnesia:dirty_index_read(legal_apns_token,  MsgId, msg_id) of
        [LegalApnsToken] ->
            Token = LegalApnsToken#legal_apns_token.token,
            Id = "trends_token-"++ Token,
            ?DEBUG("~p:error_fun ~p MsgId=~p, Result=~p ,Token=~p", [?MODULE, ?LINE, MsgId, Result, Token]),
            %mnesia:dirty_delete_object(LegalApnsToken),
            eapns_service:reconnect(fun resend/1,  Token),
            delete_token(Id);
        [] -> ok
    end.

feedback_fun(Data) ->
    ?DEBUG("~p:feedback_fun ~p X=~p ", [?MODULE, ?LINE, Data]),
    {_Time, Token} = Data,
    Key = string:to_lower(Token),
    Id = "trends_token-"++ Key,
    mnesia:dirty_delete({legal_apns_token, Key}),
    delete_token(Id).

resend(Token) ->
    ?DEBUG("~p:resend ~p Token=~p ", [?MODULE, ?LINE, Token]),
    List = lists:sort(mnesia:dirty_all_keys(legal_apns_token)),
    {_Success, [Token| Failures]} = lists:splitwith(fun(A) -> A/=Token end, List), 
    case mnesia:dirty_read({legal_apns_token, Token}) of
        [LAToken] ->
            mnesia:dirty_delete({legal_apns_token, Token}),
            resend_alert({Failures, LAToken#legal_apns_token.alert});
        [] -> ok
    end.

    
delete_token(Id) ->
    case boss_db:find(Id) of
        {error, Error} ->
            ?ERROR("~p:error_fun ~p Error=~p", [?MODULE, ?LINE, Error]);
        _Ohter ->
            boss_db:delete(Id)
    end,
    ok.


send_with_extra(Token, _ConnId, Msg, Extra) ->
    Alert = #apns_msg{device_token = Token,
                      %id = list_to_binary(Token),
                      badge = 1,
                      sound = "bingbong.aiff",
                      %extra = [{"jump","yes"}],
                      extra = [{extra, Extra}],
                      alert = #loc_alert{body = Msg
                                        }}, 
    ?DEBUG("~p:appns_packet ~p Alert = ~p ~n", [?MODULE, ?LINE, Alert]),
    ?DEBUG("~p:appns_packet ~p Token = ~p Alert.id=~p ,~n", [?MODULE, ?LINE, Token, Alert#apns_msg.id]),
    mnesia:dirty_write(#legal_apns_token{token=Token, msg_id=Alert#apns_msg.id, alert=Alert}),
    eapns_service:send_message(Alert).

send_with_extra(Tokens, Msg, Extra) ->
    clear_legal_apns_token(),
    F = fun(Token) -> 
	    receive after 100 ->  ok end,
            send_with_extra(Token, connId, Msg, Extra)
        end,
    lists:foreach(F, lists:sort(Tokens)).



send(Token, _ConnId, Msg) ->
     Alert = #apns_msg{device_token = Token,
                       badge = 1,
                       sound = "bingbong.aiff",
                       %extra = [{"jump","yes"}],
                       extra = [{jump, yes}],
                       alert = #loc_alert{body = Msg
                                        }}, 
    ?DEBUG("appns_packet ~p Alert = ~p ~n", [?MODULE, ?LINE, Alert]),
    mnesia:dirty_write(#legal_apns_token{token=Token, msg_id=Alert#apns_msg.id, alert=Alert}),
    eapns_service:send_message(Alert).


send(Tokens, Msg) ->
    clear_legal_apns_token(),
    F = fun(Token) -> 
            send(Token, connId, Msg)
        end,
    lists:foreach(F, lists:sort(Tokens)).


resend_alert({Tokens, Alert}) ->
    %clear_legal_apns_token(),
    ?DEBUG("~p:resend_alert ~p Tokens=~p Alert = ~p ~n", [?MODULE, ?LINE, Tokens, Alert]),
    F = fun(Token) -> 
	    receive after 100 ->  ok end,
            resend_alert(Token, Alert)
        end,
    lists:foreach(F, lists:sort(Tokens)).
resend_alert(Token, Alert) ->
    NewAlert = Alert#apns_msg{id=apns:message_id(), device_token=Token},
    ?DEBUG("~p:resend_alert ~p Token = ~p Alert=~p ,~n", [?MODULE, ?LINE, Token, NewAlert]),
    mnesia:dirty_write(#legal_apns_token{token=Token, msg_id=NewAlert#apns_msg.id, alert=NewAlert}),
    eapns_service:send_message(NewAlert).

clear_legal_apns_token() ->
    mnesia:clear_table(legal_apns_token).

