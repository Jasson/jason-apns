%%%-------------------------------------------------------------------  
%%% File    : eapns_service.erl
%%% Author  : langxianzhe@gmail.com 
%%% Description : recording the  flowers and favorite 
%%%    
%%% Created : 2013-08-20 
%%% Update  :  
%%%-------------------------------------------------------------------


-module(eapns_service).
-behaviour(gen_server).

%% API.
-export([start_link/0]).

%% gen_server.
-export([init/1]).
-export([handle_call/3]).
-export([handle_cast/2]).
-export([handle_info/2]).
-export([terminate/2]).
-export([code_change/3, 
         send_message/1,
         reconnect/2
         ]).
-include("eapns.hrl").



-record(state, {pid=[]}).

%% API.
send_message(Msg) ->
    gen_server:cast(?MODULE, {send, Msg}).

reconnect(ResendFun, Token) ->
    gen_server:cast(?MODULE, {reconnect, ResendFun, Token}).

%% @doc Start the eapns gen_server.
-spec start_link() -> {ok, pid()}.
start_link() ->
	gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).
%% gen_server.

%% @private
init([]) ->
    process_flag(trap_exit, true),
    Pid = trends_apns:get_connect(),
    ?DEBUG("~p:init ~p Msg=~p", [?MODULE, ?LINE, Pid]),
    {ok, #state{pid=[Pid]}}.

handle_call(_Request, _From, State) ->
    {reply, ignore, State}.

handle_cast({reconnect, ResendFun, Token}, #state{pid=[Pid]}=State) ->
    ?DEBUG("~p:handle_cast ~p reconnect Pid=~p", [?MODULE, ?LINE, Pid]),
    receive after 5000 ->ok end,
    NewPid = 
        case is_process_alive(Pid) of
            false ->
                ?DEBUG("~p:handle_cast ~p reconnect", [?MODULE, ?LINE]),
                trends_apns:get_connect();
            true ->
                ?DEBUG("~p:handle_cast ~p reconnect", [?MODULE, ?LINE]),
                Pid
        end,
    ?DEBUG("~p:handle_cast ~p reconnect", [?MODULE, ?LINE]),
    trends_apns:resend(Token),
    ?DEBUG("~p:handle_cast ~p reconnect", [?MODULE, ?LINE]),
    {noreply, State#state{pid=[NewPid]}};
handle_cast({send, Msg}, #state{pid=[Pid]}=State) ->
    ?DEBUG("~p:handel_cast ~p Pid=~p", [?MODULE, ?LINE, is_process_alive(Pid)]),
    NewPid = 
        case is_process_alive(Pid) of
            false ->
                trends_apns:get_connect();
            true ->
                Pid
        end,
    apns:send_message(NewPid, Msg),
    ?DEBUG("~p:handel_cast ~p Msg=~p", [?MODULE, ?LINE, Msg]),
    {noreply, State#state{pid=[NewPid]}}.


handle_info({'EXIT', _From, _Reason}, State) ->
    ?DEBUG("handle_cast ~p Info=~p", [?MODULE, ?LINE, {'EXIT', _From, _Reason}]),
    Pid = trends_apns:get_connect(),
    {noreply, State#state{pid=[Pid]}};
handle_info(_Info, State) ->
    ?DEBUG("handle_cast ~p Info=~p", [?MODULE, ?LINE, _Info]),
    {noreply, State}.

%% @private
terminate(_Reason, _State) ->
    ok.

%% @private
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.







