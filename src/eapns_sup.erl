
-module(eapns_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

-include("eapns.hrl").
%% Helper macro for declaring children of supervisor
-define(CHILD(I, Type), {I, {I, start_link, []}, permanent, 5000, Type, [I]}).

%% ===================================================================
%% API functions
%% ===================================================================

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

init([]) ->
    inets:start(),     
    init_db(),

    EapnsService = {eapns_service, 
                    {eapns_service, start_link, []},
                     permanent, 5000, worker, [eapns_service]},
    Proces = [EapnsService],
    {ok, { {one_for_one, 5, 10}, Proces} }.

init_db() ->
    application:start(mnesia),
    mnesia:create_schema([node()]),
    R = 
    mnesia:create_table(legal_apns_token,
                        [{ram_copies, [node()|nodes()]},   
                         {attributes, record_info(fields, legal_apns_token)}]),
    io:format("R===~p~n",[{?MODULE, R}]),
    mnesia:add_table_index(legal_apns_token, msg_id).

