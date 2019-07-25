%%%-------------------------------------------------------------------
%%% @author huangyao
%%% @copyright (C) 2018, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 30. 八月 2018 16:05
%%%-------------------------------------------------------------------
-module(chat_room_user_info_server).

-author("huangyao").

-behaviour(gen_server).

-include("chat_room.hrl").

%% API
-export([start_link/0]).

%% gen_server callbacks
-export([init/1,
    handle_call/3,
    handle_cast/2,
    handle_info/2,
    terminate/2,
    code_change/3]).

-define(SERVER, ?MODULE).

-record(state, {}).

%%%===================================================================
%%% API
%%%===================================================================
start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================
init([]) ->
    {ok, #state{}}.

handle_call(_Request, _From, State) ->
    {reply, ok, State}.

handle_cast(_Request, State) ->
    {noreply, State}.

handle_info(Info, State) ->
    case Info of
        {save_user_info_to_table, UserInfo} ->
            ets:insert(?TALLUSERINFO, UserInfo),
        {noreply, State};

        _Info ->
            {noreply, State}
    end.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
