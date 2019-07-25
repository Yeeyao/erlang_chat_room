%%%-------------------------------------------------------------------
%%% @author huangyao
%%% @copyright (C) 2018, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 21. 八月 2018 15:04
%%%-------------------------------------------------------------------
-module(chat_room_test_server).

-author("huangyao").

-behaviour(gen_server).

-include("chat_room.hrl").

-define(TYPENUM, 8).

-define(UPLOADTIME, 30000).

%% API
-export([
    start/0
]).

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
start() ->
    start_link().

start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================
init([]) -> %% listener name, transport handler, args, protocol handler
    do_init(),
%%    {ok, _Pid} = ranch:start_listener(chat_room,
%%        ranch_tcp, [{port, 1234}, {num_acceptors, 100}],  %% 可以选择supervisor的行为模式
%%        chat_room_protocol_server, []),
    {ok, _Pid} = ranch:start_listener(chat_room,
        ranch_tcp, #{
            num_acceptors => 100,
            socket_opts => [{port, 1234}]
        }, chat_room_protocol_server, []),
    {ok, #state{}}.

handle_call(_Request, _From, State) ->
    {reply, ok, State}.

handle_cast(_Request, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
do_init() ->
    start_app(),
    create_ets(),
    channel_start(),
    user_info_server_start().

channel_start() ->
    ChannelIDList = [ch0, ch1, ch2, ch3, ch4, ch5],
    lists:foreach(
        fun(ChannelID) ->
            chat_channel_server:start(ChannelID)
        end,
        ChannelIDList
    ).

%%    ChannelIDList = lists:seq(0, 5),
%%    lists:foreach(
%%        fun(ChannelID) ->
%%            ChannelNameList = lists:concat([ch, ChannelID]),
%%            ChannelName = ic_pragma:list_to_term(ChannelNameList),
%%            chat_channel_server:start(ChannelName)
%%        end,
%%        ChannelIDList
%%    ).

user_info_server_start() ->
    chat_room_user_info_server:start_link().

start_app() ->
    ok = application:start(crypto),
    ok = application:start(asn1),
    ok = application:start(public_key),
    ok = application:start(ssl),
    ok = application:start(ranch).

create_ets() ->
    ets:new(?TALLUSERINFO, [public, set, named_table, {keypos, #r_user.name}]),
    ets:new(?TSERVERCHANNEL, [public, set, named_table, {keypos, #r_channel.id}]),
    ets:new(?TDEBUGFLAT, [public, set, named_table]).