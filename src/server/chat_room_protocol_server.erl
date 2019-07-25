%%%-------------------------------------------------------------------
%%% @author huangyao
%%% @copyright (C) 2018, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 29. 八月 2018 15:33
%%%-------------------------------------------------------------------
-module(chat_room_protocol_server).
-author("huangyao").

-behaviour(gen_server).

-behaviour(ranch_protocol).

-include("chat_room.hrl").
-include("protocol.hrl").

-define(MAXCONNECTIONS, 10000).

%% API
-export([
    start_link/4,
%%    start/0,
    add_channel/1,
    get_rand_channel/0
]).

%% gen_server callbacks
-export([init/1,
    handle_call/3,
    handle_cast/2,
    handle_info/2,
    terminate/2,
    code_change/3]).

-define(SERVER, ?MODULE).

-record(state, {t_socket, c_socket}).

%%%===================================================================
%%% API
%%%===================================================================
start_link(Ref, _Socket, Transport, Opts) ->
    {ok, proc_lib:spawn_link(?MODULE, init, [{Ref, Transport, Opts}])}.

add_channel(ChannelInfo) ->
    ets:insert(?TSERVERCHANNEL, ChannelInfo).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================
init({Ref, Transport, _Opts = []}) ->
    {ok, Socket} = ranch:handshake(Ref),  %% Transport Socket
    ranch:set_max_connections(Ref, ?MAXCONNECTIONS),
    ok = Transport:setopts(Socket, [{active, true}, {packet, 4}]),
    gen_server:enter_loop(?MODULE, [], #state{t_socket = Socket}).

handle_call(_Request, _From, State) ->
    {reply, ok, State}.

handle_cast(_Request, State) ->
    {noreply, State}.

handle_info({tcp, Socket, Bin}, State) ->
    ?DEBUG("protocol recv bin: ~w~n", [Bin]),
    [_ProtNum, Msg] = protocol:decode(Bin),
    {UserName} = Msg,
    IsOnline = is_name_exist(UserName),
    case IsOnline of
        true ->
            ReplyStr = "Sorry, Name already in use!",
            Reply = protocol:encode(1001, #r_login_sc{msg = ReplyStr}),
            gen_tcp:send(Socket, Reply),
            gen_tcp:close(Socket),
            NewState = State,
            ?MODULE ! {stop};
        false ->
            ReplyStr = "Connect established",
            Reply = protocol:encode(1001, #r_login_sc{msg = ReplyStr}),
            gen_tcp:send(Socket, Reply),
            InitPos = get_rand_pos(), %% 初始化时分配名字和世界频道，位置，时间
            CurChannelID = get_rand_channel(),
            {ok, Pid} = chat_connect_client_server:start_link(#r_user{name = UserName,
                current_chid = CurChannelID, channel_list = [CurChannelID, ?WORLDCHANNELID],
                pos = InitPos, socket = Socket}),
            gen_tcp:controlling_process(Socket, Pid),
            server_debug("[~w]: login, pos:~w~n", [UserName, InitPos]),
            NewState = State#state{c_socket = Socket};
        _ ->
            NewState = State,
            ?ERRORLOG("Error return type")
    end,
    {noreply, NewState};

handle_info({tcp_closed, _Socket}, State) ->
    ?DEBUG("User disconnect~n"),
    {noreply, State};

handle_info({stop}, State) ->
    {stop, normal, State};

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
is_name_exist(UserName) ->
    case ets:lookup(?TALLUSERINFO, UserName) of
        [] ->
            false;
        _ ->
            true
    end.

get_rand_pos() -> %% 这里两个用户位置重叠不做处理
    random:seed(erlang:now()),
    HorCoor = random:uniform(?HORMAX) - 1 + ?HORMIN,
    VerCoor = random:uniform(?VERMAX) - 1 + ?VERMIN,
    {HorCoor, VerCoor}.

get_rand_channel() ->
    ChID = random:uniform(?OTHERCHANNELNUM),
    ChNameList = lists:concat([ch, ChID]),
    ChName = ic_pragma:list_to_term(ChNameList),
    ChName.


server_debug(Str, Args) ->
    FlagList = ets:lookup(t_debug_flag, debug),
    case FlagList of
        [] ->
            skip;
        [Flag] ->
            case Flag of
                true ->
                    ?SERVERDEBUG(Str, Args);
                false ->
                    skip
            end
    end.