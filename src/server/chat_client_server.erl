%%%-------------------------------------------------------------------
%%% @author huangyao
%%% @copyright (C) 2018, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 15. 八月 2018 15:29
%%%-------------------------------------------------------------------
-module(chat_client_server).

-author("huangyao").

-behaviour(gen_server).

-include("chat_room.hrl").
-include("protocol.hrl").

-define(UPLOADTIME, 30).

%% API
-export([
    start/1,
    command/2
]).

%% gen_server callbacks
-export([init/1,
    handle_call/3,
    handle_cast/2,
    handle_info/2,
    terminate/2,
    code_change/3]).

-define(SERVER, ?MODULE).

-record(state, {socket, usename, userpos}).  %% 可以考虑放到进程字典当中

%%%===================================================================
%%% API
%%%===================================================================
start(UserName) ->
    start_link(UserName).

start_link(UserName) ->  %% 启动的时候需要输入名字进行登录以及注册进程
    gen_server:start_link({local, UserName}, ?MODULE, [UserName], []).

command(UserName, Cmd) ->  %% 需要改成gen_server:handle_info处理？直接使用Pid !
    case Cmd of

        {private_chat, ReceiverName, Msg} ->
            UserName ! {private_chat, ReceiverName, Msg};
        {channel_chat, Msg} ->
            UserName ! {channel_chat, Msg};
        {world_chat, Msg} ->
            UserName ! {world_chat, Msg};
        {near_chat, Msg} ->
            UserName ! {near_chat, Msg};

        {join_channel, ChannelID} ->
            UserName ! {join_channel, ChannelID};
        {leave_channel, ChannelID} ->
            UserName ! {leave_channel, ChannelID};
        {change_current_channel, ChannelID} ->
            UserName ! {change_current_channel, ChannelID};

        {get_channel_user_info} ->  %% 这里只给出名字和位置
            UserName ! {get_channel_user_info};
        {get_self_log_info} ->
            UserName ! {get_self_log_info};
        {get_self_info} ->
            UserName ! {get_self_info};

        {random_move} ->
            UserName ! {random_move};

        {upload_user_log} ->
            UserName ! {upload_user_log};

        {set_client_debug, DebugFlag} ->
            UserName ! {set_client_debug, DebugFlag};

        {set_client_log_info_debug, DebugFlag} ->
            UserName ! {set_client_log_info_debug, DebugFlag};

        {set_server_debug, DebugFlag} ->
            UserName ! {set_server_debug, DebugFlag};

        {stop} ->
            UserName ! {stop};

        _ ->
            ?ERRORLOG("Error command!~n")
    end.

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================
init([UserName]) ->
    do_init(UserName),
    Host = "localhost",  %% 这里host使用默认的localhost
    {ok, Socket} = gen_tcp:connect(Host, 1234, [binary, {packet, 4}]),
    EncodeBin = protocol:encode(?PROTO_LOGIN_CS, #r_login_cs{user_name = UserName}),
    client_log(#r_user_to_log{name = UserName, in_out = ?OUT, bin = EncodeBin}),
    gen_tcp:send(Socket, EncodeBin),
    {ok, #state{socket = Socket, usename = UserName}}.

handle_call(_Request, _From, State) ->
    {reply, ok, State}.

handle_cast(_Request, State) ->
    {noreply, State}.

handle_info({tcp, _Socket, Bin}, State) ->
    ?DEBUG("Receive from Server~n"),
    ?DEBUG("Bin:~w", [Bin]),
    [ProtNum, MsgTuple] = protocol:decode(Bin),
    #state{socket = Socket, usename = UserName} = State,
    ?DEBUG("protnum:~w~n", [ProtNum]),
    case ProtNum of
        ?PROTO_LOGIN_SC -> %% 登录
            {ReplyStr} = MsgTuple,
            client_log(#r_user_to_log{name = UserName, in_out = ?IN, bin = Bin, msg = ReplyStr}),
           ?DEBUG("Login return: ~w~n", [ReplyStr]);

        ?PROTO_PRIVATE_CHAT_SC ->  %% 私聊
            {ChatType, SenderName, SenderPos, ReceiverPos, Msg} = MsgTuple,
            case ChatType of
                pc_return ->
                    client_log(#r_user_to_log{name = UserName, in_out = ?IN, bin = Bin,
                        msg = Msg, type = ?WITHMSG}),
                    client_debug("Private chat return: ~p~n", [Msg]);
                _ ->  %% type: private_chat channel_chat near_chat world_chat
                    client_debug("[~-5w,pos:~-7w]| ~-12w| From: [~-5w,pos:~-7w]| Msg: ~p|~n",
                        [UserName, ReceiverPos, ChatType, SenderName, SenderPos, Msg]),
                    ChatGetStr = io_lib:format("[~w, pos:~w] ~w [From: [~w, pos:~w], Msg:~p]) ",
                        [UserName, ReceiverPos, ChatType, SenderName, SenderPos, Msg]),
                    Str = lists:flatten(ChatGetStr),
                    ?DEBUG("output str:~w~n", [Str]),
                    SendBin = protocol:encode(?PROTO_CGM_CS, #r_cgm_cs{msg = Str}),  %% 接收到消息发给通信进程
                    client_log(#r_user_to_log{name = UserName, in_out = ?IN, bin = Bin,
                        msg = Msg, type = ?WITHMSG}),
                    client_log(#r_user_to_log{name = UserName, in_out = ?OUT, bin = SendBin,
                        msg = Str, type = ?WITHMSG}),
                    gen_tcp:send(Socket, SendBin)
            end;

        ?PROTO_CHANNEL_CHAT_SC ->
            {Msg} = MsgTuple,
            client_log(#r_user_to_log{name = UserName, in_out = ?IN, bin = Bin,
                msg = Msg, type = ?WITHMSG}),
            client_debug("Channel chat return: ~p~n", [Msg]);

        ?PROTO_WORLD_CHAT_SC ->
            {Msg} = MsgTuple,
            client_log(#r_user_to_log{name = UserName, in_out = ?IN, bin = Bin,
                msg = Msg, type = ?WITHMSG}),
            client_debug("World chat return: ~p~n", [Msg]);

        ?PROTO_NEAR_CHAT_SC ->
            {Msg} = MsgTuple,
            client_log(#r_user_to_log{name = UserName, in_out = ?IN, bin = Bin,
                msg = Msg, type = ?WITHMSG}),
            client_debug("Near chat return: ~p~n", [Msg]);

        ?PROTO_JOIN_CHANNEL_SC ->
            {Msg} = MsgTuple,
            client_log(#r_user_to_log{name = UserName, in_out = ?IN, bin = Bin,
                msg = Msg, type = ?WITHMSG}),
            client_debug("Join channel return: ~p~n", [Msg]);

        ?PROTO_LEAVE_CHANNEL_SC ->
            {Msg} = MsgTuple,
            client_log(#r_user_to_log{name = UserName, in_out = ?IN, bin = Bin,
                msg = Msg, type = ?WITHMSG}),
            client_debug("Leave channel return: ~p~n", [Msg]);

        ?PROTO_CCC_SC ->
            {Msg} = MsgTuple,
            client_log(#r_user_to_log{name = UserName, in_out = ?IN, bin = Bin,
                msg = Msg, type = ?WITHMSG}),
            client_debug("Change current channel return: ~p~n", [Msg]);

        ?PROTO_SCUI_SC ->  %% 这条指令不参与测试，因此不记录
            {UserNum, ChannelUserInfoList} = MsgTuple,
%%            client_log(#r_user_to_log{name = UserName, in_out = ?IN, bin = Bin, type = ?WITHMSG})
            client_debug("Current Channel User Number:~p, List:~w~n",
                [UserNum, ChannelUserInfoList]);

        ?PROTO_GET_SELF_INFO_SC ->
            {RUser} = MsgTuple,
            client_debug("User Info:~p~n", [RUser]);

        ?PROTO_RANDOM_MOVE_SC ->
            {Msg} = MsgTuple,
            client_log(#r_user_to_log{name = UserName, in_out = ?IN, bin = Bin, msg = Msg}),
            client_debug("Random Move return: ~p~n", [Msg]);

        _ ->
            ?ERRORLOG("Error message")
    end,
    {noreply, State};

handle_info({tcp_closed, _Socket}, State) ->
    client_debug("Server closed!", []),
    {noreply, State};

handle_info(MsgTuple, State) ->
    #state{socket = Socket, usename = SenderName} = State,
    case MsgTuple of
        {private_chat, ReceiverName, Msg} ->
            Bin = protocol:encode(?PROTO_PRIVATE_CHAT_CS,
                #r_private_chat_cs{receiver_name = ReceiverName, msg = Msg}),
            gen_tcp:send(Socket, Bin),
            client_log(#r_user_to_log{name = SenderName, in_out = ?OUT, bin = Bin,
                msg = Msg, type = ?WITHMSG}),
            {noreply, State};

        {channel_chat, Msg} ->
            Bin = protocol:encode(?PROTO_CHANNEL_CHAT_CS, #r_channel_chat_cs{msg = Msg}),
            gen_tcp:send(Socket, Bin),
            client_log(#r_user_to_log{name = SenderName, in_out = ?OUT, bin = Bin,
                msg = Msg, type = ?WITHMSG}),
            {noreply, State};

        {world_chat, Msg} ->
            Bin = protocol:encode(?PROTO_WORLD_CHAT_CS, #r_world_chat_cs{msg = Msg}),
            gen_tcp:send(Socket, Bin),
            client_log(#r_user_to_log{name = SenderName, in_out = ?OUT, bin = Bin,
                msg = Msg, type = ?WITHMSG}),
            {noreply, State};

        {near_chat, Msg} ->
            Bin = protocol:encode(?PROTO_NEAR_CHAT_CS, #r_near_chat_cs{msg = Msg}),
            gen_tcp:send(Socket, Bin),
            client_log(#r_user_to_log{name = SenderName, in_out = ?OUT, bin = Bin,
                msg = Msg, type = ?WITHMSG}),
            {noreply, State};

        {join_channel, ChannelID} ->
            Bin = protocol:encode(?PROTO_JOIN_CHANNEL_CS,
                #r_join_channel_cs{channel_id = ChannelID}),
            gen_tcp:send(Socket, Bin),
            client_log(#r_user_to_log{name = SenderName, in_out = ?OUT, bin = Bin}),
            {noreply, State};

        {leave_channel, ChannelID} ->
            Bin = protocol:encode(?PROTO_LEAVE_CHANNEL_CS,
                #r_leave_channel_cs{channel_id = ChannelID}),
            gen_tcp:send(Socket, Bin),
            client_log(#r_user_to_log{name = SenderName, in_out = ?OUT, bin = Bin}),
            {noreply, State};

        {change_current_channel, ChannelID} ->
            Bin = protocol:encode(?PROTO_CCC_CS, #r_ccc_cs{channel_id = ChannelID}),
            gen_tcp:send(Socket, Bin),
            client_log(#r_user_to_log{name = SenderName, in_out = ?OUT, bin = Bin}),
            {noreply, State};

        {get_channel_user_info} ->
            Bin = protocol:encode(?PROTO_SCUI_CS, #r_scui_cs{}),
            gen_tcp:send(Socket, Bin),
%%            client_log(#r_user_to_log{name = SenderName, in_out = ?OUT, bin = Bin}),
            {noreply, State};

        {get_self_info} ->
            Bin = protocol:encode(?PROTO_GET_SELF_INFO_CS, #r_get_self_info_cs{}),
            gen_tcp:send(Socket, Bin),
            {noreply, State};

        {get_self_log_info} ->
            RUserLogInfo = erlang:get(r_user_log_info),
            chat_room_log_server:show_user_log_info(RUserLogInfo),
            {noreply, State};

        {random_move} ->
            Bin = protocol:encode(?PROTO_RANDOM_MOVE_CS, #r_random_move_cs{}),
            gen_tcp:send(Socket, Bin),
            client_log(#r_user_to_log{name = SenderName, in_out = ?OUT, bin = Bin}),
            {noreply, State};

        {upload_user_log} ->
            upload_user_log(),
            {noreply, State};

        {set_client_debug, DebugFlag} ->
            erlang:put(debug, DebugFlag),
            {noreply, State};

        {set_client_log_info_debug, DebugFlag} ->
            erlang:put(log_info_debug, DebugFlag),
            {noreply, State};

        {set_server_debug, DebugFlag} ->
            ets:insert(?TDEBUGFLAT, {debug, DebugFlag}),
            {noreply, State};

        {logout} ->
%%            gen_tcp:send(Socket, {tcp_closed, })
            {noreply, State};

        {stop} ->
            upload_user_log(),
            {stop, normal, State};

        {'EXIT', _From, Reason} ->
            upload_user_log(),
            {stop, Reason, State};

        _ ->
            {noreply, State}
    end.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
do_init(UserName) ->
    BeginLogTime = chat_room_log_server:get_time_millisec(),
    erlang:put(r_user_log_info,
        #r_user_log_info{user_name = UserName, begin_log_time = BeginLogTime}).
%%    start_upload_log_info_timer(?UPLOADTIME).

client_debug(Msg, Args) ->
    case erlang:get(debug) of
        true ->
            ?CLIENTDEBUG(Msg, Args);
        _ ->
            fasle
    end .

client_log(UserLog) ->
    case ?CLIENTNEEDLOG of
        false ->
            skip;
        true ->
            OldRUserLogInfo = erlang:get(r_user_log_info),
            NewRUserLogInfo = chat_room_log_server:user_log_to_self(OldRUserLogInfo, UserLog),
            erlang:put(r_user_log_info, NewRUserLogInfo)
    end.

%%start_upload_log_info_timer(TimeGap) ->
%%    receive
%%        after TimeGap * 1000 ->
%%            upload_user_log()
%%    end.

upload_user_log() ->
    RUserLogInfo = erlang:get(r_user_log_info),
    chat_room_log_server:user_upload_log(RUserLogInfo),
    ok.