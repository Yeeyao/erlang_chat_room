%%%-------------------------------------------------------------------
%%% @author huangyao
%%% @copyright (C) 2018, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 21. 八月 2018 15:13
%%%-------------------------------------------------------------------
-module(chat_connect_client_server).

-author("huangyao").

-behaviour(gen_server).

-include("chat_room.hrl").

-include("protocol.hrl").

%% API
-export([
    start_link/1
]).

%% gen_server callbacks
-export([init/1,
    handle_call/3,
    handle_cast/2,
    handle_info/2,
    terminate/2,
    code_change/3]).

-define(SERVER, ?MODULE).

-record(state, {user_info = #r_user{}}).

%%%===================================================================
%%% API
%%%===================================================================
start_link(UserInfo) ->
    #r_user{name = UserName} = UserInfo,
    ConnectClientNameList = lists:concat([UserName, connect_client]),
    ConnectClientName = ic_pragma:list_to_term(ConnectClientNameList),
    gen_server:start_link({local, ConnectClientName}, ?MODULE, [UserInfo], []).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================
init([UserInfo]) ->
    save_user_to_table(UserInfo),
    save_user_to_channel(UserInfo),  %% 当前和世界频道
    {ok, #state{user_info = UserInfo}}.

handle_call(_Request, _From, State) ->
    {reply, ok, State}.

handle_cast(_Request, State) ->
    {noreply, State}.

handle_info({tcp, _Socket, Bin}, State) ->
    #state{user_info = UserInfo} = State,
    [ProtNum, MsgTuple] = protocol:decode(Bin),
    #r_user{current_chid = CurrentChannelID, name = _UserName} = UserInfo,
    case ProtNum of
        ?PROTO_PRIVATE_CHAT_CS ->  %% 私聊
            {ReceiverName, Msg} = MsgTuple,
            private_chat(UserInfo, ReceiverName, Msg),
            {noreply, State};

        ?PROTO_CHANNEL_CHAT_CS ->  %% 频道聊天
            {Msg} = MsgTuple,
            channel_chat(UserInfo, CurrentChannelID, Msg),
            {noreply, State};

        ?PROTO_WORLD_CHAT_CS ->  %% 世界聊天
            {Msg} = MsgTuple,
            world_chat(UserInfo, ?WORLDCHANNELID, Msg),
            {noreply, State};

        ?PROTO_NEAR_CHAT_CS ->  %% 周围聊天
            {Msg} = MsgTuple,
            near_chat(UserInfo, Msg),
            {noreply, State};

        ?PROTO_JOIN_CHANNEL_CS ->  %% 加入频道
            {ChannelID} = MsgTuple,
            NewUserInfo = join_channel(UserInfo, ChannelID),
            save_user_to_table(NewUserInfo),
            save_user_to_channel(NewUserInfo),
            NewState = State#state{user_info = NewUserInfo},
            {noreply, NewState};

        ?PROTO_LEAVE_CHANNEL_CS ->  %% 离开频道
            {ChannelID} = MsgTuple,
            NewUserInfo = leave_channel(UserInfo, ChannelID),
            save_user_to_table(NewUserInfo),
            save_user_to_channel(NewUserInfo),
            NewState = State#state{user_info = NewUserInfo},
            {noreply, NewState};

        ?PROTO_CCC_CS ->  %% 改变当前频道
            {ChannelID} = MsgTuple,
            NewUserInfo = change_current_channel(UserInfo, ChannelID),
            save_user_to_table(NewUserInfo),
            save_user_to_channel(NewUserInfo),
            NewState = State#state{user_info = NewUserInfo},
            {noreply, NewState};

        ?PROTO_SCUI_CS ->  %% 获得当前频道玩家信息
            send_channel_user_info(UserInfo),
            {noreply, State};

        ?PROTO_GET_SELF_INFO_CS ->
            send_self_info(UserInfo),
            {noreply, State};

        ?PROTO_CGM_CS ->  %% 玩家得到的消息
            {Msg} = MsgTuple,
            channel_debug("~w~n", [Msg]),
            {noreply, State};

        ?PROTO_RANDOM_MOVE_CS ->
            NewUserInfo = random_move(UserInfo),
            save_user_to_table(NewUserInfo),
            save_user_to_channel(NewUserInfo),
            NewState = State#state{user_info = NewUserInfo},
            {noreply, NewState};

        _ ->
            ?ERRORLOG("chat_room_server protcol decode error~n"),
            {noreply, State}
    end;

handle_info({tcp_closed, _Socket}, State) ->
    #state{user_info = UserInfo} = State,
    player_logout(UserInfo),
    {noreply, State};

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
private_chat(SenderInfo, ReceiverName, Msg) ->
    #r_user{name = SenderName, socket = SenderSocket} = SenderInfo,
    case ets:lookup(?TALLUSERINFO, ReceiverName) of
        [] ->%% 找不到接收消息的用户
            ReplyMsg = "Destination user is not online or not exist";
        [ReceiverInfo] ->  %% 找到了接收消息的用户
            private_chat_send(SenderInfo, ReceiverInfo, Msg),
            channel_debug("[~w]: private_chat(To: ~w, Msg: ~w)~n",
                [SenderName, ReceiverName, Msg]),
            ReplyMsg = "ok"
    end,
    private_chat_send_to_self(ReplyMsg, SenderSocket).

private_chat_send(SenderInfo, ReceiverInfo, Msg) ->
    #r_user{name = SenderName, pos = SenderPos} = SenderInfo,
    #r_user{socket = ReceiverSocket, pos = ReceiverPos} = ReceiverInfo,
    SendMsg = protocol:encode(?PROTO_PRIVATE_CHAT_SC,
        #r_private_chat_sc{chat_type = private_chat, sender_name = SenderName,
            sender_pos = SenderPos, receiver_pos = ReceiverPos, msg = Msg}),
    ?DEBUG("pc sent to dest bin: ~w~n", [SendMsg]),
    gen_tcp:send(ReceiverSocket, SendMsg). %% 发送给对方

private_chat_send_to_self(ReplyMsg, SenderSocket) ->
    SendToSelfMsg = protocol:encode(?PROTO_PRIVATE_CHAT_SC,
        #r_private_chat_sc{chat_type = pc_return, msg = ReplyMsg}),
    gen_tcp:send(SenderSocket, SendToSelfMsg).

channel_chat(UserInfo, ChannelID, Msg) ->
    #r_user{socket = SenderSocket, channel_list = ChannelList,
        current_chid = CurrentChannelID, name = SenderName} = UserInfo,
    case get_channel(ChannelID) of
        [] ->  %% 频道不存在
            ReplyMsg = "Channel does not exist!";
        [_ChannelInfo] ->
            case lists:member(ChannelID, ChannelList) of
                true ->
                    ChannelID ! {channel_chat, UserInfo, Msg},
                    channel_debug("[~w]: channel_chat(Channel ID: ~w, Msg: ~w)~n",
                        [SenderName, CurrentChannelID, Msg]),
                    ReplyMsg = "ok";
                false ->  %% 玩家不在该频道
                    ReplyMsg = "Sorry, you're not in this channel, try to join it."
            end
    end,
    channel_chat_send_to_self(ReplyMsg, SenderSocket).

channel_chat_send_to_self(ReplyMsg, SenderSocket) ->
    SendToSelfMsg = protocol:encode(?PROTO_CHANNEL_CHAT_SC,
        #r_channel_chat_sc{msg = ReplyMsg}),
    gen_tcp:send(SenderSocket, SendToSelfMsg).

world_chat(UserInfo, _ChannelID, Msg) ->
    #r_user{socket = SenderSocket} = UserInfo,
    ?WORLDCHANNELID ! {world_chat, UserInfo, Msg},
    ReplyMsg = "ok",
    SendToSelfMsg = protocol:encode(?PROTO_WORLD_CHAT_SC, #r_world_chat_sc{msg = ReplyMsg}),
    gen_tcp:send(SenderSocket, SendToSelfMsg).

near_chat(UserInfo, Msg) ->  %% 发送给当前频道的所有附近的人
    #r_user{socket = SenderSocket, current_chid = CurrentChannelID,
        name = SenderName, pos = SenderPos} = UserInfo,
    CurrentChannelID ! {near_chat, UserInfo, Msg},
    channel_debug("[~w, pos:~w]: near_chat(Channel ID: ~w, Msg: ~w)~n",
        [SenderName, SenderPos, CurrentChannelID, Msg]),
    ReplyMsg = "ok",
    SendToSelfMsg = protocol:encode(?PROTO_NEAR_CHAT_SC, #r_near_chat_sc{msg = ReplyMsg}),
    gen_tcp:send(SenderSocket, SendToSelfMsg).

join_channel(UserInfo, ChannelID) ->
    #r_user{socket = SenderSocket, channel_list = ChannelList, name = SenderName} = UserInfo,
    case lists:member(ChannelID, ChannelList) of
        true ->
            ReplyMsg = "You've already joint this channel.",
            NewUserInfo = UserInfo;
        false ->
            ?DEBUG("join channel ChannelID:~w~n", [ChannelID]),
            case get_channel(ChannelID) of
                [] ->
                    ReplyMsg = "Channel does not exist!",
                    NewUserInfo = UserInfo;
                [_ChannelInfo] ->
                    channel_debug("[~w]: join channel(Channel ID: ~w)~n", [SenderName, ChannelID]),
                    ReplyMsg = "ok",
                    NewUserInfo = UserInfo#r_user{channel_list = [ChannelID | ChannelList]}
            end
    end,
    join_channel_send_to_self(ReplyMsg, SenderSocket),
    NewUserInfo.

join_channel_send_to_self(ReplyMsg, SenderSocket) ->
    SendToSelfMsg = protocol:encode(?PROTO_JOIN_CHANNEL_SC,
        #r_join_channel_sc{msg = ReplyMsg}),
    gen_tcp:send(SenderSocket, SendToSelfMsg).

leave_channel(UserInfo, ChannelID) ->
    #r_user{socket = SenderSocket, channel_list = ChannelList} = UserInfo,
    case lists:member(ChannelID, ChannelList) of
        true ->
            {ReplyMsg, NewUserInfo} = leave_channel_two(UserInfo, ChannelID);
        false ->
            ReplyMsg = "You are not in this channel.",
            NewUserInfo = UserInfo
    end,
    leave_channel_send_to_self(ReplyMsg, SenderSocket),
    NewUserInfo.

leave_channel_two(UserInfo, ChannelID) ->
    #r_user{current_chid = CurrentChannelID, channel_list = ChannelList,
        name = SenderName} = UserInfo,
    case get_channel(ChannelID) of
        [] ->
            ReplyMsg = "Channel does not exist!",
            NewUserInfo = UserInfo;
        [ChannelInfo] ->
            case ChannelID =:= ?WORLDCHANNELID of
                true ->  %% 世界频道不可以删除
                    ReplyMsg = "you can't leave world channel ",
                    NewUserInfo = UserInfo;
                false ->
                    #r_channel{channel_pid = ChannelPid} = ChannelInfo,
                    ChannelPid ! {delete_player, UserInfo},
                    NewChannelList1 = lists:delete(ChannelID, ChannelList),
                    channel_debug("[~w]: leave channel(Channel ID: ~w)~n",
                        [SenderName, ChannelID]),
                    ReplyMsg = "ok",
                    {NewCurChannelID, NewChannelList} =
                        set_new_curch(ChannelID, CurrentChannelID, NewChannelList1),
                    NewUserInfo = UserInfo#r_user{current_chid = NewCurChannelID,
                        channel_list = NewChannelList}
            end
    end,
    {ReplyMsg, NewUserInfo}.

set_new_curch(ChannelID, CurrentChannelID, NewChannelList1) ->
    case ChannelID =:= CurrentChannelID of
        true ->  %% 删除了当前的频道
            set_new_curch(NewChannelList1);
        false ->
            {CurrentChannelID, NewChannelList1}
    end.

set_new_curch(NewChannelList1) ->  %% new current channel
    if
        erlang:length(NewChannelList1) < 2 ->
            NewChannelID = chat_room_protocol_server:get_rand_channel(),
            NewChannelList = [NewChannelID | NewChannelList1];
        true ->
            NewChannelList = NewChannelList1
    end,
    [NewCurChannelID | _T] = NewChannelList,
    {NewCurChannelID, NewChannelList}.

leave_channel_send_to_self(ReplyMsg, SenderSocket) ->
    SendToSelfMsg = protocol:encode(?PROTO_LEAVE_CHANNEL_SC,
        #r_leave_channel_sc{msg = ReplyMsg}),
    gen_tcp:send(SenderSocket, SendToSelfMsg).

change_current_channel(UserInfo, ChannelID) ->
    #r_user{socket = SenderSocket, current_chid = OldCurrentChannelID,
        channel_list = ChannelList} = UserInfo,
    case ChannelID =:= OldCurrentChannelID of
        true ->
            ReplyMsg = "You're already in this channel",
            NewUserInfo = UserInfo;
        false ->
            case lists:member(ChannelID, ChannelList) of
                true ->
                    {ReplyMsg, NewUserInfo} = change_current_channel_two(UserInfo, ChannelID);
                false ->  %% 玩家不在该频道
                    ReplyMsg = "Sorry, you're not in this channel, try to join it.",
                    NewUserInfo = UserInfo
            end
    end,
    ccc_send_to_self(ReplyMsg, SenderSocket),
    NewUserInfo.

change_current_channel_two(UserInfo, ChannelID) ->
    #r_user{current_chid = OldCurrentChannelID,
        name = SenderName} = UserInfo,
    ChannelInfoList = ets:lookup(?TSERVERCHANNEL, ChannelID),
    case ChannelInfoList of
        [] ->
            ReplyMsg = "Channel does not exist!",
            NewUserInfo = UserInfo;
        [ChannelInfo] ->
            #r_channel{channel_pid = _ChannelPid} = ChannelInfo,
            save_user_to_one_channel(UserInfo, ChannelID),  %% 将玩家信息保存到新的频道
            OldChannelInfoList = ets:lookup(?TSERVERCHANNEL, OldCurrentChannelID),
            case OldChannelInfoList of
                [] ->
                    skip;
                [_OldCurrentChannelInfo] ->
                    delete_user_from_one_channel(UserInfo, OldCurrentChannelID)
            end,
            channel_debug("[~w]: change current channel(old one: ~w, new one: ~w)~n",
                [SenderName, OldCurrentChannelID, ChannelID]),
            ReplyMsg = "ok",
            NewUserInfo = UserInfo#r_user{current_chid = ChannelID}
    end,
    {ReplyMsg, NewUserInfo}.

ccc_send_to_self(ReplyMsg, SenderSocket) ->
    SendToSelfMsg = protocol:encode(?PROTO_CCC_SC, #r_ccc_sc{msg = ReplyMsg}),
    gen_tcp:send(SenderSocket, SendToSelfMsg).

get_channel(ChannelID) ->
    ets:lookup(?TSERVERCHANNEL, ChannelID).

player_logout(UserInfo) ->
    #r_user{current_chid = CurrentChannelID} = UserInfo,
    ChannelInfoList = ets:lookup(?TSERVERCHANNEL, CurrentChannelID),
    case ChannelInfoList of
        [] ->
            skip;
        ChannelInfoList ->
            [ChannelInfo] = ChannelInfoList,
            #r_channel{channel_pid = ChannelPid} = ChannelInfo,
            ChannelPid ! {delete_player, UserInfo}
    end.

save_user_to_table(UserInfo) ->
    ?USERINFOSERVER ! {save_user_info_to_table, UserInfo}.

save_user_to_channel(UserInfo) ->
    #r_user{current_chid = CurrentChannelID, name = UserName, socket = UserSocket,
        pos = UserPos} = UserInfo,
    RUserInfoCh = #r_user_channel{name = UserName, socket = UserSocket, pos = UserPos},
    [ChannelInfo] = ets:lookup(?TSERVERCHANNEL, CurrentChannelID),
    #r_channel{channel_pid = ChannelPid} = ChannelInfo,
    ChannelPid ! {save_player, RUserInfoCh},
    RUserInfoWorld = #r_user_world{name = UserName, socket = UserSocket},
    ?WORLDCHANNELID ! {save_player_wc, RUserInfoWorld}.

save_user_to_one_channel(UserInfo, ChannelID) ->  %% 这里在调用前就检查了channel是存在的
    #r_user{name = UserName, socket = UserSocket, pos = UserPos} = UserInfo,
    RUserInfoCh = #r_user_channel{name = UserName, socket = UserSocket, pos = UserPos},
    ChannelID ! {save_player, RUserInfoCh}.

delete_user_from_one_channel(UserInfo, ChannelID) ->
    case ChannelID of
        ?WORLDCHANNELID ->  %% 世界频道下的玩家信息不删除
            skip;
        _ ->
            ChannelID ! {delete_player, UserInfo}
    end.

send_channel_user_info(UserInfo) ->
    #r_user{current_chid = CurrentChannelID, socket = SenderSocket} = UserInfo,
    AllUserInfoTable = ets:tab2list(?TALLUSERINFO),
    ChannelUserInfoList = lists:foldl(
        fun(RUser, Tab) ->
            #r_user{current_chid = CurChID, name = Name, pos = Pos} = RUser,
            if
                CurChID == CurrentChannelID ->
                    [{Name, Pos} | Tab];
                true ->
                    Tab
            end
        end, [], AllUserInfoTable),
    UserNum = erlang:length(ChannelUserInfoList),
    Bin = protocol:encode(?PROTO_SCUI_SC, #r_scui_sc{user_num = UserNum,
        user_info_list = ChannelUserInfoList}),
    gen_tcp:send(SenderSocket, Bin).

send_self_info(UserInfo) ->
    #r_user{socket = SenderSocket} = UserInfo,
    Bin = protocol:encode(?PROTO_GET_SELF_INFO_SC, #r_get_self_info_sc{r_user = UserInfo}),
    gen_tcp:send(SenderSocket, Bin).

random_move(UserInfo) ->
    #r_user{current_chid = CurrentChannelID, socket = SenderSocket} = UserInfo,
    NewPos = get_rand_pos(),
    NewUserInfo = UserInfo#r_user{pos = NewPos},
    save_user_to_one_channel(NewUserInfo, CurrentChannelID),
    ReplyMsg = "ok",
    SendToSelfMsg = protocol:encode(?PROTO_RANDOM_MOVE_SC, #r_random_move_sc{msg = ReplyMsg}),
    gen_tcp:send(SenderSocket, SendToSelfMsg),
    NewUserInfo.

get_rand_pos() -> %% 这里两个用户位置重叠不做处理
    random:seed(erlang:now()),
    HorCoor = random:uniform(?HORMAX) - 1 + ?HORMIN,
    VerCoor = random:uniform(?VERMAX) - 1 + ?VERMIN,
    {HorCoor, VerCoor}.

channel_debug(Str, Args) ->
    FlagList = ets:lookup(?TDEBUGFLAT, debug),
    case FlagList of
        [] ->
            skip;
        [Flag] ->
            case Flag of
                true ->
                    ?CHANNELDEBUG(Str, Args);
                false ->
                    skip
            end
    end.