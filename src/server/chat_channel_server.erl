%%%-------------------------------------------------------------------
%%% @author huangyao
%%% @copyright (C) 2018, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 15. 八月 2018 8:41
%%%-------------------------------------------------------------------
-module(chat_channel_server).

-author("huangyao").

-behaviour(gen_server).

-include("chat_room.hrl").
-include("protocol.hrl").

-define(SPLITLISTLEN, 1000).

%% API
-export([
    start/1,
    send_to_user_wc/3
]).

%% gen_server callbacks
-export([init/1,
    handle_call/3,
    handle_cast/2,
    handle_info/2,
    terminate/2,
    code_change/3]).

%%-export([cast/2]).

-define(SERVER, ?MODULE).

-record(state, {channel_id = 0, user_num = 0}).

%%%===================================================================
%%% Export functions
%%%===================================================================
%%cast(create_channel, ChannelID) ->
%%	gen_server:cast(ChannelID, {create_channel, ChannelID}).

%%%===================================================================
%%% API
%%%===================================================================
start(ChannelID) ->
    start_link(ChannelID).

start_link(ChannelID) -> %% 这里频道名字统一为ch0, ch1, ...
    {ok, Pid} = gen_server:start_link({local, ChannelID}, ?MODULE, [ChannelID], []),
    ChannelInfo = #r_channel{id = ChannelID, channel_pid = Pid, user_num = 0},
    chat_room_protocol_server:add_channel(ChannelInfo).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================
init([ChannelID]) ->
    ?DEBUG("Channel ~w start:~n", [ChannelID]),  %% 需要决定主动被动方式
    State = #state{channel_id = ChannelID},  %% 该用进程字典
	{ok, State}.

handle_call(_Request, _From, State) ->
    {reply, ok, State}.

handle_cast(_Request, State) ->
    {noreply, State}.

handle_info(Info, State) ->
    case Info of
        {save_player, RUserInfoCh} ->
            #r_user_channel{name = UserName, pos = Pos} = RUserInfoCh,
            BlockNum = get_block_num(Pos),
            #state{channel_id = ChannelID, user_num = UserNum} = State,
            ?DEBUG("ChannelID:~w get user name:~w~n", [ChannelID, UserName]),
            WithBlockNum = RUserInfoCh#r_user_channel{block_num = BlockNum},
            erlang:put(UserName, WithBlockNum),
            {noreply, State#state{user_num = UserNum + 1}};

        {save_player_wc, RUserInfoWorld} ->
            #r_user_world{name = UserName} = RUserInfoWorld,
            #state{channel_id = ChannelID, user_num = UserNum} = State,
            ?DEBUG("ChannelID:~w get user name:~w~n", [ChannelID, UserName]),
            erlang:put(UserName, RUserInfoWorld),
            {noreply, State#state{user_num = UserNum + 1}};

        {delete_player, UserInfo} ->
            #r_user{name = UserName} = UserInfo,
            #state{channel_id = _ChannelID, user_num = UserNum} = State,
            case erlang:get(UserName) of
                undefined ->
                    NewState = State,
                    skip;
                _ ->
                    NewState = State#state{user_num = UserNum + 1},
                    erlang:erase(UserName)
            end,
            {noreply, NewState};

        {near_chat, SenderInfo, Msg} ->
            #r_user{name = UserName} = SenderInfo,
            case erlang:get(UserName) of
                undefined ->
                    skip;
                SenderChInfo ->
                    UserInfoList = erlang:get(),
                    #r_user_channel{block_num = SenderBlockNum} = SenderChInfo,
                    lists:foreach(near_chat_fun(SenderBlockNum, SenderInfo, Msg), UserInfoList)
            end,
            {noreply, State};

        {channel_chat, SenderInfo, Msg} ->
            UserInfoList = erlang:get(),
            lists:foreach(channel_chat_fun(SenderInfo, Msg), UserInfoList),
            {noreply, State};

        {world_chat, SenderInfo, Msg} ->
            UserInfoList = erlang:get(),
            TotalLen = erlang:length(UserInfoList),
            wc_split(SenderInfo, Msg, UserInfoList, TotalLen),
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
%%is_channel_exist(ChannelID) ->
%%    case ets:lookup(channel_table, ChannelID) of
%%        [] ->
%%            false;
%%        _ ->
%%            true
%%    end.

%%is_near_enough(SenderPos, UserPos) ->
%%    {SenderHorCoor, SenderVerCoor} = SenderPos,
%%	{UserHorCoor, UserVerCoor} = UserPos,
%%	Distance = math:sqrt(math:pow((SenderHorCoor - UserHorCoor), 2) +
%%        math:pow((SenderVerCoor - UserVerCoor), 2)),
%%    Distance =< ?NEARRADIUS.

near_chat_fun(SendBlockNum, SenderInfo, Msg) ->
    fun({_UserName, ReceiverInfo}) ->
        IsUserRecord = is_record(ReceiverInfo, r_user_channel),
        case IsUserRecord of
            true ->
                ?DEBUG("UserInfo:~w~n, SenderInfo:~w~n", [ReceiverInfo, SenderInfo]),
                #r_user_channel{block_num = ReceiverBlockNum} = ReceiverInfo,
                if
                    SendBlockNum =:= ReceiverBlockNum ->
                        send_to_user(near_chat, SenderInfo, ReceiverInfo, Msg);
                    true ->
                        skip
                end;
            false ->
                skip
        end
    end.

channel_chat_fun(SenderInfo, Msg) ->
    fun({_UserName, UserInfo}) ->
        IsUserRecord = is_record(UserInfo, r_user_channel),
        case IsUserRecord of
            true ->  %% 注意SenderInfo是玩家r_user
                send_to_user(channel_chat, SenderInfo, UserInfo, Msg);
            false ->
                skip
        end
    end.

get_block_num(Pos) ->
    {XCoor, YCoor} = Pos,
    XNum = get_num(XCoor),
    YNum = get_num(YCoor),
    BlockNum = XNum + 3 * (YNum - 1),
    BlockNum.

get_num(Coor) ->
    if
        Coor >= 0 andalso Coor < 33 ->
            1;
        Coor >= 33 andalso Coor < 66 ->
            2;
        true ->
            3
    end.

send_to_user(ChatType, SenderInfo, ReceiverInfo, Msg) ->
    #r_user_channel{name = ReceiverName, socket = ReceiverSocket,
        pos = ReceiverPos} = ReceiverInfo,
    #r_user{name = SenderName, socket = _SenderSocket, pos = SenderPos} = SenderInfo,
    if
        ReceiverName /= SenderName ->  %% 不需要发送给自己
            SendMsg = protocol:encode(?PROTO_PRIVATE_CHAT_SC,
                #r_private_chat_sc{chat_type = ChatType,
                    sender_name = SenderName, sender_pos = SenderPos,
                        receiver_pos = ReceiverPos, msg = Msg}),
            gen_tcp:send(ReceiverSocket, SendMsg);
		true ->
			skip
    end.

wc_split(SenderInfo, Msg, UserInfoList, TotalLen) ->
    if
        TotalLen > ?SPLITLISTLEN ->
            {FirThousand, Rest} = lists:split(?SPLITLISTLEN, UserInfoList),
            spawn(?MODULE, send_to_user_wc, [SenderInfo, Msg, FirThousand]),
            wc_split(SenderInfo, Msg, Rest, TotalLen - ?SPLITLISTLEN);
        true ->
            spawn(?MODULE, send_to_user_wc, [SenderInfo, Msg, UserInfoList])
    end.

send_to_user_wc(SenderInfo, Msg, UserInfoList) ->
    lists:foreach(
        fun({_UserName, UserInfo}) ->
            IsUserRecord = is_record(UserInfo, r_user_world),
            case IsUserRecord of
                true ->
                    send_to_user_wc(world_chat, SenderInfo, UserInfo, Msg);
                false ->
                    skip
            end
        end,
        UserInfoList
    ).

send_to_user_wc(ChatType, SenderInfo, ReceiverInfo, Msg) ->
    #r_user_world{name = ReceiverName, socket = ReceiverSocket} = ReceiverInfo,
    #r_user{name = SenderName, socket = _SenderSocket} = SenderInfo,
    if
        ReceiverName /= SenderName ->  %% 不需要发送给自己
            SendMsg = protocol:encode(?PROTO_PRIVATE_CHAT_SC,
                #r_private_chat_sc{chat_type = ChatType, sender_name = SenderName, msg = Msg}),
            gen_tcp:send(ReceiverSocket, SendMsg);
        true ->
            skip
    end.