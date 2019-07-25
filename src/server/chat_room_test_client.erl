%%%-------------------------------------------------------------------
%%% @author huangyao
%%% @copyright (C) 2018, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 28. 八月 2018 16:59
%%%-------------------------------------------------------------------
-module(chat_room_test_client).

-author("huangyao").

-behaviour(gen_server).

-include("chat_room.hrl").

-define(TYPENUM, 8).

-define(UPLOADTIME, 30000).

%% API
-export([
    start/0,
    start_link/0,
    client_login/2,
    client_start_op/3,
    do_op/1,
    client_stop_op/0,
    client_logout/1,
    client_logout/2,
    upload_log_info/2
]).

%% gen_server callbacks
-export([init/1,
    handle_call/3,
    handle_cast/2,
    handle_info/2,
    terminate/2,
    code_change/3]).

-define(SERVER, ?MODULE).

-define(USERLEN, 1000).

-record(state, {client_op_tref, upload_op_tref, msg_list, msg_list_len,
    op_pid_list, op_pid_len}).

%%%===================================================================
%%% API
%%%===================================================================
start() ->
    start_link().

start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

client_login(Begin, End) ->
    ?MODULE ! {client_login, Begin, End}.

client_start_op(Begin, End, TimeGap) ->
    ?MODULE ! {client_start_op, Begin, End, TimeGap}.

client_stop_op() ->
   ?MODULE ! {client_stop_op}.

client_logout(UserName) ->
    ?MODULE ! {client_logout, UserName}.

client_logout(Begin, End) ->
    ?MODULE ! {client_logout, Begin, End}.

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================
init([]) ->
    chat_room_log_server:start(),
    Dir = "priv/msg.txt",  %%随机消息
    {ok, MsgList} = file:consult(Dir),  %% [{No, "msg..."}, ...]
    MsgListLen = erlang:length(MsgList),
    {ok, #state{msg_list = MsgList, msg_list_len = MsgListLen}}.

handle_call(_Request, _From, State) ->
    {reply, ok, State}.

handle_cast(_Request, State) ->
    {noreply, State}.

handle_info(Info, State) ->
    case Info of
        {client_login, Begin, End} ->
            s_client_login(Begin, End),
            OpPidList = get_op_pid2(Begin, End),
            OpPidLen = erlang:length(OpPidList),
            NewState = State#state{op_pid_list = OpPidList, op_pid_len = OpPidLen},
            {noreply, NewState};

        {client_start_op, Begin, End, TimeGap} ->
            NewState = s_client_start_op(Begin, End, TimeGap, State),
            {noreply, NewState};

        {client_op, Begin, End} ->
            #state{msg_list = MsgList, msg_list_len = MsgListLen,
                op_pid_list = OpPidList, op_pid_len = OpPidLen} = State,
%%            Seed = gen_server:call(chat_room_log_server, {get_rand_seed}),
%%            random:seed({Seed, Seed, Seed}),
            PidNum = random:uniform(OpPidLen),
            lists:nth(PidNum, OpPidList) ! {do_op, Begin, End, MsgList, MsgListLen},
            {noreply, State};

        {client_stop_op} ->
            NewState = s_client_stop_op(State),
            {noreply, NewState};

        {client_logout, UserName} ->
            chat_client_server:command(UserName, {stop}),
            {noreply, State};

        {client_logout, Begin, End} ->
            s_client_logout(Begin, End),
            {noreply, State};  %% 停止

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
s_client_login(Begin, End) ->
    UserList = lists:seq(Begin, End),
    lists:foreach(
        fun(UserNum) ->
            UserNameList = lists:concat([a, UserNum]),
            UserName = ic_pragma:list_to_term(UserNameList),
            chat_client_server:start(UserName)
        end,
        UserList
    ).

s_client_start_op(Begin, End, TimeGap, State) ->  %% 有必要分裂一个进程来执行随机指令？
    UploadPid = spawn(?MODULE, upload_log_info, [Begin, End]),
    {ok, UploadOptref} = timer:send_interval(?UPLOADTIME, UploadPid, {upload_log_info}),
    {ok, ClientOptref} = timer:send_interval(TimeGap, {client_op, Begin, End}),
    NewState = State#state{client_op_tref = ClientOptref, upload_op_tref = UploadOptref},
    NewState.

s_client_logout(Begin, End) ->
    UserList = lists:seq(Begin, End),
    lists:foreach(
        fun(UserNum) ->
            UserNameList = lists:concat([a, UserNum]),
            UserName = ic_pragma:list_to_term(UserNameList),
            chat_client_server:command(UserName, {stop})
        end,
        UserList
    ).
%%    chat_room_log_server:stop().  %% 这里需要保证程序稳定

s_client_stop_op(State) ->
    #state{client_op_tref = ClientOptref, upload_op_tref = UploadOptref} = State,
    ?ERRORLOG("client stop~n"),
    timer:cancel(ClientOptref),
    timer:cancel(UploadOptref),
    NewState = State#state{client_op_tref = 0, upload_op_tref = 0},
    NewState.

get_op_pid2(Begin, End) ->
    UserList = lists:seq(Begin, End),
    AllUserPidList = lists:foldl(
        fun(SenderNum, Acc) ->
            SenderNameList = lists:concat([a, SenderNum]),
            SenderName = ic_pragma:list_to_term(SenderNameList),
            Pid = spawn(?MODULE, do_op, [SenderName]),
            [Pid | Acc]
    end, [], UserList),
    AllUserPidList.

do_op(SenderName) ->
    receive
        {do_op, Begin, End, MsgList, MsgListLen} ->
            random:seed(erlang:now()),
            do_one_operation(SenderName, Begin, End, MsgList, MsgListLen);
        _ ->
            skip
    end,
    do_op(SenderName).

%%client_command(Begin, End, MsgList, MsgListLen) ->
%%    random:seed(erlang:now()),
%%    SenderNum = random:uniform(End) - 1 + Begin,
%%    SenderNameList = lists:concat([a, SenderNum]),
%%    SenderName = ic_pragma:list_to_term(SenderNameList),
%%    do_one_operation(SenderName, Begin, End, MsgList, MsgListLen).

do_one_operation(UserName, Begin, End, MsgList, MsgListLen) ->
%%    ?ERRORLOG("UserName: ~w, DestName: ~w, Msg:~w~n", [UserName, DestName, Msg]),
%%    ?ERRORLOG("UserName: ~w, DestName: ~w, Msg:~p~n", [UserName, DestName, Msg]),
%%    Seed = gen_server:call(chat_room_log_server, {get_rand_seed}),
%%    random:seed({Seed, Seed, Seed}),
    Type = random:uniform(?TYPENUM),
    case Type of
        1 ->
            DestName = get_dest_user(Begin, End),
            Msg = get_msg(MsgList, MsgListLen),
            chat_client_server:command(UserName, {private_chat, DestName, Msg});
        2 ->
            Msg = get_msg(MsgList, MsgListLen),
            chat_client_server:command(UserName, {channel_chat, Msg});
        3 ->
            Msg = get_msg(MsgList, MsgListLen),
            chat_client_server:command(UserName, {world_chat, Msg});
        4 ->
            Msg = get_msg(MsgList, MsgListLen),
            chat_client_server:command(UserName, {near_chat, Msg});
        5 ->
            ChannelID = get_rand_channel_id(),
            chat_client_server:command(UserName, {join_channel, ChannelID});
        6 ->
            ChannelID = get_rand_channel_id(),
            chat_client_server:command(UserName, {leave_channel, ChannelID});
        7 ->
            ChannelID = get_rand_channel_id(),
            chat_client_server:command(UserName, {change_current_channel, ChannelID});
        8 ->
            chat_client_server:command(UserName, {random_move})
    end.

get_rand_channel_id() ->
%%    Seed = gen_server:call(chat_room_log_server, {get_rand_seed}),
%%    random:seed({Seed, Seed, Seed}),
    ChID = random:uniform(?OTHERCHANNELNUM),
    ChannelID =
        case ChID of
            1 ->
                ch1;
            2 ->
                ch2;
            3 ->
                ch3;
            4 ->
                ch4;
            5 ->
                ch5
        end,
    ChannelID.

get_dest_user(Begin, End) ->
%%    Seed = gen_server:call(chat_room_log_server, {get_rand_seed}),
%%    random:seed({Seed, Seed, Seed}),
    DestNum = random:uniform(End) - 1 + Begin,
    DestNameList = lists:concat([a, DestNum]),
    DestName = ic_pragma:list_to_term(DestNameList),
    DestName.

get_msg(MsgList, MsgListLen) ->
%%    Seed = gen_server:call(chat_room_log_server, {get_rand_seed}),
%%    random:seed({Seed, Seed, Seed}),
    MsgID = random:uniform(MsgListLen),
    {_ID, Msg} = lists:keyfind(MsgID, 1, MsgList),
    Msg.

upload_log_info(Begin, End) ->
    receive
        {upload_log_info} ->
            UserList = lists:seq(Begin, End),
%%            ?ERRORLOG("Enter client upload log info.~n"),
            lists:foreach(
                fun(UserNum) ->
                    UserNameList = lists:concat([a, UserNum]),
                    UserName = ic_pragma:list_to_term(UserNameList),
                    chat_client_server:command(UserName, {upload_user_log})
                end,
                UserList
            );
        _ ->
            ok
    end,
    upload_log_info(Begin, End).