%%%-------------------------------------------------------------------
%%% @author huangyao
%%% @copyright (C) 2018, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 21. 八月 2018 12:39
%%%-------------------------------------------------------------------
-module(chat_room_log_server).
-author("huangyao").

-behaviour(gen_server).

-include("chat_room.hrl").
-include("protocol.hrl").

%% API
-export([
    start/0,
    stop/0,
    show_current_process_number/0,

    user_log_to_self/2,
    show_user_log_info/1,
    get_time_millisec/0,
    user_upload_log/1,
    %% 信息打印
    send_info/0,
    get_one_user_log_info/1,
    get_all_user_log_info/0
]).

%% gen_server callbacks
-export([init/1,
    handle_call/3,
    handle_cast/2,
    handle_info/2,
    terminate/2,
    code_change/3]).

-define(SERVER, ?MODULE).

-define(B2MB, 1024 * 1024).

-define(PRINTTIME, 20000).
-define(SHOWPNTIME, 20000).

-record(state, {print_tref, spn_tref}).

%%%===================================================================
%%% API
%%%===================================================================
start() ->
    start_link().

stop() ->
    ?MODULE ! {stop}.

start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

user_upload_log(RUserLogInfo) ->
    ?MODULE ! {user_upload_log, RUserLogInfo}.
%%    one_user_log(Name, InOut, Bin, Msg).

get_one_user_log_info(UserName) ->
    ?MODULE ! {get_one_user_info, UserName}.

get_all_user_log_info() ->
    ?MODULE ! {get_all_user_log_info}.

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================
init([]) ->
    BeginTime = get_time_millisec(),
    erlang:put({?MODULE, log_sum_info}, #r_user_log_info{begin_log_time = BeginTime}),
    SPNPid = spawn(?MODULE, show_current_process_number, []),
    AutoPrintPid = spawn(?MODULE, send_info, []),
    {ok, PrintTimerRef} = timer:send_interval(?PRINTTIME, AutoPrintPid, {auto_print}),
    {ok, SPNTimerRef} = timer:send_interval(?SHOWPNTIME, SPNPid, {show_process_num}),
    {ok, #state{print_tref = PrintTimerRef, spn_tref = SPNTimerRef}}.

%%handle_call({get_rand_seed}, _From, State) ->
%%    Reply = random:uniform(100),
%%    {reply, Reply, State};

handle_call(_Request, _From, State) ->
    {reply, ok, State}.

handle_cast(_Request, State) ->
    {noreply, State}.

handle_info(Info, State) ->
    case Info of
        {user_upload_log, RUserLogInfo} ->
            upload_log(RUserLogInfo),
            {noreply, State};

        {get_one_user_info, UserName} ->
            send_one_user_info(UserName),
            {noreply, State};

        {get_all_user_log_info} ->
            send_one_user_info(log_sum_info),
            {noreply, State};

        {stop} ->
            #state{print_tref = OldPrintTimerRef, spn_tref = OldSPNTimerRef} = State,
            timer:cancel(OldPrintTimerRef),
            timer:cancel(OldSPNTimerRef),
            get_all_user_log_info(),  %% 保存总计的流量信息
            NewState = #state{},
            {stop, normal, NewState};

        _Info ->
            {noreply, State}
    end .

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
show_current_process_number() ->
    receive
        {show_process_num} ->
            ProcessList = erlang:processes(),
            ProcessNum = erlang:length(ProcessList),
            ?SERVERDEBUG("Process number: ~w~n", [ProcessNum]);
        _ ->
            ok
    end,
    show_current_process_number().

user_log_to_self(OldRUserLogInfo, #r_user_to_log{in_out = InOut, bin = Bin, msg = Msg, type = Type}) ->
    #r_user_log_info{
        begin_log_time = BeginLogTime, op_times = OpTimes, data_flow_bytes = DataFlowBytes,
        msg_flow_bytes = MsgFlowBytes, msg_op_times = MsgOpTimes,
        other_op_times = OtherOpTimes, data_send_bytes = DataSendBytes,
        data_recv_btyes = DataRecvBytes, msg_send_bytes = MsgSendBytes,
        msg_recv_bytes = MsgRecvBytes
    } = OldRUserLogInfo,
    
    NowTime = get_time_millisec(),
    BinByteLen = erlang:length(binary_to_list(Bin)),
    MsgByteLen = erlang:length(Msg),
    NewOpTimes = OpTimes + 1,
    NewDataFlowBytes = DataFlowBytes + BinByteLen,
    NewMsgFlowBytes = MsgFlowBytes + MsgByteLen,
    NewDataLenAvg = erlang:round(NewDataFlowBytes / NewOpTimes),  %% 这里采用四舍五入
    NewDataFreq = NewOpTimes * 1000 / (NowTime - BeginLogTime) ,  %% 秒的单位

    if
        Type == ?OTHER ->
            NewOtherOpTimes = OtherOpTimes + 1,
            NewMsgOpTimes = MsgOpTimes,
            NewMsgSendBytes = MsgSendBytes,
            NewMsgRecvBytes = MsgRecvBytes;
        true ->
            NewOtherOpTimes = OtherOpTimes,
            NewMsgOpTimes = MsgOpTimes + 1,
            case InOut of
                ?IN ->
                    NewMsgSendBytes = MsgByteLen + MsgSendBytes,
                    NewMsgRecvBytes = MsgRecvBytes;
                ?OUT ->
                    NewMsgSendBytes = MsgSendBytes,
                    NewMsgRecvBytes = MsgByteLen + MsgFlowBytes
            end
    end,
    case InOut of
        ?IN ->
            NewDataSendBytes = DataSendBytes,
            NewDataRecvBytes = BinByteLen + DataRecvBytes;
        ?OUT ->
            NewDataSendBytes = BinByteLen + DataSendBytes,
            NewDataRecvBytes = DataRecvBytes
    end,

    NewMsgLenAvg =
        if
            NewMsgOpTimes == 0 ->
                0;
            true ->
%%                ?ERRORLOG("NewMsgFB:~w, NewMsgOT:~w~n", [NewMsgFlowBytes, NewMsgOpTimes]),
                erlang:round(NewMsgFlowBytes / NewMsgOpTimes)
        end,
    NewMsgFreq = NewMsgOpTimes * 1000 / (NowTime - BeginLogTime),

    NewRLogUserInfo = OldRUserLogInfo#r_user_log_info{
        op_times = NewOpTimes,
        data_flow_bytes = NewDataFlowBytes,
        msg_flow_bytes = NewMsgFlowBytes,
        data_length_average = NewDataLenAvg,
        data_frequency = NewDataFreq,
        msg_length_average = NewMsgLenAvg,
        msg_op_times = NewMsgOpTimes,
        other_op_times = NewOtherOpTimes,
        data_send_bytes = NewDataSendBytes,
        data_recv_btyes = NewDataRecvBytes,
        msg_send_bytes = NewMsgSendBytes,
        msg_recv_bytes = NewMsgRecvBytes,
        msg_frequency = NewMsgFreq
    },
    NewRLogUserInfo.

send_info() ->
    receive
        {auto_print} ->
%%            ?ERRORLOG("Enter send_info~n"),
            chat_room_log_server:get_all_user_log_info(),
            chat_room_log_server:get_one_user_log_info(a0);
        _ ->
        ok
    end,
    send_info().

upload_log(RUserLogInfo) ->
    #r_user_log_info{user_name = UserName} = RUserLogInfo,
    case erlang:get({?MODULE, UserName}) of
        undefined ->
            DiffUserLogInfo = RUserLogInfo;
        OldUserLogInfo ->
                DiffUserLogInfo = get_diff(OldUserLogInfo, RUserLogInfo)
    end,
    erlang:put({?MODULE, UserName}, RUserLogInfo),%% 更新该用户
    OldLogSumInfo = erlang:get({?MODULE, log_sum_info}),
    update_user_log_info(OldLogSumInfo, DiffUserLogInfo).  %% 更新整个系统

get_diff(OldUserLogInfo, RUserLogInfo) ->
    #r_user_log_info{
        op_times = OldUserOpTimes, data_flow_bytes = OldUserDataFlowBytes,
        msg_flow_bytes = OldUserMsgFlowBytes, msg_op_times = OldUserMsgOpTimes,
        other_op_times = OldUserOtherOpTimes, data_send_bytes = OldUserDataSendBytes,
        data_recv_btyes = OldUserDataRecvBytes, msg_send_bytes = OldUserMsgSendBytes,
        msg_recv_bytes = OldUserMsgRecvBytes
    } = OldUserLogInfo,
    #r_user_log_info{
        op_times = UserOpTimes, data_flow_bytes = UserDataFlowBytes,
        msg_flow_bytes = UserMsgFlowBytes, msg_op_times = UserMsgOpTimes,
        other_op_times = UserOtherOpTimes, data_send_bytes = UserDataSendBytes,
        data_recv_btyes = UserDataRecvBytes, msg_send_bytes = UserMsgSendBytes,
        msg_recv_bytes = UserMsgRecvBytes
    } = RUserLogInfo,

    DiffOpTimes = UserOpTimes - OldUserOpTimes,
    DiffDataFlowBytes = UserDataFlowBytes - OldUserDataFlowBytes,
    DiffMsgFlowBytes = UserMsgFlowBytes - OldUserMsgFlowBytes,
    DiffMsgOpTimes = UserMsgOpTimes - OldUserMsgOpTimes,
    DiffOtherOpTimes = UserOtherOpTimes - OldUserOtherOpTimes,
    DiffDataSendBytes = UserDataSendBytes - OldUserDataSendBytes,
    DiffDataRecvBytes = UserDataRecvBytes - OldUserDataRecvBytes,
    DiffMsgSendBytes = UserMsgSendBytes - OldUserMsgSendBytes,
    DiffMsgRecvBytes = UserMsgRecvBytes - OldUserMsgRecvBytes,

    DiffUserLogInfo = #r_user_log_info{
        op_times = DiffOpTimes,
        data_flow_bytes = DiffDataFlowBytes,
        msg_flow_bytes = DiffMsgFlowBytes, msg_op_times = DiffMsgOpTimes,
        other_op_times = DiffOtherOpTimes, data_send_bytes = DiffDataSendBytes,
        data_recv_btyes = DiffDataRecvBytes, msg_send_bytes = DiffMsgSendBytes,
        msg_recv_bytes = DiffMsgRecvBytes
    },
    DiffUserLogInfo.

update_user_log_info(OldLogSumInfo, RUserLogInfo) ->
    NowTime = get_time_millisec(),
    #r_user_log_info{
        begin_log_time = BeginLogTime, op_times = OpTimes, data_flow_bytes = DataFlowBytes,
        msg_flow_bytes = MsgFlowBytes, msg_op_times = MsgOpTimes,
        other_op_times = OtherOpTimes, data_send_bytes = DataSendBytes,
        data_recv_btyes = DataRecvBytes, msg_send_bytes = MsgSendBytes,
        msg_recv_bytes = MsgRecvBytes
    } = OldLogSumInfo,
    #r_user_log_info{
        op_times = UserOpTimes, data_flow_bytes = UserDataFlowBytes,
        msg_flow_bytes = UserMsgFlowBytes, msg_op_times = UserMsgOpTimes,
        other_op_times = UserOtherOpTimes, data_send_bytes = UserDataSendBytes,
        data_recv_btyes = UserDataRecvBytes, msg_send_bytes = UserMsgSendBytes,
        msg_recv_bytes = UserMsgRecvBytes
    } = RUserLogInfo,
    NewOpTimes = OpTimes + UserOpTimes,
    NewMsgOpTimes = MsgOpTimes + UserMsgOpTimes,
    NewOtherOpTimes = OtherOpTimes + UserOtherOpTimes,
    NewDataFlowBytes = DataFlowBytes + UserDataFlowBytes,
    NewDataSendBytes = DataSendBytes + UserDataSendBytes,
    NewDataRecvBytes = DataRecvBytes + UserDataRecvBytes,
    NewMsgFlowBytes = MsgFlowBytes + UserMsgFlowBytes,
    NewMsgSendBytes = MsgSendBytes + UserMsgSendBytes,
    NewMsgRecvBytes = MsgRecvBytes + UserMsgRecvBytes,
    NewDataLenAvg =
        if
            NewOpTimes == 0 ->
                0;
            true ->
                erlang:round(NewDataFlowBytes / NewOpTimes)
        end,
    NewMsgLenAvg =
        if
            NewMsgOpTimes == 0 ->
                0;
            true ->
                erlang:round(NewMsgFlowBytes / NewMsgOpTimes)
        end,
    NewDataFreq = NewOpTimes * 1000 / (NowTime - BeginLogTime) ,  %% 秒的单位
    NewMsgFreq = NewMsgOpTimes * 1000 / (NowTime - BeginLogTime),
    NewRLogUserInfo = OldLogSumInfo#r_user_log_info{
        op_times = NewOpTimes,
        data_flow_bytes = NewDataFlowBytes,
        msg_flow_bytes = NewMsgFlowBytes,
        data_length_average = NewDataLenAvg,
        data_frequency = NewDataFreq,
        msg_length_average = NewMsgLenAvg,
        msg_op_times = NewMsgOpTimes,
        other_op_times = NewOtherOpTimes,
        data_send_bytes = NewDataSendBytes,
        data_recv_btyes = NewDataRecvBytes,
        msg_send_bytes = NewMsgSendBytes,
        msg_recv_bytes = NewMsgRecvBytes,
        msg_frequency = NewMsgFreq
    },
    erlang:put({?MODULE, log_sum_info}, NewRLogUserInfo).

%%get_time_microsec() ->
%%    {_MegaSecs, Secs, MicroSecs} = erlang:now(),
%%    MicroS = Secs * 1000000 + MicroSecs,
%%    MicroS.

%%get_time_sec() ->
%%    {_MegaSecs, Secs, _MicroSecs} = erlang:now(),
%%    Secs.

get_time_millisec() ->
    {_MegaSecs, Secs, MicroSecs} = erlang:now(),
    MicroS = Secs * 1000000 + MicroSecs,
    MilliSec = MicroS / 1000,
%%    ?ERRORLOG("MilliSec:~w~n", [MilliSec]),
    MilliSec.

send_one_user_info(UserName) ->
    Res = erlang:get({?MODULE, UserName}),
    case Res of
        undefined ->
            ?IOLOG("User:~w is not online~n", [UserName]);
    LogUserInfo ->
            #r_user_log_info{
                begin_log_time = BeginLogTime, op_times = OpTimes, data_flow_bytes = DataFlowBytes,
                msg_flow_bytes = MsgFlowBytes, msg_op_times = MsgOpTimes,
                other_op_times = OtherOpTimes, data_send_bytes = DataSendBytes,
                data_recv_btyes = DataRecvBytes, msg_send_bytes = MsgSendBytes,
                msg_recv_bytes = MsgRecvBytes, data_length_average = DataLenAvg,
                data_frequency = DataFreq, msg_length_average = MsgLenAvg, msg_frequency = MsgFreq
            } = LogUserInfo,
        Date = calendar:now_to_local_time(erlang:now()),
        ?IOLOG("UserName:~w~n", [UserName]),
        ?IOLOG("Date: ~w~n", [Date]),
        ?IOLOG("Begin Log Time: ~w~n", [BeginLogTime]),
        ?IOLOG("Op Times: ~w~n", [OpTimes]),
        ?IOLOG("Msg Op Times: ~w~n", [MsgOpTimes]),
        ?IOLOG("Other Op Times: ~w~n", [OtherOpTimes]),
        ?IOLOG("Data Flow Bytes: ~w~n", [DataFlowBytes]),
        ?IOLOG("Data Send Bytes: ~w~n", [DataSendBytes]),
        ?IOLOG("Data Recv Bytes: ~w~n", [DataRecvBytes]),
        ?IOLOG("Data Flow MegaBytes: ~w~n", [DataFlowBytes / ?B2MB]),
        ?IOLOG("Data Send MegaBytes: ~w~n", [DataSendBytes / ?B2MB]),
        ?IOLOG("Data Recv MegaBytes: ~w~n", [DataRecvBytes / ?B2MB]),
        ?IOLOG("Msg Flow Bytes: ~w~n", [MsgFlowBytes]),
        ?IOLOG("Msg Send Bytes: ~w~n", [MsgSendBytes]),
        ?IOLOG("Msg Recv Bytes: ~w~n", [MsgRecvBytes]),
        ?IOLOG("Msg Flow MegaBytes: ~w~n", [MsgFlowBytes  / ?B2MB]),
        ?IOLOG("Msg Send MegaBytes: ~w~n", [MsgSendBytes  / ?B2MB]),
        ?IOLOG("Msg Recv Bytes: ~w~n", [MsgRecvBytes  / ?B2MB]),
        ?IOLOG("Data Length Avg: ~w~n", [DataLenAvg]),
        ?IOLOG("Data Frequency: ~w~n", [DataFreq]),
        ?IOLOG("Msg Length Avg: ~w~n", [MsgLenAvg]),
        ?IOLOG("Msg Frequency: ~w~n", [MsgFreq]),

        InfoList =
        [
            {"UserName:~w~n", [UserName]},
            {"Date: ~w~n", [Date]},
            {"Begin Log Time: ~w~n", [BeginLogTime]},
            {"Op Times: ~w~n", [OpTimes]},
            {"Msg Op Times: ~w~n", [MsgOpTimes]},
            {"Other Op Times: ~w~n", [OtherOpTimes]},
            {"Data Flow Bytes: ~w~n", [DataFlowBytes]},
            {"Data Send Bytes: ~w~n", [DataSendBytes]},
            {"Data Recv Bytes: ~w~n", [DataRecvBytes]},
            {"Data Flow MegaBytes: ~w~n", [DataFlowBytes / ?B2MB]},
            {"Data Send MegaBytes: ~w~n", [DataSendBytes / ?B2MB]},
            {"Data Recv MegaBytes: ~w~n", [DataRecvBytes / ?B2MB]},
            {"Msg Flow Bytes: ~w~n", [MsgFlowBytes]},
            {"Msg Send Bytes: ~w~n", [MsgSendBytes]},
            {"Msg Recv Bytes: ~w~n", [MsgRecvBytes]},
            {"Msg Flow MegaBytes: ~w~n", [MsgFlowBytes  / ?B2MB]},
            {"Msg Send MegaBytes: ~w~n", [MsgSendBytes  / ?B2MB]},
            {"Msg Recv Bytes: ~w~n", [MsgRecvBytes  / ?B2MB]},
            {"Data Length Avg: ~w~n", [DataLenAvg]},
            {"Data Frequency: ~w~n", [DataFreq]},
            {"Msg Length Avg: ~w~n", [MsgLenAvg]},
            {"Msg Frequency: ~w~n", [MsgFreq]},
            {"~n", []}
        ],
        {ok, S} = file:open("log.txt", [append]),
        lists:foreach(fun({Str, Args}) -> io:format(S, Str, Args) end, InfoList),
%%        lists:foreach(fun(X) -> io:format(S, "~p", [X]) end, InfoList),
        file:close(S)
    end.

show_user_log_info(RUserLogInfo) ->
    case erlang:get(log_info_debug) of
        true ->
            #r_user_log_info{
                begin_log_time = BeginLogTime, op_times = OpTimes, data_flow_bytes = DataFlowBytes,
                msg_flow_bytes = MsgFlowBytes, msg_op_times = MsgOpTimes,
                other_op_times = OtherOpTimes, data_send_bytes = DataSendBytes,
                data_recv_btyes = DataRecvBytes, msg_send_bytes = MsgSendBytes,
                msg_recv_bytes = MsgRecvBytes, data_length_average = DataLenAvg,
                data_frequency = DataFreq, msg_length_average = MsgLenAvg, msg_frequency = MsgFreq
            } = RUserLogInfo,
            Date = calendar:now_to_local_time(erlang:now()),
            ?CLIENTDEBUG("Date: ~w~n", [Date]),
            ?CLIENTDEBUG("Begin Log Time: ~w~n", [BeginLogTime]),
            ?CLIENTDEBUG("Op Times: ~w~n", [OpTimes]),
            ?CLIENTDEBUG("Msg Op Times: ~w~n", [MsgOpTimes]),
            ?CLIENTDEBUG("Other Op Times: ~w~n", [OtherOpTimes]),
            ?CLIENTDEBUG("Data Flow Bytes: ~w~n", [DataFlowBytes]),
            ?CLIENTDEBUG("Data Send Bytes: ~w~n", [DataSendBytes]),
            ?CLIENTDEBUG("Data Recv Bytes: ~w~n", [DataRecvBytes]),
            ?CLIENTDEBUG("Data Flow MegaBytes: ~w~n", [DataFlowBytes / ?B2MB]),
            ?CLIENTDEBUG("Data Send MegaBytes: ~w~n", [DataSendBytes / ?B2MB]),
            ?CLIENTDEBUG("Data Recv MegaBytes: ~w~n", [DataRecvBytes / ?B2MB]),
            ?CLIENTDEBUG("Msg Flow Bytes: ~w~n", [MsgFlowBytes]),
            ?CLIENTDEBUG("Msg Send Bytes: ~w~n", [MsgSendBytes]),
            ?CLIENTDEBUG("Msg Recv Bytes: ~w~n", [MsgRecvBytes]),
            ?CLIENTDEBUG("Msg Flow MegaBytes: ~w~n", [MsgFlowBytes  / ?B2MB]),
            ?CLIENTDEBUG("Msg Send MegaBytes: ~w~n", [MsgSendBytes  / ?B2MB]),
            ?CLIENTDEBUG("Msg Recv Bytes: ~w~n", [MsgRecvBytes  / ?B2MB]),
            ?CLIENTDEBUG("Data Length Avg: ~w~n", [DataLenAvg]),
            ?CLIENTDEBUG("Data Frequency: ~w~n", [DataFreq]),
            ?CLIENTDEBUG("Msg Length Avg: ~w~n", [MsgLenAvg]),
            ?CLIENTDEBUG("Msg Frequency: ~w~n", [MsgFreq]);

        _ ->
            fasle
    end .