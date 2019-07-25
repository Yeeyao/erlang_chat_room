%%%-------------------------------------------------------------------
%%% @author huangyao
%%% @copyright (C) 2018, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 14. 八月 2018 21:14
%%%-------------------------------------------------------------------
-author("huangyao").

%%%===================================================================
%%% 宏定义
%%%===================================================================
-define(VERMIN, 0).
-define(VERMAX, 100).
-define(HORMIN, 0).
-define(HORMAX, 100).
-define(NEARRADIUS, 25).
-define(OTHERCHANNELNUM, 5).
-define(WORLDCHANNELID, ch0).

%% MODULE names
-define(USERINFOSERVER, chat_room_user_info_server).

%% ETS tables
-define(TSERVERCHANNEL ,t_server_channel).
-define(TDEBUGFLAT ,t_debug_flag).
-define(TALLUSERINFO, t_all_user_info).

%% debug flags
-define(NEEDDEBUG, false).
-define(CLIENTNEEDLOG, true).  %% 控制玩家进程是否需要记录流量信息
-define(LOGNEEDLOG, false).  %% 控制日志进程是否需要在terminal打印流量信息

-define(DEBUG, ?NEEDDEBUG andalso io:format).
-define(IOLOG, ?LOGNEEDLOG andalso io:format).

%%% 可以主动设置打印
-define(CLIENTDEBUG, io:format).
-define(CHANNELDEBUG, io:format).
-define(SERVERDEBUG, io:format).

%%% 一定打印
-define(ERRORLOG, io:format).


-define(IIF(X, Y, Z), case (X) of true -> (Y); _ -> (Z) end).

-define(IN, in).
-define(OUT, out).

-define(OTHER, other).
-define(WITHMSG, with_msg).

%%%===================================================================
%%% record定义
%%%===================================================================
-record(r_user, {name = undefined, pid, socket, pos = {0, 0},
    current_chid, channel_list = []}).  %% socket为用户进程的socket

-record(r_user_channel, {name = undefined, socket, pos = {0, 0}, block_num = 0}).

-record(r_user_world, {name = undefined, socket}).

-record(r_user_wc, {name = undefined, socket}).

-record(r_channel, {id, channel_pid, user_num}).

-record(r_user_to_log, {name = <<>>, in_out = <<>>, bin = <<>>, msg = "", type = ?OTHER}).

%%% 与msg无关：op_times, dfb, dsb, drb, dla, df

-record(r_user_log_info, {
    user_name = "",
    begin_log_time = 0, op_times = 0, msg_op_times = 0, other_op_times = 0,
    data_flow_bytes = 0, data_send_bytes = 0, data_recv_btyes = 0,
    msg_flow_bytes = 0, msg_send_bytes = 0, msg_recv_bytes = 0,
    data_length_average = 0, data_frequency = 0,
    msg_length_average = 0, msg_frequency = 0
}).


