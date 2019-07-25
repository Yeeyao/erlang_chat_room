%%%-------------------------------------------------------------------
%%% @author huangyao
%%% @copyright (C) 2018, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 14. 八月 2018 21:11
%%%-------------------------------------------------------------------

%%%===================================================================
%%%===================================================================

-module(protocol).

-include("protocol.hrl").

-export([decode/1, encode/2]).

%%%===================================================================
%%% decode
%%%===================================================================
decode(<<Head:32, RecvBin/binary>>) ->
    decode2(Head, binary_to_term(RecvBin)).

decode2(1000, #r_login_cs{user_name = UserName}) ->
    [1000, {UserName}];

decode2(1001, #r_login_sc{msg = Msg}) ->
    [1001, {Msg}];

decode2(2000, #r_private_chat_cs{receiver_name = ReceiverName, msg = Msg}) ->
    [2000, {ReceiverName, Msg}];

decode2(2001, Record) ->
    #r_private_chat_sc{chat_type = ChatType, sender_name = SenderName, msg = Msg,
        sender_pos = SenderPos, receiver_pos = ReceiverPos} = Record,
    [2001, {ChatType, SenderName, SenderPos, ReceiverPos, Msg}];

decode2(2010, #r_channel_chat_cs{msg = Msg}) ->
    [2010, {Msg}];

decode2(2011, #r_channel_chat_sc{msg = Msg}) ->
    [2011, {Msg}];

decode2(2020, #r_world_chat_cs{msg = Msg}) ->
    [2020, {Msg}];

decode2(2021, #r_world_chat_sc{msg = Msg}) ->
    [2021, {Msg}];

decode2(2030, #r_near_chat_cs{msg = Msg}) ->
    [2030, {Msg}];

decode2(2031, #r_near_chat_sc{msg = Msg}) ->
    [2031, {Msg}];

decode2(2100, #r_join_channel_cs{channel_id = ChannelID}) ->
    [2100, {ChannelID}];

decode2(2101, #r_join_channel_sc{msg = Msg}) ->
    [2101, {Msg}];

decode2(2110, #r_leave_channel_cs{channel_id = ChannelID}) ->
    [2110, {ChannelID}];

decode2(2111, #r_leave_channel_sc{msg = Msg}) ->
    [2111, {Msg}];

decode2(2120, #r_ccc_cs{channel_id = ChannelID}) ->
    [2120, {ChannelID}];

decode2(2121, #r_ccc_sc{msg = Msg}) ->
    [2121, {Msg}];

decode2(2130, #r_scui_cs{}) ->
    [2130, {}];

decode2(2131, #r_scui_sc{user_num = UserNum, user_info_list = UserInfoList}) ->
    [2131, {UserNum, UserInfoList}];

decode2(2132, #r_get_self_info_cs{}) ->
    [2132, {}];

decode2(2133, #r_get_self_info_sc{r_user = RUser}) ->
    [2133, {RUser}];

decode2(2200, #r_cgm_cs{msg = Msg}) ->
    [2200, {Msg}];

decode2(2300, #r_random_move_cs{}) ->
    [2300, {}];

decode2(2301, #r_random_move_sc{msg = Msg}) ->
    [2301, {Msg}];

decode2(_, _Record) ->
    [0, {0}].

%%%===================================================================
%%% encode
%%%===================================================================
encode(1000, RLoginCs) ->
    BinRecord = term_to_binary(RLoginCs),
    <<1000:32, BinRecord/binary>>;

encode(1001, RloginSc) ->
    BinRecord = term_to_binary(RloginSc),
    <<1001:32, BinRecord/binary>>;

encode(2000, RpcCs) ->
    BinRecord = term_to_binary(RpcCs),
    <<2000:32, BinRecord/binary>>;

encode(2001, RpcSc) ->
    BinRecord = term_to_binary(RpcSc),
    <<2001:32, BinRecord/binary>>;

encode(2010, RccCs) ->
    BinRecord = term_to_binary(RccCs),
    <<2010:32, BinRecord/binary>>;

encode(2011, RccSc) ->
    BinRecord = term_to_binary(RccSc),
    <<2011:32, BinRecord/binary>>;

encode(2020, RwcCs) ->
    BinRecord = term_to_binary(RwcCs),
    <<2020:32, BinRecord/binary>>;

encode(2021, RwcSc) ->
    BinRecord = term_to_binary(RwcSc),
    <<2021:32, BinRecord/binary>>;

encode(2030, RncCs) ->
    BinRecord = term_to_binary(RncCs),
    <<2030:32, BinRecord/binary>>;

encode(2031, RncSc) ->
    BinRecord = term_to_binary(RncSc),
    <<2031:32, BinRecord/binary>>;

encode(2100, RjcCs) ->
    BinRecord = term_to_binary(RjcCs),
    <<2100:32, BinRecord/binary>>;

encode(2101, RcccCS) ->
    BinRecord = term_to_binary(RcccCS),
    <<2101:32, BinRecord/binary>>;

encode(2110, RlcCs) ->
    BinRecord = term_to_binary(RlcCs),
    <<2110:32, BinRecord/binary>>;

encode(2111, RlcSc) ->
    BinRecord = term_to_binary(RlcSc),
    <<2111:32, BinRecord/binary>>;

encode(2120, RcccCs) ->
    BinRecord = term_to_binary(RcccCs),
    <<2120:32, BinRecord/binary>>;

encode(2121, RcccSc) ->
    BinRecord = term_to_binary(RcccSc),
    <<2121:32, BinRecord/binary>>;

encode(2130, RscCs) ->
    BinRecord = term_to_binary(RscCs),
    <<2130:32, BinRecord/binary>>;

encode(2131, RscuiSc) ->
    BinRecord = term_to_binary(RscuiSc),
    <<2131:32, BinRecord/binary>>;

encode(2132, RGetSelfInfoCs) ->
    BinRecord = term_to_binary(RGetSelfInfoCs),
    <<2132:32, BinRecord/binary>>;

encode(2133, RGetSelfInfoSc) ->
    BinRecord = term_to_binary(RGetSelfInfoSc),
    <<2133:32, BinRecord/binary>>;

encode(2200, RCsmCs) ->
    BinRecord = term_to_binary(RCsmCs),
    <<2200:32, BinRecord/binary>>;

encode(2300, RrmCs) ->
    BinRecord = term_to_binary(RrmCs),
    <<2300:32, BinRecord/binary>>;

encode(2301, RrmSc) ->
    BinRecord = term_to_binary(RrmSc),
    <<2301:32, BinRecord/binary>>;

encode(_, _Tuple) ->
    <<>>.