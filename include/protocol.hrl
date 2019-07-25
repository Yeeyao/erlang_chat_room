%%%-------------------------------------------------------------------
%%% @author huangyao
%%% @copyright (C) 2018, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 21. 八月 2018 19:18
%%%-------------------------------------------------------------------
-author("huangyao").

%%%===================================================================
%%% 1000 - 1099 : 玩家登录
-record(r_login_cs, {user_name = name}).  %% 1000 玩家登录
-record(r_login_sc, {msg = ""}).  %% 1001 登录返回

%%% 2000 - 2099 : 聊天
-record(r_private_chat_cs, {receiver_name = name, msg = ""}).  %% 2000 玩家私聊
-record(r_private_chat_sc, {chat_type = undefined, sender_name = name,
    sender_pos = { }, receiver_pos = { }, msg = ""}).  %% 2001 私聊返回以及发送

%%-record(sc_pc_send, {chat_type, sender_name, sender_pos, receiver_pos, msg}).  %% 2002 聊天发送给目标

-record(r_channel_chat_cs, {msg = ""}).  %% 2010 频道聊天
-record(r_channel_chat_sc, {msg = ""}).  %% 2011 频道聊天返回

-record(r_world_chat_cs, {msg = ""}).  %% 2020 世界聊天
-record(r_world_chat_sc, {msg = ""}).  %% 2021 世界聊天

-record(r_near_chat_cs, {msg = ""}).  %% 2030 附近聊天
-record(r_near_chat_sc, {msg = ""}).  %% 2031 附近聊天返回

%%% 2100 - 2199：频道
-record(r_join_channel_cs, {channel_id = id}).  %% 2100 加入频道
-record(r_join_channel_sc, {msg = ""}).  %% 2101 加入频道返回

-record(r_leave_channel_cs, {channel_id = id}).  %% 2110 离开频道
-record(r_leave_channel_sc, {msg = ""}).  %% 2111 离开频道返回

-record(r_ccc_cs, {channel_id = id}).  %% 2120 改变当前频道  ccc stands for change current channel
-record(r_ccc_sc, {msg = ""}).  %% 2121 改变当前频道返回

-record(r_scui_cs, {}).  %% 2130 查询当前频道玩家信息 scui stands for send channel user info ...
-record(r_scui_sc, {user_num = 0, user_info_list = [ ]}).  %% 2131 查询当前频道玩家信息返回

-record(r_get_self_info_cs, {}).  %% 2132 查询玩家当前信息
-record(r_get_self_info_sc, {r_user}).  %% 2133

-record(r_cgm_cs, {msg = ""}).  %% 2200 将玩家得到的消息转发给服务器  cgm stands for client get message

-record(r_random_move_cs, {}).  %% 2300 玩家发送随机移动请求 ...
-record(r_random_move_sc, {msg = ""}).  %% 2301 玩家得到移动的结果
%%%===================================================================

%%%===================================================================
-define(PROTO_LOGIN_CS, 1000).  %% 玩家登录
-define(PROTO_LOGIN_SC, 1001).  

-define(PROTO_PRIVATE_CHAT_CS, 2000).  %% 私聊
-define(PROTO_PRIVATE_CHAT_SC, 2001).  

-define(PROTO_CHANNEL_CHAT_CS, 2010).  %% 频道聊天
-define(PROTO_CHANNEL_CHAT_SC, 2011).  

-define(PROTO_WORLD_CHAT_CS, 2020).  %% 世界聊天
-define(PROTO_WORLD_CHAT_SC, 2021).

-define(PROTO_NEAR_CHAT_CS, 2030).  %% 附近聊天
-define(PROTO_NEAR_CHAT_SC, 2031).

-define(PROTO_JOIN_CHANNEL_CS, 2100).  %% 加入频道
-define(PROTO_JOIN_CHANNEL_SC, 2101).  

-define(PROTO_LEAVE_CHANNEL_CS, 2110).  %% 离开频道
-define(PROTO_LEAVE_CHANNEL_SC, 2111).  

-define(PROTO_CCC_CS, 2120).  %% 改变当前频道 
-define(PROTO_CCC_SC, 2121).  

-define(PROTO_SCUI_CS, 2130).  %% 查询当前频道玩家信息
-define(PROTO_SCUI_SC, 2131).

-define(PROTO_GET_SELF_INFO_CS, 2132).  %% 查询玩家自己的信息
-define(PROTO_GET_SELF_INFO_SC, 2133).

-define(PROTO_CGM_CS, 2200).  %% 将玩家得到的信息转发给服务器

-define(PROTO_RANDOM_MOVE_CS, 2300).  %% 随机移动  
-define(PROTO_RANDOM_MOVE_SC, 2301).  %% 随机移动  
%%%===================================================================