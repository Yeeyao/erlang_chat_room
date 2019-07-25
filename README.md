erl_chat_room
===============

An erlang chat room demo

# 总体描述

&emsp;&emsp;  整个项目包含6个主要部分：频道服务器，客户端，监听服务器，与客户端连接的服务器（由监听服务器在客户端连接时启动），保存所有用户信息的服务器，记录客户端消息收发情况的日志服务器。其中，客户端，与客户端直接通信的服务器，日志服务器都是gen_server行为模式。

# 使用方法

&emsp;&emsp;说明：监听服务器和频道服务器需要在同一个节点上启动，客户端可以在多个节点启动，也可以多个客户端在一个节点启动。日志服务器需要与所要记录日志的客户端在一个节点上启动才能实现记录，同时注意日志服务器需要在客户端启动前启动。

### 编译

- 工作目录为chat_room目录，执行sh compile.sh命令，这里将自动启动erl。
```erlang
    sh compile.sh
```

### 通用过程

- 启动监听(连接)服务器
```erlang
    ok = application:start(crypto).
    ok = application:start(asn1).
    ok = application:start(public_key).
    ok = application:start(ssl).
    ok = application:start(ranch).
    chat_room_test_server:start().
    chat_room_test_server:start().
```

- 启动日志服务器
```erlang
    chat_room_log_server:start().
```

### 1.使用测试模块启动多个客户端的方式

- 测试模块启动多个客户端(Begin和End都是数字且Begin >= End)
```erlang
    chat_room_test_client:client_login(Begin, End).  
    %% 测试的用户名以a开头，后面为数字，这里启动Begin - End个客户端
```

- 一定数量的用户执行操作测试(TimeGap为执行的时间间隔，单位是ms)
```erlang
    chat_room_test_client:client_start_op(Begin, End, TimeGap).
```

- 一定数量的用户停止执行操作
```erlang
    chat_room_test_client:client_stop_op().
```

- 一定数量的用户登出
```erlang
    chat_room_test_client:client_logout(Begin, End).
```

### 2.单独启动一个客户端的方式

- 启动客户端
```erlang
    chat_room_test_client:start(UserName).
```

## 模块的使用

### 1.客户端的使用

- 命令格式(UserName是执行命令的用户，该用户需要是已经登录的状态):

```erlang
    chat_client_server:command(UserName, Cmd).
```

- 支持的命令(Cmd)说明:

```erlang
    {private_chat, ReceiverName, Msg}  %% 私聊

    {channel_chat, Msg}  %% 频道聊天

    {world_chat, Msg}  %% 世界聊天

    {near_chat, Msg}  %% 附近人聊天(同频道下)

    {join_channel, ChannelID}  %% 加入一个频道(不改变当前频道)

    {leave_channel, ChannelID}  %% 退出一个频道(不改变当前频道)

    {change_current_channel, ChannelID}  %% 改变当前频道
    
    {get_channel_user_info}  %% 获取当前频道的其他玩家信息(名字和位置)

    {get_self_log_info}  %% 获取该用户当前流量信息

    {upload_user_info}  %% 上传该用户流量信息

    {random_move}  %% 随机移动(直接改变用户坐标)

    {set_client_debug, DebugFlag}  %% 客户端调试信息打印开关

    {set_client_log_info_debug, DebugFlag}  %% 用户流量信息打印开关

    {set_server_debug, DebugFlag}  %% 对应通信进程的调试信息打印开关

    {stop}  %% 停止客户端
```

### 2.日志服务器的使用

- 命令种类和说明：

```erlang
    chat_room_log_server:get_one_user_log_info().  
    %% 获取指定用户的日志记录信息，日志写入在log.txt文件中，打印可以通过chat_rool.hrl进的宏进行开关

    chat_room_log_server:get_one_user_log_info().  
    %% 获取所有用户的日志记录信息
```