#!/bin/bash use Erlang 18+



help()
{
    echo "help              显示当前帮助内容"
    echo "--------------------------------------------"
    echo "compile               执行编译"
    echo "server_start          服务器启动"
    echo "test_client_start     测试客户端启动"
    echo "test_client_login     启动客户端并登陆"
    echo "test_client_start_op  登陆客户端执行并随机操作"
    echo "client_login          单一用户登陆"
    echo "client_cmd            单一用户操作"
}

# compile
compile()
{
    ranch_path=./lib/ranch
    cd $ranch_path
    make
    cp ./ebin/* ../../ebin
    rm -rf ./ebin
    cd ..
    cd ..
    erl -make
}

server_start()
{
    erl -s chat_room_test_server start -hidden -sname server -setcookie 123456 -pa ./ebin/
}

test_client_start()
{
   erl -sname client -setcookie 123456 -pa ./ebin/ \
   -s chat_room_test_client start -hidden
}

test_client_login()
{
   BEGIN=$1
   END=$2
   echo ${BEGIN}
   echo ${END}
   erl -sname client -setcookie 123456 -pa ./ebin/ \
   -s chat_room_test_client start -hidden \
   -s chat_room_test_client [client_login, [${BEGIN}, ${END}]]
}

test_client_start_op()
{
   BEGIN=$1
   END=$2
   TIMEGAP=$3
   erl -sname client -setcookie 123456 -pa ./ebin/ \
   -s chat_room_test_client start -hidden \
   -s chat_room_test_client [client_login, [${BEGIN}, ${END}]] \
   -s chat_room_test_client [client_start_op, [${BEGIN}, ${END}, ${TIMEGAP}]]
}

client_login()
{
    USERNAME=$1
    erl -sname client -setcookie 123456 -pa ./ebin/ \
    -s chat_room_test_client start -hidden \
    -s chat_client_server [start [${USERNAME}]]
}

client_cmd()
{
    USERNAME=$1
    CMDNAME=$2
}

## 获取子shell命令
USERCOMMAND=$1
shift
case $USERCOMMAND in
    help) help ;;
    compile) compile ;;
    server_start) server_start ;;
    test_client_start) test_client_start $*;;
    test_client_login) test_client_login $*;;
    test_client_start_op) test_client_start_op $*;;
    client_login) client_login $*;;
    client_cmd) client_cmd $*;;
esac
