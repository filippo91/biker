#! /bin/bash

cmd_list=("build" "start" "stop" "attach" "join" "clean")

cmd=$1
dev=$2

case $cmd in
build) 
    sudo rm -rf dev/dev{1..3}/data/ring/*
    sudo rm -rf dev/dev{1..3}/data/cluster_meta/*
    for d in dev/dev*
    do  
        sudo $d/bin/biker stop
    done
    make devrel
    ;;
start)
    for d in dev/dev*
    do  
        sudo $d/bin/biker start
    done
  ;;
stop)
    for d in dev/dev*
    do  
        sudo $d/bin/biker stop
    done
  ;;
ping)
   for d in dev/dev*
    do  
        $d/bin/biker ping
    done
 
  ;;
clean)
    sudo rm -rf dev/dev{1..3}/data/ring/*
    sudo rm -rf dev/dev{1..3}/data/cluster_meta/*
    ;;
join)
    sudo rm -rf dev/dev{1..3}/data/ring/*
    sudo rm -rf dev/dev{1..3}/data/cluster_meta/*
    for d in dev/dev{2,3}   
    do  
        $d/bin/biker-admin join biker1@127.0.0.1
    done
  ;;
attach) 
    sudo ./dev/dev$dev/bin/biker attach
    ;;
status)
    ./dev/dev1/bin/biker-admin member_status
    ;;
*)
  echo "WTF did you type? :D"
  echo "commands available: " ${cmd_list[*]}

  ;;
esac
