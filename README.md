
-----------------
##recon_web
[![Build Status](https://travis-ci.org/zhongwencool/recon_web.png)](https://travis-ci.org/zhongwencool/recon_web)

A web tool using [recon](https://github.com/ferd/recon) to monitor erlang node status. 


**Base on**

[recon](https://github.com/ferd/recon), [cowboy websocket](https://github.com/ninenines/cowboy), [socket.io.client](https://github.com/socketio/socket.io-client),   [hightchat](http://www.highcharts.com/) 

--------------------
##Demo


[Demo](http://23.88.59.186:8080/)
 
$ [`open http://127.0.0.1:8080/`](http://127.0.0.1:8080/)


------------------
###Get Start:

* Get deps and compile && start in erlang shell mode:

	$ `make && make shell`
	$ [`open http://127.0.0.1:8080/`](http://127.0.0.1:8080/)

* Combined with your system:

   1> application:ensure_all_started(recon_web).
   
   2> application:stop(recon_web).
	
----------
###Config

* **Recommend** using Line command 

    $ `make config IP=127.0.0.1 PORT=8080`
    
1. it will modify **Client** IP and PORT:

    /priv/js/recon_web.js

	var socket =  io.connect(**'http://yourhost:yourport'**);

2. it will modify **Server** IP and PORT:

    /src/recon_web.app.src

    {ip,{**"127.0.0.1"**}},%% default :localhost,better using nginx to proxypass

    {port,**8080**}]}
    
----------  

###Some Useful Command

| Command                            | Action | 
| ------------                       | ------------- 
| make help                          | erlang.mk's help
| make config IP=127.0.0.1 PORT=8080 | configure IP and Port  
| make debug                         | compile && start debug shell mode lager:debug
| make shell                         | start info shell mode lager:info
| make start                         | start a demon erlang node by heart
| make stop                          | stop erlang node
| make clean_all                     | clean all beam (include deps beam)
| make remsh                         | remsh mode shell
| rake                               | 

-------------------
###Flow
![pic](http://7xi3w8.com1.z0.glb.clouddn.com/recon_web_flow949_561.jpg)
