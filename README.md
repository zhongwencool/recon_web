
-----------------
##recon_web

A web tool using [recon](https://github.com/ferd/recon) to monitor erlang node staus. 

[![Build Status](https://travis-ci.org/zhongwencool/recon_web.png)](https://travis-ci.org/zhongwencool/recon_web)


[recon](https://github.com/ferd/recon), [cowboy websocket](https://github.com/ninenines/cowboy), [socket.io.client](https://github.com/socketio/socket.io-client),   [hightchat](http://www.highcharts.com/) 

-----------------
###Demo

  `open http://182.254.178.59:8080/`

------------------
###Get Start:

* Get deps and compile:

	$ `make`

* Start in erlang shell:

	$ `make shell`    
	
* Result:

	$ `open http://127.0.0.1:8080/`
	
----------
###Config

* **Recommend** using Line command 

    $ `make config IP+=127.0.0.1 PORT+=8080`
    
1. it will modify **Client** IP and PORT:

    /priv/js/recon_web.js

	var socket =  io.connect(**'http://yourhost:yourport'**);

2. it will modify **Server** IP and PORT:

    /src/recon_web.app.src

    {ip,{**"10,142,35,165"**}},%% default :localhost,better using nginx to proxypass

    {port,**8080**}]}
    
----------  

###Some Userfull Command
* make help 
* make clean_all clear all beam include deps beam
* make debug    # start debug module lager:info
* make remsh    # remsh mode shell
* make restart  # restart node
* make shell    # erl -pa ../recon_web/ebin deps/*/ebin
* make start    # start a demon erlang node by heart
* make stop     # stop erlang node normal
* rake




