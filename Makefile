PROJECT = recon_web

DEPS = lager cowboy jsx recon

dep_lager = git git://github.com/basho/lager.git 2.0.3
dep_cowboy = git git://github.com/ninenines/cowboy.git 1.0.1
dep_jsx = git git://github.com/talentdeficit/jsx.git v2.6.0
dep_recon = git git://github.com/ferd/recon.git 2.2.1

include erlang.mk

SHELL_OPTS = -s recon_web start

# Compile flags
ERLC_COMPILE_OPTS= +'{parse_transform, lager_transform}'

# Append these settings
ERLC_OPTS += $(ERLC_COMPILE_OPTS)
TEST_ERLC_OPTS += $(ERLC_COMPILE_OPTS)

.PHONY: clean_all

clean_all:
	@echo "clean_all start"
	for file_a in `ls ./deps`; do \
	cd ./deps/$$file_a;\
	make clean;\
	cd -;\
	done; \
	make clean
	@echo "clean_all done"

.PHONY: config

IP+=127.0.0.1
PORT+=8080

config:
	@echo "Setting IP:$(IP) PORT:$(PORT)"
	perl -p -i -e "s/^var HostPort .*/var HostPort = \'http:\/\/$(IP):$(PORT)\';/g" ./priv/js/recon_web.js
	#perl -p -i -e "s/^{ip,.*/{ip, \"$(IP)\"},/g" ./src/recon_web.app.src
	perl -p -i -e "s/^{port,.*/{port, $(PORT)}/g" ./src/recon_web.app.src
	@echo "Done"

.PHONY: start
start:
	erl -heart -env HEART_COMMAND 'rake restart' -sname zhongwencool -setcookie zhongwencool -pa `pwd`/ebin deps/*/ebin -detached -noinput -noshell -s recon_web

.PHONY: stop
stop:
	erl -noshell -sname temp_control -setcookie zhongwencool -eval \"rpc:call\(zhongwencool@localhost, init, stop, \[\]\)\" -s init stop

.PHONY: restart
restart: stop restart

.PHONY: debug
debug:
	rebar co&& exec erl -sname zhongwencool -setcookie zhongwencool -pa `pwd`/ebin deps/*/ebin -boot start_sasl -s recon_web start debug

.PHONY: remsh
remsh:
	erl -setcookie zhongwencool -remsh zhongwencool@localhost -sname remsh_tmp


