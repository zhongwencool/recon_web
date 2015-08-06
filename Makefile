PROJECT = recon_web

DEPS = lager cowboy jsx recon

dep_lager = git git://github.com/basho/lager.git 2.0.3
dep_cowboy = git git://github.com/ninenines/cowboy.git 1.0.1
dep_jsx = git git://github.com/talentdeficit/jsx.git v2.6.0
dep_recon = git git://github.com/ferd/recon.git 2.2.1

include erlang.mk

# Compile flags
ERLC_COMPILE_OPTS= +'{parse_transform, lager_transform}'

# Append these settings
ERLC_OPTS += $(ERLC_COMPILE_OPTS)
TEST_ERLC_OPTS += $(ERLC_COMPILE_OPTS)

.PHONY: clean_all

dir = $(dir, "./deps/")


clean_all:
	@echo "clean_all start"
	for file_a in `ls ./deps`; do \
	cd ./deps/$$file_a;\
	make clean;\
	cd -;\
	done 
	@echo "clean_all done"
