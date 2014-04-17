
PREFIX:=../
DEST:=$(PREFIX)$(PROJECT)

REBAR=./rebar

# 节点名称
NODE=$(shell cat ./config/node_name.conf)
ifeq ($(NODE),)
        NODE = slg_gm_proxy@127.0.0.1
endif

OPTS = \
	-pa ebin edit deps/*/ebin \
	-name $(NODE) \
	-setcookie abcdeft \
        $(NULL)
ERL_CALL=erl_call -c abcdeft \
	-name $(NODE) -e

all:
	@$(REBAR) get-deps compile

edoc:
	@$(REBAR) doc

test:
	@rm -rf .eunit
	@mkdir -p .eunit
	@$(REBAR) skip_deps=true eunit

clean:
	@$(REBAR) clean

build_plt:
	@$(REBAR) build-plt

dialyzer:
	@$(REBAR) dialyze

app:
	@$(REBAR) create template=mochiwebapp dest=$(DEST) appid=$(PROJECT)

# 调试启动节点
start:
	erl $(OPTS) -s proxy -s reloader

# 后台运行节点
s:
	erl $(OPTS) -detached -s proxy -s reloader

#mash
mash:
	erl -name mash_shell@127.0.0.1 \
	    -setcookie abcdeft
	    -remsh $(NODE)

# 停止erlang节点.
stop:
	echo "proxy:stop()." | $(ERL_CALL)
	-$(ERL_CALL) -q

# 重启服务器
restart:
        echo "proxy:stop()." | $(ERL_CALL)
	-$(ERL_CALL) -q
	erl $(OPTS) -detached -s proxy -s reloader