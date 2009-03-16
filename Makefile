ERLC=erlc -o ebin -I include/

all: ebin/eadc_client_fsm.beam ebin/eadc_listener.beam ebin/eadc_app.beam \
	 ebin/eadc_utils.beam ebin/eadc_master.beam ebin/eadc_plugin.beam \
	ebin/plugin_bot.beam ebin/plugin_hub_merge.beam plugins tiger \
	ebin/eadc_user.beam

ebin/eadc_client_fsm.beam: src/eadc_client_fsm.erl
	$(ERLC) $^
ebin/eadc_listener.beam: src/eadc_listener.erl
	$(ERLC) $^
ebin/eadc_app.beam: src/eadc_app.erl
	$(ERLC) $^
ebin/eadc_utils.beam: src/eadc_utils.erl
	$(ERLC) $^
ebin/eadc_master.beam: src/eadc_master.erl
	$(ERLC) $^
ebin/eadc_user.beam: src/eadc_user.erl
	$(ERLC) $^
ebin/eadc_plugin.beam: src/eadc_plugin.erl
	$(ERLC) $^
ebin/plugin_bot.beam: src/plugin_bot.erl
	$(ERLC) $^
ebin/plugin_hub_merge.beam: src/plugin_hub_merge.erl
	$(ERLC) $^
plugins:
	$(ERLC) src/plugins/*.erl

tiger: ebin/tiger.beam priv/tiger_drv.so

priv/tiger_drv.so: priv/tiger.c priv/tiger_drv.c
	(cd priv; make)

ebin/tiger.beam: priv/tiger.erl
	$(ERLC) $^

boot: all
	(cd ebin; echo 'systools:make_script("eadc"),erlang:halt().' | erl)

clean:
	$(RM) ebin/*.beam ebin/*.boot ebin/*.script ebin/*crash.dump \
	ebin/*~ src/*~ priv/*~ priv/*.so priv/*.dll

cleandb:
	$(RM) -r ebin/Mnesia*
