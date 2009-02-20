all: ebin/eadc_client_fsm.beam ebin/eadc_listener.beam ebin/eadc_app.beam ebin/eadc_utils.beam ebin/eadc_master.beam

ebin/eadc_client_fsm.beam: src/eadc_client_fsm.erl
	erlc -o ebin $^
ebin/eadc_listener.beam: src/eadc_listener.erl
	erlc -o ebin $^
ebin/eadc_app.beam: src/eadc_app.erl
	erlc -o ebin $^
ebin/eadc_utils.beam: src/eadc_utils.erl
	erlc -o ebin $^
ebin/eadc_master.beam: src/eadc_master.erl
	erlc -o ebin $^


boot: all
	(cd ebin; echo 'systools:make_script("eadc"),erlang:halt().' | erl)

clean:
	$(RM) ebin/*.beam ebin/*.boot ebin/*.script ebin/*crash.dump \
	ebin/*~ src/*~
