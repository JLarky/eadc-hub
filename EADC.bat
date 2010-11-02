cd ebin &&
mkdir -p ../priv && cp ../deps/tiger/priv/*.dll ../priv/ &&
erl -pa ../deps/*/ebin/ -boot start_sasl -sname eadc -eval "application:start(tiger)" -eval "application:start(eadc)" $@