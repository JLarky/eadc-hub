all: ebin ebin/tiger.beam ebin/tiger.app tiger

ebin:
	mkdir -p ebin

ebin/%.beam: src/%.erl
	erlc -o ebin $<

tiger: priv/tiger_drv.so

priv/tiger_drv.so: priv/*.c
	(cd priv; make;)

ebin/tiger.app: src/tiger.app
	cp src/tiger.app ebin/tiger.app

clean:
	rm -rf ebin/
	(cd priv; make clean;)