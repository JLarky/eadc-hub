all: ebin ebin/tiger.beam ebin/tiger.app

ebin:
	mkdir -p ebin

ebin/%.beam: src/%.erl
	erlc -o ebin $<

tiger-win: priv/tiger_drv.dll

priv/tiger_drv.dll: priv/*.c
	(cd priv; make tiger-win;)

tiger: priv/tiger_drv.so

priv/tiger_drv.so: priv/*.c
	(cd priv; make tiger;)

ebin/tiger.app: src/tiger.app
	cp src/tiger.app ebin/tiger.app

clean:
	rm -rf ebin/
	(cd priv; make clean;)