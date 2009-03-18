MARKDOWN_SOURCES=$(wildcard doc/*.md)
MARKDOWN_TARGETS=$(patsubst doc/%.md,doc/html/%.html,$(MARKDOWN_SOURCES))

all: eadc boot deps

eadc: ebin
	(cd src;$(MAKE))

deps: tiger

tiger:
	(cd deps/tiger;$(MAKE))
	(mkdir -p priv/)
	(cp deps/tiger/priv/tiger_drv.so ./priv/)

docs: erlang-docs html-docs

erlang-docs: doc/edoc
	(cd src;$(MAKE) docs)

html-docs: doc/html $(MARKDOWN_TARGETS)

doc/edoc:
	mkdir -p doc/edoc

doc/html:
	mkdir -p doc/html

doc/html/%.html: doc/%.md
	(title=`grep '^# ' $< | head -1 | sed -e 's:^# ::'` ;\
	 t=/tmp/$*.md ;\
	 sed -e "s:@TITLE@:$$title:g" < doc/header.html > $@ ;\
	 python doc/buildtoc.py < $< > $$t ;\
	 markdown $$t >> $@ ;\
	 rm $$t ;\
	 cat doc/footer.html >> $@)

ebin:
	mkdir -p ebin

clean: clean-docs
	(cd src;$(MAKE) clean)
	(cd deps/*/; $(MAKE) clean)
	$(RM) -r priv
	$(RM) ebin/*.boot ebin/*.script ebin/*crash.dump ebin/*~ src/*~ priv/*~

clean-docs: clean-html
	rm -rf doc/edoc

clean-html:
	rm -rf doc/html

boot: ebin/eadc.boot

ebin/eadc.boot:
	(cd ebin; echo 'systools:make_script("eadc"),erlang:halt().' | erl)

cleandb:
	$(RM) -r ebin/Mnesia*
