DESTDIR=eadc-hub

all:

clean:
	$(RM) -r eadc eadc-hub

build:
	../rebar generate force=1

install:
	install -d $(DESTDIR)/usr/lib/eadc-hub/bin
	install -m 0755 eadc/bin/eadc -t $(DESTDIR)/usr/lib/eadc-hub/bin
	cp -r eadc/erts-5.8 $(DESTDIR)/usr/lib/eadc-hub/
	cp -r eadc/lib $(DESTDIR)/usr/lib/eadc-hub/
	cp -r eadc/releases $(DESTDIR)/usr/lib/eadc-hub/
	install -d $(DESTDIR)/var/log/eadc-hub/
	install -d $(DESTDIR)/etc/eadc-hub/
	cp eadc/etc/app.config $(DESTDIR)/etc/eadc-hub
	cp eadc/etc/vm.args $(DESTDIR)/etc/eadc-hub
	install -d $(DESTDIR)/var/spool/eadc-hub/Mnesia
	install -d $(DESTDIR)/var/run/eadc-hub

.PHONY: install