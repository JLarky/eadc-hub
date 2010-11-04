DESTDIR=eadc-hub

all: clean build install build-deb

clean:
	$(RM) -rf eadc *.deb
	sudo rm -rf $(DESTDIR)
	$(RM) ../ebin/erl_crash.dump ../ebin/error.log

build:
	../rebar generate force=1

build-deb:
	sudo chown root.root -R $(DESTDIR)
	sudo chown nobody.nogroup -R $(DESTDIR)/var
	dpkg -b $(DESTDIR)

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
	install -d $(DESTDIR)/etc/init.d
	install -m 0755 files/eadc-hub.init.d $(DESTDIR)/etc/init.d/eadc-hub
	install -d $(DESTDIR)/var/lib/eadc-hub/Mnesia
	install -d $(DESTDIR)/var/run/eadc-hub

	install -d $(DESTDIR)/DEBIAN
	cp -r debian/* $(DESTDIR)/DEBIAN

.PHONY: install