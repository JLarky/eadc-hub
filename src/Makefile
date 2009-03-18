include ../support/include.mk
PLUGINS_SOURCES := $(wildcard plugins/*.erl)
PLUGINS_OBJECTS := $(PLUGINS_SOURCES:plugins/%.erl=$(EBIN_DIR)/%.$(EMULATOR))

all: $(EBIN_FILES_NO_DOCS) plugins

plugins: $(PLUGINS_OBJECTS)

$(EBIN_DIR)/%.$(EMULATOR): plugins/%.erl
	$(ERLC) $(ERLC_FLAGS) -o $(EBIN_DIR) $<

docs: $(ERL_DOCUMENTS)

debug:
	$(MAKE) DEBUG=-DDEBUG

clean:
	rm -rf $(EBIN_FILES) $(PLUGINS_OBJECTS)