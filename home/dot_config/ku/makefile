MAKEFLAGS += --jobs=$(shell getconf _NPROCESSORS_ONLN)

PANDOC_OPTIONS =
PANDOC_HTML_OPTIONS =
PANDOC_DOCX_OPTIONS =

BUILD_DIR := ./output

NODES := $(shell find . -name '*.md')
SRC_OBJS := $(shell find docs -type f ! -name '*.md')
OBJS := $(SRC_OBJS:%=$(BUILD_DIR)/%)

DOCX := $(NODES:%.md=$(BUILD_DIR)/%.docx)
HTML := $(NODES:%.md=$(BUILD_DIR)/%.html)

all: $(DOCX) $(OBJS)

HEADER = $(shell head -n 1 $< | cut -c 3- | sed 's/"/\\"/g')
NODE = $(shell echo "$<" | awk -F'/' '{print $(NF-1)}')

$(BUILD_DIR)/%.docx: %.md
	mkdir -p $(dir $@)
	pandoc -s -f gfm -t docx --metadata title="$<" --resource-path "$(dir $<)" -o $@ $<

$(BUILD_DIR)/%.html: %.md
	mkdir -p $(dir $@)
	pandoc -s -f gfm -t docx --metadata title="$<" --resource-path "$(dir $<)" -o $@ $<

$(OBJS): $(BUILD_DIR)/%: %
	mkdir -p $(dir $@)
	cp $< $@

$(DOCX) $(OBJS): | $(BUILD_DIR)

$(BUILD_DIR):
	mkdir -p $@

.PHONY: clean
clean:
	rm -rf $(BUILD_DIR)
