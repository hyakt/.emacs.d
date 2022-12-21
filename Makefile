TOP_DIR := $(realpath $(dir $(lastword $(MAKEFILE_LIST))))

.PHONY: init
init: compile link

.PHONY: link
link:
	ln -nfs $(TOP_DIR) ~/

.PHONY: compile
compile:
	emacs -Q --batch -f batch-byte-compile early-init.el
	emacs -Q --batch -f batch-byte-compile init.el
