TOP_DIR := $(realpath $(dir $(lastword $(MAKEFILE_LIST))))
EMACS_MAC_PORT_APP_PATH = $(shell brew --prefix emacs-mac)

.PHONY: init
init: emacs-mac-port link compile all-the-icons

.PHONY: link
link:
	ln -nfs $(TOP_DIR) ~/

.PHONY: compile
compile:
	emacs -Q --batch -f batch-byte-compile early-init.el
	emacs -Q --batch -f batch-byte-compile init.el
	emacs -Q --batch -f batch-byte-compile lisp/functions/*.el
	emacs -Q --batch --eval "(progn (require 'package) (package-generate-autoloads \"my-functions\" \"~/.emacs.d/lisp/functions\"))"

.PHONY: emacs-mac-port
emacs-mac-port:
	brew tap railwaycat/emacsmacport
	brew install emacs-mac --with-ctags --with-glib --with-mac-metal --with-native-comp --with-natural-title-bar --with-librsvg --with-starter --with-xwidgets
	ln -sfv $(EMACS_MAC_PORT_APP_PATH)/Emacs.app /Applications
	# Adopting mac dark mode
	defaults write org.gnu.Emacs TransparentTitleBar DARK
	# Set No Title Bar Icon
	defaults write org.gnu.Emacs HideDocumentIcon YES

all-the-icons:
	emacs -Q --batch --eval '(progn (package-initialize) (all-the-icons-install-fonts t))'