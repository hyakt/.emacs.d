TOP_DIR := $(realpath $(dir $(lastword $(MAKEFILE_LIST))))
EMACS_MAC_PLUS_APP_PATH := $(shell brew --prefix emacs-plus)
EMACS_MAC_PORT_APP_PATH := $(shell brew --prefix emacs-mac)

.PHONY: init
init: install-emacs-plus link compile install-icons setup-git-hook

.PHONY: install-emacs-plus
install-emacs-plus:
	brew tap d12frosted/emacs-plus
	brew install emacs-plus
	ln -sfv $(EMACS_MAC_PLUS_APP_PATH)/Emacs.app /Applications

.PHONY: install-emacs-mac
install-emacs-mac:
	brew tap railwaycat/emacsmacport
	brew install emacs-mac --with-ctags --with-glib --with-mac-metal --with-native-comp --with-natural-title-bar --with-librsvg --with-starter --with-xwidgets
	ln -sfv $(EMACS_MAC_PORT_APP_PATH)/Emacs.app /Applications
	# Adopting mac dark mode
	defaults write org.gnu.Emacs TransparentTitleBar DARK
	# Set No Title Bar Icon
	defaults write org.gnu.Emacs HideDocumentIcon YES

.PHONY: link
link:
	ln -nfs $(TOP_DIR) ~/

.PHONY: compile
compile:
	emacs -Q --batch -f batch-byte-compile early-init.el
	emacs -Q --batch -f batch-byte-compile init.el
	emacs -Q --batch -f batch-byte-compile lisp/functions/*.el
	emacs -Q --batch --eval "(progn (require 'package) (package-generate-autoloads \"my-functions\" \"~/.emacs.d/lisp/functions\"))"

.PHONY: install-icons
install-icons:
	emacs -Q --batch --eval '(progn (package-initialize) (all-the-icons-install-fonts t))'
	emacs -Q --batch --eval '(progn (package-initialize) (nerd-icons-install-fonts t))'

.PHONY: setup-git-hook
setup-git-hook:
	echo "make compile" > "${TOP_DIR}/.git/hooks/post-merge"
	chmod +x "${TOP_DIR}/.git/hooks/post-merge"
