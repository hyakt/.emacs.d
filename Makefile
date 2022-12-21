TOP_DIR := $(realpath $(dir $(lastword $(MAKEFILE_LIST))))

.PHONY: init
init: install-mac-port install-fonts compile link

.PHONY: link
link:
	ln -nfs $(TOP_DIR) ~/

.PHONY: compile
compile:
	emacs -Q --batch -f batch-byte-compile early-init.el
	emacs -Q --batch -f batch-byte-compile init.el

.PHONY: install-mac-port
install-mac-port:
	brew tap railwaycat/emacsmacport
	brew install emacs-mac --with-ctags --with-glib --with-mac-metal --with-native-comp --with-natural-title-bar --with-librsvg --with-starter --with-xwidgets
	ln -sfv /usr/local/opt/emacs-mac/Emacs.app /Applications/
	# Adopting mac dark mode
	defaults write org.gnu.Emacs TransparentTitleBar DARK
	# Set No Title Bar Icon
	defaults write org.gnu.Emacs HideDocumentIcon YES

.PHONY: install-fonts
install-fonts:
	brew tap homebrew/cask-fonts
	brew tap iandol/adobe-fonts
	brew install --cask font-source-han-code-jp font-myrica
