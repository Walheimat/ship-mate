PACKAGE_NAME=ship-mate
PACKAGE_SUFFIX=tar
CURRENT_PACKAGE_VERSION=0.7.0

UPDATE_VERSION_FILES=Cask \
					 ship-mate.el \
					 ship-mate-dinghy.el \
					 ship-mate-edit.el \
					 ship-mate-submarine.el \
					 Makefile

include dinghy/emacs-package.mk
