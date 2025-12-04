.PHONY: default all clean

include project.conf
PROJECT_ARCHIVE=$(PROJECT_NAME)-$(PROJECT_VERSION)

DESTDIR ?=
PREFIX ?= $(HOME)/.local
BINDIR ?= $(PREFIX)/bin
DATADIR ?= $(PREFIX)/share

# Interner Binary-Name bleibt "laby"
INTERNAL_BIN=laby
# Installierter Name wird "filaby"
INSTALLED_BIN=filaby

default: all

all:
	@./build --all

clean:
	@./build --clean

byte native byte-debug native-profile:
	@./build --$@

install:
	# Binary installieren unter neuem Namen
	install -Dp --mode=0755 $(INTERNAL_BIN) \
		"$(DESTDIR)$(BINDIR)/$(INSTALLED_BIN)"

	# Daten installieren
	install -d "$(DESTDIR)$(DATADIR)/$(INSTALLED_BIN)/"
	cp -pr data/* "$(DESTDIR)$(DATADIR)/$(INSTALLED_BIN)/"

	# Icon installieren unter neuem Namen
	install -Dp --mode=0644 data/tiles/ant_f-e.svg \
		"$(DESTDIR)$(DATADIR)/icons/hicolor/scalable/apps/$(INSTALLED_BIN).svg"

	# Desktop-Datei dynamisch erzeugen
	echo "[Desktop Entry]" > packaging/$(INSTALLED_BIN).desktop
	echo "Name=Filaby" >> packaging/$(INSTALLED_BIN).desktop
	echo "Comment=Startet das Filaby-Spiel" >> packaging/$(INSTALLED_BIN).desktop
	echo "Exec=$(BINDIR)/$(INSTALLED_BIN)" >> packaging/$(INSTALLED_BIN).desktop
	echo "Icon=$(DATADIR)/icons/hicolor/scalable/apps/$(INSTALLED_BIN).svg" \
    >> packaging/$(INSTALLED_BIN).desktop
	echo "Terminal=false" >> packaging/$(INSTALLED_BIN).desktop
	echo "Type=Application" >> packaging/$(INSTALLED_BIN).desktop
	echo "Categories=Game;" >> packaging/$(INSTALLED_BIN).desktop

	# Desktop-Datei installieren unter neuem Namen
	desktop-file-install packaging/$(INSTALLED_BIN).desktop \
		--dir="$(DESTDIR)$(DATADIR)/applications"
	

	# AppData installieren
	install -Dp --mode=0644 packaging/$(INSTALLED_BIN).appdata.xml \
		"$(DESTDIR)$(DATADIR)/appdata/$(INSTALLED_BIN).appdata.xml"

dist:
	@mkdir _dist
	@git archive --prefix="$(PROJECT_ARCHIVE)/" HEAD \
		 | gzip >"_dist/$(PROJECT_ARCHIVE).tar.gz"
	@echo archive stored in "_dist/$(PROJECT_ARCHIVE).tar.gz"
