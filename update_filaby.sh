#!/bin/bash
set -e  # Bei Fehlern stoppen

# Zielverzeichnis
TARGET_DIR="$HOME/Programme/filaby"

# Verzeichnis ggf. anlegen
mkdir -p "$TARGET_DIR"

# 1. Repository klonen oder aktualisieren
if [ -d "$TARGET_DIR/.git" ]; then
    echo "Repository updaten..."
    cd "$TARGET_DIR"
    git fetch origin
    git reset --hard origin/main
else
    echo "Repository klonen..."
    mkdir -p "$TARGET_DIR"
    git clone https://github.com/Filasez/filaby.git "$TARGET_DIR"
    cd "$TARGET_DIR"
fi

# 2. Build durchführen
echo "Build starten..."
make clean
make

# 3. Installieren
echo "Installieren..."
make install

echo "Update abgeschlossen!"
