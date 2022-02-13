#!/bin/sh

SCRIPT_DIR="$( cd "$( dirname "$BASH_SOURCE[0]" )" && pwd )"

symlinkDir() {
    filename="$SCRIPT_DIR/$1"
    destination="$HOME/$2/$1"

    mkdir -p $(dirname "$destination")
    
    if [ ! -L "$destination" ]; then
        if [ -e "$destination" ]; then
            echo "[ERROR] $destination exists but it's not a symlink. Please fix that manually" && exit 1
        else
            ln -s "$filename" "$destination"
            echo "[OK] $filename -> $destination"
        fi
    else
        echo "[WARNING] $filename already symlinked"
    fi
}

symlinkFile() {
    filepath="$SCRIPT_DIR/$1"
    filename="$(basename -- $1)"
    destination="$HOME/$2"

    mkdir -p $(dirname "$destination")
    
    if [ ! -L "$destination/$filename" ]; then
        if [ -e "$destination/$filename" ]; then
            echo "[ERROR] $destination exists but it's not a symlink. Please fix that manually" && exit 1
        else
            ln -s "$filepath" "$destination/$filename"
            echo "[OK] $filepath -> $destination/$filename"
        fi
    else
        echo "[WARNING] $filepath already symlinked"
    fi
}

deployManifest() {
    for row in $(cat $SCRIPT_DIR/$1); do
        filename=$(echo $row | cut -d \| -f 1)
        operation=$(echo $row | cut -d \| -f 2)
        destination=$(echo $row | cut -d \| -f 3)

        case $operation in
            symlinkdir)
                symlinkDir $filename $destination
                ;;
            symlinkfile)
                symlinkFile $filename $destination
                ;;

            *)
                echo "[WARNING] Unknown operation $operation. Skipping..."
                ;;
        esac
    done
}
