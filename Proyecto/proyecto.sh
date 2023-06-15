#!/bin/bash

filename=$(basename -- "$1")
extension="${filename##*.}"
filename="${filename%.*}"

# Convierte la primera letra a mayúscula
filename_capitalized=$(echo $filename | sed -e "s/\b\(.\)/\u\1/g")

./proyecto.rkt $1 $filename_capitalized > "${filename_capitalized}.java"

javac "${filename_capitalized}.java"