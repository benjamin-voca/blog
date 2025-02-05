#!/usr/bin/env bash

# Check if a file is provided as an argument
if [ -z "$1" ]; then
  echo "Usage: $0 <filename>"
  exit 1
fi

# Assign the filename to a variable
filename="$1"

# Check if the file exists
if [ ! -f "$filename" ]; then
  echo "File not found: $filename"
  exit 1
fi

# Replace opacity="0.7" with opacity="0.0"
sed -i 's/opacity="0.7"/opacity="0.0"/g' "$filename"

# Delete everything within the <defs> tag (including the tag itself)
sed -i ':a;N;$!ba;s/<defs>.*<\/defs>//g' "$filename"

echo "All occurrences of opacity=\"0.7\" have been replaced with opacity=\"0.0\", and all content within <defs> tags has been deleted in $filename."
