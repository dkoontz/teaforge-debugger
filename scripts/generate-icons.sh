#!/bin/bash

# Generate platform-specific icons from source PNG
# Requires ImageMagick (brew install imagemagick) and png2icns (part of libicns, brew install libicns)

set -e

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
PROJECT_ROOT="$(dirname "$SCRIPT_DIR")"
SOURCE_ICON="$PROJECT_ROOT/docs/teaforge-icon.png"
BUILD_DIR="$PROJECT_ROOT/build"

# Detect ImageMagick command (v7 uses 'magick', v6 uses 'convert')
if command -v magick &> /dev/null; then
    MAGICK="magick"
elif command -v convert &> /dev/null; then
    MAGICK="convert"
else
    MAGICK=""
fi

if [ ! -f "$SOURCE_ICON" ]; then
    echo "Error: Source icon not found at $SOURCE_ICON"
    exit 1
fi

echo "Generating icons from $SOURCE_ICON..."

# Copy source to build directory
cp "$SOURCE_ICON" "$BUILD_DIR/icon.png"
echo "Copied icon.png"

# Generate Windows .ico (requires ImageMagick)
if [ -n "$MAGICK" ]; then
    $MAGICK "$SOURCE_ICON" -define icon:auto-resize=256,128,64,48,32,16 "$BUILD_DIR/icon.ico"
    echo "Generated icon.ico"
else
    echo "Warning: ImageMagick not found, skipping icon.ico generation"
fi

# Generate macOS .icns
if command -v iconutil &> /dev/null && [ -n "$MAGICK" ]; then
    # Use ImageMagick + iconutil
    ICONSET_DIR="$BUILD_DIR/icon.iconset"
    mkdir -p "$ICONSET_DIR"

    for size in 16 32 64 128 256 512; do
        $MAGICK "$SOURCE_ICON" -resize ${size}x${size} "$ICONSET_DIR/icon_${size}x${size}.png"
        double=$((size * 2))
        if [ $double -le 1024 ]; then
            $MAGICK "$SOURCE_ICON" -resize ${double}x${double} "$ICONSET_DIR/icon_${size}x${size}@2x.png"
        fi
    done

    iconutil -c icns "$ICONSET_DIR" -o "$BUILD_DIR/icon.icns"
    echo "Generated icon.icns"
    rm -rf "$ICONSET_DIR"
else
    echo "Warning: iconutil or ImageMagick not available, skipping icon.icns"
fi

# Generate Linux icons (multiple sizes in build/icons/)
ICONS_DIR="$BUILD_DIR/icons"
mkdir -p "$ICONS_DIR"

if [ -n "$MAGICK" ]; then
    for size in 16 24 32 48 64 128 256 512; do
        $MAGICK "$SOURCE_ICON" -resize ${size}x${size} "$ICONS_DIR/${size}x${size}.png"
    done
    echo "Generated Linux icons"
else
    echo "Warning: ImageMagick not found, skipping Linux icon generation"
    # Fall back to copying source as 256x256
    cp "$SOURCE_ICON" "$ICONS_DIR/256x256.png"
fi

echo "Icon generation complete!"
