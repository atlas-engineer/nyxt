#!/bin/sh
# SPDX-FileCopyrightText: Atlas Engineer LLC
# SPDX-License-Identifier: BSD-3-Clause

# Build the CEF Nyxt binary and pack it as an AppImage.
#
# The image layout follows the earlier AppDir:
#
#   AppDir/AppRun
#   AppDir/nyxt.desktop
#   AppDir/nyxt.png
#   AppDir/usr/bin/nyxt
#   AppDir/usr/bin/libcl-clipboard.so
#   AppDir/usr/lib/                 Lisp shared libraries
#   AppDir/usr/lib/cef/             libcl-cef, the helper, and CEF resources
#
# CEF_ROOT may point at an unpacked CEF binary distribution (the directory
# that contains include/, Release/, and cmake/). When unset, this downloads
# the CEF 150 build Nyxt is developed against.

set -eu

root=$(CDPATH= cd -- "$(dirname "$0")/.." && pwd)
cd "$root"

CEF_VERSION="${CEF_VERSION:-150.0.22+gc1a097e+chromium-150.0.7871.256}"
CEF_SHA1="${CEF_SHA1:-7e7ca1e73e5d5e6317ca53dc1302fd180a3e1222}"
jobs=$(nproc 2>/dev/null || echo 2)
export CMAKE_BUILD_PARALLEL_LEVEL="$jobs"

if [ -z "${CEF_ROOT:-}" ]; then
    name="cef_binary_${CEF_VERSION}_linux64_minimal"
    archive="$root/build/appimage/${name}.tar.bz2"
    mkdir -p "$root/build/appimage"
    if [ ! -f "$archive" ]; then
        curl -fL --retry 3 -o "$archive" \
            "https://cef-builds.spotifycdn.com/${name}.tar.bz2"
    fi
    echo "$CEF_SHA1  $archive" | sha1sum -c -
    tar -xjf "$archive" -C "$root/build/appimage"
    CEF_ROOT="$root/build/appimage/$name"
fi

make NYXT_RENDERER=cef CEF_ROOT="$CEF_ROOT" NODE_SETUP=false nyxt

stage="$root/build/appimage/AppDir"
rm -rf "$stage"
mkdir -p "$stage/usr/bin" "$stage/usr/lib/cef"

cef_rel=$(awk -F', ' '$1 == "cl-cef" { print $3; exit }' ocicl.csv)
cef_build="$root/ocicl/$(dirname "$cef_rel")/build"
clipboard_rel=$(awk -F', ' '$1 == "cl-clipboard" { print $3; exit }' ocicl.csv)
clipboard_dir="$root/ocicl/$(dirname "$clipboard_rel")"

install -m 755 "$root/nyxt" "$stage/usr/bin/nyxt"
if [ ! -f "$clipboard_dir/libcl-clipboard.so" ]; then
    printf '%s\n' "libcl-clipboard.so is missing from $clipboard_dir" >&2
    exit 1
fi
install -m 755 "$clipboard_dir/libcl-clipboard.so" "$stage/usr/bin/libcl-clipboard.so"

for name in \
    libcl-cef.so \
    cl-cef-helper \
    chrome-sandbox \
    libcef.so \
    libEGL.so \
    libGLESv2.so \
    libvk_swiftshader.so \
    libvulkan.so.1 \
    v8_context_snapshot.bin \
    vk_swiftshader_icd.json \
    chrome_100_percent.pak \
    chrome_200_percent.pak \
    resources.pak \
    icudtl.dat
do
    if [ ! -f "$cef_build/$name" ]; then
        printf '%s\n' "CEF runtime file $name is missing from $cef_build" >&2
        exit 1
    fi
    cp -f "$cef_build/$name" "$stage/usr/lib/cef/$name"
done
chmod 755 "$stage/usr/lib/cef/cl-cef-helper" "$stage/usr/lib/cef/chrome-sandbox"
cp -a "$cef_build/locales" "$stage/usr/lib/cef/locales"

# CFFI loads these by file name. Copy the real object and the name it asks for.
# Leave glibc, GTK, and the rest of the desktop stack on the host: libcef.so
# is linked against those, and a second copy on LD_LIBRARY_PATH breaks it.
find_lib() {
    name=$1
    found=$(ldconfig -p | awk -v name="$name" '$1 == name { print $NF; exit }')
    if [ -n "$found" ]; then
        printf '%s\n' "$found"
        return 0
    fi
    for dir in /lib /usr/lib /lib64 /usr/lib64 \
        /lib/x86_64-linux-gnu /usr/lib/x86_64-linux-gnu
    do
        if [ -e "$dir/$name" ]; then
            printf '%s\n' "$dir/$name"
            return 0
        fi
    done
    printf '%s\n' "Could not find shared library $name" >&2
    return 1
}

install_lib() {
    name=$1
    path=$(find_lib "$name")
    real=$(readlink -f "$path")
    base=$(basename "$real")
    cp -f "$real" "$stage/usr/lib/$base"
    if [ "$base" != "$name" ]; then
        ln -sfn "$base" "$stage/usr/lib/$name"
    fi
}

install_lib libssl.so.3
install_lib libcrypto.so.3
install_lib libsqlite3.so.0
install_lib libfixposix.so
install_lib libenchant-2.so

cp "$root/assets/nyxt.desktop" "$stage/nyxt.desktop"
chmod 755 "$stage/nyxt.desktop"
cp "$root/assets/icons/nyxt_256x256.png" "$stage/nyxt.png"
cp "$root/assets/icons/nyxt_256x256.png" "$stage/.DirIcon"

cat > "$stage/AppRun" <<'EOF'
#!/bin/sh
set -eu
here=$(CDPATH= cd -- "$(dirname "$0")" && pwd)
export APPDIR="${APPDIR:-$here}"
if [ -n "${ARGV0:-}" ]; then
    export APPIMAGE_PATH=$(readlink -f "$ARGV0")
elif [ -n "${APPIMAGE:-}" ]; then
    export APPIMAGE_PATH=$(readlink -f "$APPIMAGE")
else
    export APPIMAGE_PATH=$APPDIR
fi
export LD_LIBRARY_PATH="$APPDIR/usr/lib${LD_LIBRARY_PATH:+:$LD_LIBRARY_PATH}"
export CL_CEF_LIBRARY="$APPDIR/usr/lib/cef/libcl-cef.so"
export CL_CLIPBOARD_LIBRARY="$APPDIR/usr/bin/libcl-clipboard.so"
exec "$APPDIR/usr/bin/nyxt" "$@"
EOF
chmod 755 "$stage/AppRun"

tool="$root/build/appimage/appimagetool-x86_64.AppImage"
if [ ! -x "$tool" ]; then
    curl -fL --retry 3 -o "$tool" \
        "https://github.com/AppImage/appimagetool/releases/download/continuous/appimagetool-x86_64.AppImage"
    chmod 755 "$tool"
fi

output="$root/build/appimage/Nyxt-x86_64.AppImage"
rm -f "$output"
ARCH=x86_64 NO_APPSTREAM=1 "$tool" --appimage-extract-and-run "$stage" "$output"
chmod 755 "$output"
printf '%s\n' "Wrote $output"
