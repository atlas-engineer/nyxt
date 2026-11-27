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
# that contains include/, Release/, and cmake/). When unset, x86_64 downloads
# the CEF 150 build from the atlas-engineer/cef release. aarch64 downloads
# that same version from cef-builds.spotifycdn.com. The upstream aarch64
# archive does not include the Atlas CEF patches.

set -eu

root=$(CDPATH= cd -- "$(dirname "$0")/.." && pwd)
cd "$root"

CEF_VERSION="${CEF_VERSION:-150.0.22+gc1a097e+chromium-150.0.7871.256}"
case $(uname -m) in
    x86_64)
        cef_platform=linux64
        appimage_arch=x86_64
        ld_arch=x86-64
        multiarch=x86_64-linux-gnu
        CEF_SHA1="${CEF_SHA1:-00f5a3278faa74570026f34216087248831078ba}"
        ;;
    aarch64|arm64)
        cef_platform=linuxarm64
        appimage_arch=aarch64
        ld_arch=AArch64
        multiarch=aarch64-linux-gnu
        CEF_SHA1="${CEF_SHA1:-dca12dae6fb104fef4c614e990eaeb1c0a6e263f}"
        ;;
    *)
        printf '%s\n' "Unsupported architecture $(uname -m)." >&2
        exit 1
        ;;
esac
jobs=$(nproc 2>/dev/null || echo 2)
export CMAKE_BUILD_PARALLEL_LEVEL="$jobs"

if [ -z "${CEF_ROOT:-}" ]; then
    name="cef_binary_${CEF_VERSION}_${cef_platform}_minimal"
    archive="$root/build/appimage/${name}.tar.bz2"
    mkdir -p "$root/build/appimage"
    if [ ! -f "$archive" ]; then
        file=$(printf '%s' "${name}.tar.bz2" | sed 's/+/%2B/g')
        if [ "$cef_platform" = "linux64" ]; then
            tag=$(printf '%s' "$CEF_VERSION" | sed 's/+/%2B/g')
            url="https://github.com/atlas-engineer/cef/releases/download/${tag}/${file}"
        else
            url="https://cef-builds.spotifycdn.com/${file}"
        fi
        curl -fL --retry 3 -o "$archive" "$url"
    fi
    echo "$CEF_SHA1  $archive" | sha1sum -c -
    tar -xjf "$archive" -C "$root/build/appimage"
    CEF_ROOT="$root/build/appimage/$name"
fi

make NYXT_RENDERER=cef CEF_ROOT="$CEF_ROOT" nyxt

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
ldconfig_bin() {
    if command -v ldconfig >/dev/null 2>&1; then
        command -v ldconfig
        return 0
    fi
    for candidate in /sbin/ldconfig /usr/sbin/ldconfig; do
        if [ -x "$candidate" ]; then
            printf '%s\n' "$candidate"
            return 0
        fi
    done
    return 1
}

elf_class() {
    od -An -t u1 -j 4 -N 1 "$1" | tr -d ' \n'
}

find_lib() {
    name=$1
    found=""
    if ldconfig=$(ldconfig_bin); then
        found=$("$ldconfig" -p | awk -v name="$name" -v arch="$ld_arch" '
            $1 == name && index($0, arch) && index($0, "hwcap") == 0 {
                print $NF
                exit
            }')
        if [ -z "$found" ]; then
            found=$("$ldconfig" -p | awk -v name="$name" -v arch="$ld_arch" '
                $1 == name && index($0, arch) {
                    print $NF
                    exit
                }')
        fi
    fi
    if [ -z "$found" ]; then
        for dir in /lib64 /usr/lib64 \
            "/lib/${multiarch}" "/usr/lib/${multiarch}" \
            /lib /usr/lib
        do
            if [ -e "$dir/$name" ] && [ "$(elf_class "$dir/$name")" = "2" ]; then
                found=$dir/$name
                break
            fi
        done
    fi
    if [ -z "$found" ]; then
        printf '%s\n' "Could not find a 64-bit shared library $name" >&2
        return 1
    fi
    printf '%s\n' "$found"
}

install_lib() {
    name=$1
    path=$(find_lib "$name")
    real=$(readlink -f "$path")
    if [ "$(elf_class "$real")" != "2" ]; then
        printf '%s\n' "Refusing to ship $name from $real: not a 64-bit ELF." >&2
        return 1
    fi
    base=$(basename "$real")
    cp -f "$real" "$stage/usr/lib/$base"
    if [ "$base" != "$name" ]; then
        ln -sfn "$base" "$stage/usr/lib/$name"
    fi
}

install_lib libssl.so.3
install_lib libcrypto.so.3
install_lib libsqlite3.so.0

ca_bundle=""
for bundle in \
    /etc/ssl/certs/ca-certificates.crt \
    /etc/pki/ca-trust/extracted/pem/tls-ca-bundle.pem \
    /etc/pki/tls/certs/ca-bundle.crt \
    /etc/ssl/ca-bundle.pem \
    /etc/ssl/cert.pem
do
    if [ -f "$bundle" ]; then
        ca_bundle=$bundle
        break
    fi
done
if [ -z "$ca_bundle" ]; then
    printf '%s\n' "Could not find a CA bundle to ship with the AppImage." >&2
    exit 1
fi
mkdir -p "$stage/usr/ssl"
cp -f "$ca_bundle" "$stage/usr/ssl/cert.pem"
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
if [ -z "${SSL_CERT_FILE:-}" ]; then
    for bundle in \
        /etc/ssl/certs/ca-certificates.crt \
        /etc/pki/ca-trust/extracted/pem/tls-ca-bundle.pem \
        /etc/pki/tls/certs/ca-bundle.crt \
        /etc/ssl/ca-bundle.pem \
        /etc/ssl/cert.pem \
        "$here/usr/ssl/cert.pem"
    do
        if [ -f "$bundle" ]; then
            export SSL_CERT_FILE="$bundle"
            break
        fi
    done
fi
exec "$APPDIR/usr/bin/nyxt" "$@"
EOF
chmod 755 "$stage/AppRun"

tool="$root/build/appimage/appimagetool-${appimage_arch}.AppImage"
if [ ! -x "$tool" ]; then
    curl -fL --retry 3 -o "$tool" \
        "https://github.com/AppImage/appimagetool/releases/download/continuous/appimagetool-${appimage_arch}.AppImage"
    chmod 755 "$tool"
fi

output="$root/build/appimage/Nyxt-${appimage_arch}.AppImage"
rm -f "$output"
ARCH="$appimage_arch" NO_APPSTREAM=1 "$tool" --appimage-extract-and-run "$stage" "$output"
chmod 755 "$output"
printf '%s\n' "Wrote $output"
