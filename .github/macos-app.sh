#!/bin/sh
# SPDX-FileCopyrightText: Atlas Engineer LLC
# SPDX-License-Identifier: BSD-3-Clause

# Build the CEF Nyxt binary and pack it as a macOS application bundle.
#
#   Nyxt.app/Contents/Info.plist
#   Nyxt.app/Contents/MacOS/apprun
#   Nyxt.app/Contents/MacOS/nyxt
#   Nyxt.app/Contents/Resources/nyxt.core
#   Nyxt.app/Contents/MacOS/libcl-cef.dylib
#   Nyxt.app/Contents/MacOS/libcl-clipboard.dylib
#   Nyxt.app/Contents/Frameworks/                 Lisp shared libraries
#   Nyxt.app/Contents/Frameworks/Chromium Embedded Framework.framework
#   Nyxt.app/Contents/Frameworks/nyxt Helper.app
#   Nyxt.app/Contents/Frameworks/nyxt Helper (Alerts|GPU|Plugin|Renderer).app
#   Nyxt.app/Contents/Resources/                  pak files, locales, icon
#
# CEF names helper bundles after the main executable. apprun execs nyxt, so
# the helpers are "nyxt Helper".
#
# CEF_ROOT may point at an unpacked CEF binary distribution (the directory
# that contains include/, Release/, and cmake/). When unset, this downloads
# the newest CEF 150 macOS build. Linux is pinned to 150.0.22, which has no
# macOS archive; 150.0.21 is the matching macOS release.

set -eu

if [ "$(uname -s)" != "Darwin" ]; then
    printf '%s\n' "This script builds a macOS app bundle and must run on Darwin." >&2
    exit 1
fi

root=$(CDPATH= cd -- "$(dirname "$0")/.." && pwd)
cd "$root"

case $(uname -m) in
    arm64)
        cef_platform=macosarm64
        CEF_SHA1="${CEF_SHA1:-4057b6558bc83c93eb955b6983e00ef6361a0ffc}"
        ;;
    x86_64)
        cef_platform=macosx64
        CEF_SHA1="${CEF_SHA1:-6a76dc19c34b7858ba409dc2fa017cb9a2dcd27b}"
        ;;
    *)
        printf '%s\n' "Unsupported architecture $(uname -m)." >&2
        exit 1
        ;;
esac

CEF_VERSION="${CEF_VERSION:-150.0.21+ga61e9a5+chromium-150.0.7871.255}"
jobs=$(sysctl -n hw.ncpu 2>/dev/null || echo 2)
export CMAKE_BUILD_PARALLEL_LEVEL="$jobs"

work="$root/build/macos"
mkdir -p "$work"

if [ -z "${CEF_ROOT:-}" ]; then
    name="cef_binary_${CEF_VERSION}_${cef_platform}_minimal"
    archive="$work/${name}.tar.bz2"
    if [ ! -f "$archive" ]; then
        curl -fL --retry 3 -o "$archive" \
            "https://cef-builds.spotifycdn.com/${name}.tar.bz2"
    fi
    echo "$CEF_SHA1  $archive" | shasum -a 1 -c -
    tar -xjf "$archive" -C "$work"
    CEF_ROOT="$work/$name"
fi

framework_src="$CEF_ROOT/Release/Chromium Embedded Framework.framework/Chromium Embedded Framework"
if [ ! -f "$framework_src" ]; then
    printf '%s\n' "CEF framework is missing from $CEF_ROOT/Release." >&2
    exit 1
fi

vars="$CEF_ROOT/cmake/cef_variables.cmake"
if [ ! -f "$vars" ]; then
    printf '%s\n' "CEF CMake variables are missing from $vars." >&2
    exit 1
fi
if ! grep -q 'Chromium Embedded Framework.framework/Chromium Embedded Framework' "$vars"; then
    cat >> "$vars" <<'EOF'

if(OS_MAC)
  set(CEF_LIB_DEBUG "${CEF_BINARY_DIR_DEBUG}/Chromium Embedded Framework.framework/Chromium Embedded Framework")
  set(CEF_LIB_RELEASE "${CEF_BINARY_DIR_RELEASE}/Chromium Embedded Framework.framework/Chromium Embedded Framework")
endif()
EOF
fi

if [ -d /opt/local/bin ]; then
    PATH="/opt/local/bin:/opt/local/sbin:$PATH"
    export PATH
fi
if [ -d /opt/local/lib/pkgconfig ]; then
    pkg_config="/opt/local/lib/pkgconfig"
    if [ -d /opt/local/libexec/openssl3/lib/pkgconfig ]; then
        pkg_config="/opt/local/libexec/openssl3/lib/pkgconfig:$pkg_config"
    fi
    if [ -n "${PKG_CONFIG_PATH:-}" ]; then
        PKG_CONFIG_PATH="$pkg_config:$PKG_CONFIG_PATH"
    else
        PKG_CONFIG_PATH="$pkg_config"
    fi
    export PKG_CONFIG_PATH
    export DYLD_LIBRARY_PATH="/opt/local/lib${DYLD_LIBRARY_PATH:+:$DYLD_LIBRARY_PATH}"
fi

make NYXT_RENDERER=cef CEF_ROOT="$CEF_ROOT" NODE_SETUP=false nyxt

cef_rel=$(awk -F', ' '$1 == "cl-cef" { print $3; exit }' ocicl.csv)
cef_build="$root/ocicl/$(dirname "$cef_rel")/build"
clipboard_rel=$(awk -F', ' '$1 == "cl-clipboard" { print $3; exit }' ocicl.csv)
clipboard_dir="$root/ocicl/$(dirname "$clipboard_rel")"

for candidate in "$cef_build/libcl-cef.dylib" "$cef_build/Release/libcl-cef.dylib"; do
    if [ -f "$candidate" ]; then
        cl_cef=$candidate
        break
    fi
done
for candidate in "$cef_build/cl-cef-helper" "$cef_build/Release/cl-cef-helper"; do
    if [ -f "$candidate" ]; then
        helper_src=$candidate
        break
    fi
done
if [ -z "${cl_cef:-}" ] || [ -z "${helper_src:-}" ]; then
    printf '%s\n' "cl-cef build outputs are missing from $cef_build." >&2
    exit 1
fi
if [ ! -f "$clipboard_dir/libcl-clipboard.dylib" ]; then
    printf '%s\n' "libcl-clipboard.dylib is missing from $clipboard_dir." >&2
    exit 1
fi

resolve() {
    path=$1
    while [ -L "$path" ]; do
        dir=$(CDPATH= cd -- "$(dirname "$path")" && pwd)
        target=$(readlink "$path")
        case $target in
            /*) path=$target ;;
            *) path=$dir/$target ;;
        esac
    done
    dir=$(CDPATH= cd -- "$(dirname "$path")" && pwd)
    printf '%s\n' "$dir/$(basename "$path")"
}

macho_deps() {
    local file listing line
    file=$1
    listing=$(mktemp)
    otool -L "$file" | sed '1d' > "$listing"
    while IFS= read -r line; do
        line=${line#"${line%%[![:space:]]*}"}
        case $line in
            *" (compatibility version "*)
                printf '%s\n' "${line%% \(compatibility version *}"
                ;;
        esac
    done < "$listing"
    rm -f "$listing"
}

retarget_framework() {
    local binary new listing dep
    binary=$1
    new=$2
    if ! otool -L "$binary" | grep -q 'Chromium Embedded Framework'; then
        printf '%s\n' "CEF framework is not linked into $binary." >&2
        exit 1
    fi
    listing=$(mktemp)
    macho_deps "$binary" > "$listing"
    while IFS= read -r dep; do
        case $dep in
            *"Chromium Embedded Framework"*)
                install_name_tool -change "$dep" "$new" "$binary"
                break
                ;;
        esac
    done < "$listing"
    rm -f "$listing"
}

stage="$work/Nyxt.app"
rm -rf "$stage"
mkdir -p "$stage/Contents/MacOS" "$stage/Contents/Frameworks" "$stage/Contents/Resources"

framework="$stage/Contents/Frameworks/Chromium Embedded Framework.framework"
src_framework="$CEF_ROOT/Release/Chromium Embedded Framework.framework"
mkdir -p "$framework"
if [ -d "$src_framework/Versions/A" ]; then
    cp -R "$src_framework/." "$framework/"
else
    mkdir -p "$framework/Versions/A"
    cp -R "$src_framework/." "$framework/Versions/A/"
    ln -sfn "Versions/A/Chromium Embedded Framework" "$framework/Chromium Embedded Framework"
    ln -sfn "Versions/A/Libraries" "$framework/Libraries"
    ln -sfn "Versions/A/Resources" "$framework/Resources"
    ln -sfn "A" "$framework/Versions/Current"
fi
if [ -d "$framework/Resources" ]; then
    cp -R "$framework/Resources/." "$stage/Contents/Resources/"
fi
cp "$root/assets/nyxt.icns" "$stage/Contents/Resources/nyxt.icns"

helper_bin="$work/cl-cef-helper"
cp -f "$helper_src" "$helper_bin"
chmod 755 "$helper_bin"
retarget_framework "$helper_bin" \
    "@executable_path/../../../Chromium Embedded Framework.framework/Chromium Embedded Framework"

write_helper() {
    suffix_name=$1
    suffix_id=$2
    bundle_name="nyxt Helper${suffix_name}"
    app="$stage/Contents/Frameworks/${bundle_name}.app"
    mkdir -p "$app/Contents/MacOS"
    cp -f "$helper_bin" "$app/Contents/MacOS/$bundle_name"
    chmod 755 "$app/Contents/MacOS/$bundle_name"
    printf '%s' 'APPL????' > "$app/Contents/PkgInfo"
    cat > "$app/Contents/Info.plist" <<EOF
<?xml version="1.0" encoding="UTF-8"?>
<!DOCTYPE plist PUBLIC "-//Apple//DTD PLIST 1.0//EN" "http://www.apple.com/DTDs/PropertyList-1.0.dtd">
<plist version="1.0">
<dict>
  <key>CFBundleDevelopmentRegion</key>
  <string>en</string>
  <key>CFBundleDisplayName</key>
  <string>${bundle_name}</string>
  <key>CFBundleExecutable</key>
  <string>${bundle_name}</string>
  <key>CFBundleIdentifier</key>
  <string>engineer.Atlas.Nyxt.helper${suffix_id}</string>
  <key>CFBundleInfoDictionaryVersion</key>
  <string>6.0</string>
  <key>CFBundleName</key>
  <string>${bundle_name}</string>
  <key>CFBundlePackageType</key>
  <string>APPL</string>
  <key>CFBundleSignature</key>
  <string>????</string>
  <key>CFBundleVersion</key>
  <string>4.0.0</string>
  <key>CFBundleShortVersionString</key>
  <string>4.0.0</string>
  <key>LSEnvironment</key>
  <dict>
    <key>MallocNanoZone</key>
    <string>0</string>
  </dict>
  <key>LSFileQuarantineEnabled</key>
  <false/>
  <key>LSMinimumSystemVersion</key>
  <string>12.0</string>
  <key>LSUIElement</key>
  <true/>
  <key>NSSupportsAutomaticGraphicsSwitching</key>
  <true/>
</dict>
</plist>
EOF
}

write_helper "" ""
write_helper " (Alerts)" ".alerts"
write_helper " (GPU)" ".gpu"
write_helper " (Plugin)" ".plugin"
write_helper " (Renderer)" ".renderer"

cp -f "$cl_cef" "$stage/Contents/MacOS/libcl-cef.dylib"
chmod 755 "$stage/Contents/MacOS/libcl-cef.dylib"
retarget_framework "$stage/Contents/MacOS/libcl-cef.dylib" \
    "@loader_path/../Frameworks/Chromium Embedded Framework.framework/Chromium Embedded Framework"
cp -f "$clipboard_dir/libcl-clipboard.dylib" "$stage/Contents/MacOS/libcl-clipboard.dylib"
chmod 755 "$stage/Contents/MacOS/libcl-clipboard.dylib"
cp -f "$root/nyxt" "$stage/Contents/MacOS/nyxt"
chmod 755 "$stage/Contents/MacOS/nyxt"
dynamic_mb=$(python3 - "$stage/Contents/MacOS/nyxt" "$stage/Contents/Resources/nyxt.core" <<'PY'
import os, struct, sys
path, core_path = sys.argv[1], sys.argv[2]
with open(path, "rb") as src:
    data = src.read()
size = len(data)
if size < 32:
    sys.exit("executable is too small to contain an SBCL core")
magic_macho = struct.unpack_from("<I", data, 0)[0]
if magic_macho != 0xFEEDFACF:
    sys.exit("executable is not a thin 64-bit Mach-O")
ncmds = struct.unpack_from("<I", data, 16)[0]
command = 32
macho_end = 0
for _ in range(ncmds):
    cmd, cmdsize = struct.unpack_from("<II", data, command)
    if cmd == 0x19:
        fileoff, filesize = struct.unpack_from("<QQ", data, command + 40)
        macho_end = max(macho_end, fileoff + filesize)
    command += cmdsize
offset, core_magic = struct.unpack_from("<QQ", data, size - 16)
if offset <= 0 or offset >= size - 16:
    sys.exit("executable has no embedded SBCL core")
if struct.unpack_from("<Q", data, offset)[0] != core_magic:
    sys.exit("embedded SBCL core magic does not match")
if macho_end <= 0 or macho_end > offset:
    sys.exit("Mach-O layout does not leave the SBCL core outside the segments")
words = struct.unpack_from("<5Q", data, offset + 8)
mb = 3072
if words[0] == 0x31EBF355:
    space = words[2]
    if space % (1024 * 1024) == 0:
        parsed = space // (1024 * 1024)
        if 512 <= parsed <= 8192:
            mb = parsed
with open(core_path, "wb") as dst:
    dst.write(data[offset:])
os.truncate(path, macho_end)
print(mb)
PY
)

frameworks="$stage/Contents/Frameworks"

stage_library() {
    local src real base alias listing dep neighbor
    src=$1
    if [ ! -e "$src" ]; then
        return 0
    fi
    real=$(resolve "$src")
    case $real in
        /usr/lib/*|/System/*) return 0 ;;
    esac
    base=$(basename "$real")
    alias=$(basename "$src")
    if [ -f "$frameworks/$base" ]; then
        if [ "$alias" != "$base" ] && [ ! -e "$frameworks/$alias" ]; then
            ln -sfn "$base" "$frameworks/$alias"
        fi
        return 0
    fi
    cp -f "$real" "$frameworks/$base"
    chmod 755 "$frameworks/$base"
    if [ "$alias" != "$base" ]; then
        ln -sfn "$base" "$frameworks/$alias"
    fi
    listing=$(mktemp)
    macho_deps "$frameworks/$base" > "$listing"
    while IFS= read -r dep; do
        case $dep in
            ""|/usr/lib/*|/System/*) continue ;;
            @loader_path/*)
                stage_library "$(dirname "$real")/${dep#@loader_path/}"
                ;;
            @rpath/*)
                neighbor="$(dirname "$real")/${dep#@rpath/}"
                if [ -e "$neighbor" ]; then
                    stage_library "$neighbor"
                fi
                ;;
            @executable_path/*) ;;
            *)
                if [ -e "$dep" ]; then
                    stage_library "$dep"
                fi
                ;;
        esac
    done < "$listing"
    rm -f "$listing"
}

stage_named() {
    dir=$1
    shift
    for name in "$@"; do
        if [ -e "$dir/$name" ]; then
            stage_library "$dir/$name"
            return 0
        fi
    done
    printf '%s\n' "Missing shared library in $dir." >&2
    return 1
}

if [ ! -d /opt/local/lib ]; then
    printf '%s\n' "MacPorts is required to locate OpenSSL, Enchant, and libfixposix." >&2
    exit 1
fi
openssl_lib=/opt/local/libexec/openssl3/lib
if [ ! -d "$openssl_lib" ]; then
    openssl_lib=/opt/local/lib/openssl3
fi
stage_named "$openssl_lib" libssl.dylib libssl.3.dylib
stage_named "$openssl_lib" libcrypto.dylib libcrypto.3.dylib
stage_named /opt/local/lib libfixposix.dylib libfixposix.4.dylib
stage_named /opt/local/lib libenchant-2.dylib libenchant-2.2.dylib

rewrite_staged() {
    local dest base listing dep depbase
    for dest in "$frameworks"/*.dylib; do
        if [ ! -f "$dest" ] || [ -L "$dest" ]; then
            continue
        fi
        base=$(basename "$dest")
        install_name_tool -id "@loader_path/$base" "$dest"
        listing=$(mktemp)
        macho_deps "$dest" > "$listing"
        while IFS= read -r dep; do
            case $dep in
                ""|/usr/lib/*|/System/*|@loader_path/*|@executable_path/*) continue ;;
            esac
            depbase=$(basename "$dep")
            if [ -e "$frameworks/$depbase" ]; then
                install_name_tool -change "$dep" "@loader_path/$depbase" "$dest"
            fi
        done < "$listing"
        rm -f "$listing"
    done
}
rewrite_staged

cat > "$work/apprun.c" <<'EOF'
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>
#include <mach-o/dyld.h>

int main(int argc, char **argv) {
    char exe[4096];
    char nyxt[4096];
    char core[4096];
    char frameworks[4096];
    char libcef[4096];
    char libclip[4096];
    char *slash;
    char **args;
    uint32_t size;
    int i;
    int n;

    size = (uint32_t)sizeof exe;
    if (_NSGetExecutablePath(exe, &size) != 0)
        return 1;
    slash = strrchr(exe, '/');
    if (slash == NULL)
        return 1;
    *slash = '\0';
    snprintf(nyxt, sizeof nyxt, "%s/nyxt", exe);
    snprintf(core, sizeof core, "%s/../Resources/nyxt.core", exe);
    snprintf(frameworks, sizeof frameworks, "%s/../Frameworks", exe);
    snprintf(libcef, sizeof libcef, "%s/libcl-cef.dylib", exe);
    snprintf(libclip, sizeof libclip, "%s/libcl-clipboard.dylib", exe);
    setenv("DYLD_LIBRARY_PATH", frameworks, 1);
    setenv("DYLD_FALLBACK_LIBRARY_PATH", frameworks, 1);
    setenv("CL_CEF_LIBRARY", libcef, 1);
    setenv("CL_CLIPBOARD_LIBRARY", libclip, 1);
    args = calloc((size_t)argc + 5, sizeof *args);
    if (args == NULL)
        return 1;
    n = 0;
    args[n++] = nyxt;
    args[n++] = "--dynamic-space-size";
    args[n++] = NYXT_DYNAMIC_MB_STR;
    args[n++] = "--core";
    args[n++] = core;
    for (i = 1; i < argc; i++)
        args[n++] = argv[i];
    args[n] = NULL;
    execv(nyxt, args);
    perror("nyxt");
    return 1;
}
EOF
clang -O2 -DNYXT_DYNAMIC_MB_STR=\"$dynamic_mb\" -o "$stage/Contents/MacOS/apprun" "$work/apprun.c"
chmod 755 "$stage/Contents/MacOS/apprun"
printf '%s' 'APPL????' > "$stage/Contents/PkgInfo"

version=$(awk '/^  :version / { gsub(/"/, "", $2); print $2; exit }' nyxt.asd)
case $version in
    *.*.*) ;;
    *.*) version=$version.0 ;;
    "") version=4.0.0 ;;
    *) version=$version.0.0 ;;
esac

cat > "$stage/Contents/Info.plist" <<EOF
<?xml version="1.0" encoding="UTF-8"?>
<!DOCTYPE plist PUBLIC "-//Apple//DTD PLIST 1.0//EN" "http://www.apple.com/DTDs/PropertyList-1.0.dtd">
<plist version="1.0">
<dict>
  <key>CFBundleDevelopmentRegion</key>
  <string>en</string>
  <key>CFBundleExecutable</key>
  <string>apprun</string>
  <key>CFBundleIconFile</key>
  <string>nyxt.icns</string>
  <key>CFBundleIdentifier</key>
  <string>engineer.Atlas.Nyxt</string>
  <key>CFBundleInfoDictionaryVersion</key>
  <string>6.0</string>
  <key>CFBundleName</key>
  <string>Nyxt</string>
  <key>CFBundleDisplayName</key>
  <string>Nyxt</string>
  <key>CFBundlePackageType</key>
  <string>APPL</string>
  <key>CFBundleSignature</key>
  <string>????</string>
  <key>CFBundleShortVersionString</key>
  <string>${version}</string>
  <key>CFBundleVersion</key>
  <string>${version}</string>
  <key>LSEnvironment</key>
  <dict>
    <key>MallocNanoZone</key>
    <string>0</string>
  </dict>
  <key>LSMinimumSystemVersion</key>
  <string>12.0</string>
  <key>NSHighResolutionCapable</key>
  <true/>
  <key>NSPrincipalClass</key>
  <string>NSApplication</string>
  <key>NSSupportsAutomaticGraphicsSwitching</key>
  <true/>
  <key>NSAppTransportSecurity</key>
  <dict>
    <key>NSAllowsLocalNetworking</key>
    <true/>
  </dict>
  <key>CFBundleURLTypes</key>
  <array>
    <dict>
      <key>CFBundleURLName</key>
      <string>http URL</string>
      <key>CFBundleURLSchemes</key>
      <array>
        <string>http</string>
      </array>
    </dict>
    <dict>
      <key>CFBundleURLName</key>
      <string>https URL</string>
      <key>CFBundleURLSchemes</key>
      <array>
        <string>https</string>
      </array>
    </dict>
    <dict>
      <key>CFBundleURLName</key>
      <string>gopher URL</string>
      <key>CFBundleURLSchemes</key>
      <array>
        <string>gopher</string>
      </array>
    </dict>
    <dict>
      <key>CFBundleURLName</key>
      <string>gemini URL</string>
      <key>CFBundleURLSchemes</key>
      <array>
        <string>gemini</string>
      </array>
    </dict>
  </array>
  <key>CFBundleDocumentTypes</key>
  <array>
    <dict>
      <key>CFBundleTypeExtensions</key>
      <array>
        <string>html</string>
        <string>xhtml</string>
      </array>
    </dict>
  </array>
</dict>
</plist>
EOF

sign() {
    codesign --force --sign - --timestamp=none "$1"
}

sign_machos() {
    local root_dir listing candidate
    root_dir=$1
    listing=$(mktemp)
    find "$root_dir" -type f > "$listing"
    while IFS= read -r candidate; do
        case $candidate in
            "$stage/Contents/MacOS/apprun"|*.core) continue ;;
        esac
        if file "$candidate" | grep -q Mach-O; then
            sign "$candidate"
        fi
    done < "$listing"
    rm -f "$listing"
}

xattr -cr "$stage"
sign_machos "$stage"
sign "$framework"
for helper in "$stage/Contents/Frameworks/"*.app; do
    sign "$helper"
done
codesign --force --deep --sign - --timestamp=none "$stage"

case $(uname -m) in
    arm64) archive_arch=arm64 ;;
    x86_64) archive_arch=x86_64 ;;
esac
output="$work/Nyxt-${archive_arch}.app.tar.gz"
rm -f "$output"
COPYFILE_DISABLE=1 tar -C "$work" -czf "$output" Nyxt.app
printf '%s\n' "Wrote $output"
