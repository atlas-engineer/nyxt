#!/bin/sh
# SPDX-FileCopyrightText: Atlas Engineer LLC
# SPDX-License-Identifier: BSD-3-Clause

set -eu

root=$(CDPATH= cd -- "$(dirname "$0")/.." && pwd)
cd "$root"

if ! command -v flatpak >/dev/null 2>&1 || ! command -v flatpak-builder >/dev/null 2>&1; then
    printf '%s\n' "flatpak and flatpak-builder are required." >&2
    exit 1
fi

manifest="$root/.github/flatpak/engineer.atlas.Nyxt.yml"
manifest_value() {
    awk -v key="$1" '$1 == key ":" { gsub(/[\047"]/, "", $2); print $2; exit }' "$manifest"
}

app_id=$(manifest_value id)
runtime=$(manifest_value runtime)
runtime_version=$(manifest_value runtime-version)
sdk=$(manifest_value sdk)
branch=$(manifest_value branch)
branch=${branch:-master}
if [ -z "$app_id" ] || [ -z "$runtime" ] || [ -z "$runtime_version" ] || [ -z "$sdk" ]; then
    printf '%s\n' "The Flatpak manifest is missing id, runtime, or sdk." >&2
    exit 1
fi

sudo flatpak remote-add --if-not-exists flathub https://dl.flathub.org/repo/flathub.flatpakrepo
sudo flatpak install -y --noninteractive flathub \
    "${runtime}//${runtime_version}" \
    "${sdk}//${runtime_version}"

CEF_VERSION="${CEF_VERSION:-150.0.22+gc1a097e+chromium-150.0.7871.256}"
CEF_SHA1="${CEF_SHA1:-7e7ca1e73e5d5e6317ca53dc1302fd180a3e1222}"
jobs=$(nproc 2>/dev/null || echo 2)
export CMAKE_BUILD_PARALLEL_LEVEL="$jobs"

work="$root/build/flatpak"
mkdir -p "$work"

if [ -z "${CEF_ROOT:-}" ]; then
    name="cef_binary_${CEF_VERSION}_linux64_minimal"
    archive="$work/${name}.tar.bz2"
    if [ ! -f "$archive" ]; then
        curl -fL --retry 3 -o "$archive" \
            "https://cef-builds.spotifycdn.com/${name}.tar.bz2"
    fi
    echo "$CEF_SHA1  $archive" | sha1sum -c -
    tar -xjf "$archive" -C "$work"
    CEF_ROOT="$work/$name"
fi

make NYXT_RENDERER=cef CEF_ROOT="$CEF_ROOT" NODE_SETUP=false nyxt

stage="$work/stage"
rm -rf "$stage"
mkdir -p "$stage/lib" "$stage/cef"

cef_rel=$(awk -F', ' '$1 == "cl-cef" { print $3; exit }' ocicl.csv)
cef_build="$root/ocicl/$(dirname "$cef_rel")/build"
clipboard_rel=$(awk -F', ' '$1 == "cl-clipboard" { print $3; exit }' ocicl.csv)
clipboard_dir="$root/ocicl/$(dirname "$clipboard_rel")"

install -m 755 "$root/nyxt" "$stage/nyxt"
if [ ! -f "$clipboard_dir/libcl-clipboard.so" ]; then
    printf '%s\n' "libcl-clipboard.so is missing from $clipboard_dir" >&2
    exit 1
fi
install -m 755 "$clipboard_dir/libcl-clipboard.so" "$stage/lib/libcl-clipboard.so"

for cef_name in \
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
    if [ ! -f "$cef_build/$cef_name" ]; then
        printf '%s\n' "CEF runtime file $cef_name is missing from $cef_build" >&2
        exit 1
    fi
    cp -f "$cef_build/$cef_name" "$stage/cef/$cef_name"
done
chmod 755 "$stage/cef/cl-cef-helper" "$stage/cef/chrome-sandbox"
cp -a "$cef_build/locales" "$stage/cef/locales"

find_lib() {
    lib_name=$1
    found=$(ldconfig -p | awk -v name="$lib_name" '$1 == name { print $NF; exit }')
    if [ -n "$found" ]; then
        printf '%s\n' "$found"
        return 0
    fi
    for dir in /lib /usr/lib /lib64 /usr/lib64 \
        /lib/x86_64-linux-gnu /usr/lib/x86_64-linux-gnu
    do
        if [ -e "$dir/$lib_name" ]; then
            printf '%s\n' "$dir/$lib_name"
            return 0
        fi
    done
    printf '%s\n' "Could not find shared library $lib_name" >&2
    return 1
}

install_lib() {
    lib_name=$1
    path=$(find_lib "$lib_name")
    real=$(readlink -f "$path")
    base=$(basename "$real")
    cp -a "$real" "$stage/lib/$base"
    if [ "$base" != "$lib_name" ]; then
        ln -sfn "$base" "$stage/lib/$lib_name"
    fi
}

install_lib libfixposix.so
install_lib libenchant-2.so

sed "s/^Icon=nyxt$/Icon=${app_id}/" "$root/assets/nyxt.desktop" > "$stage/${app_id}.desktop"
sed "s|nyxt.desktop|${app_id}.desktop|" "$root/assets/nyxt.metainfo.xml" > "$stage/${app_id}.metainfo.xml"
for size in 16 32 128 256 512; do
    cp "$root/assets/icons/nyxt_${size}x${size}.png" "$stage/nyxt_${size}x${size}.png"
done

repo="$work/repo"
build="$work/build"
built_manifest="$work/engineer.atlas.Nyxt.yml"
rm -rf "$repo" "$build"
awk -v stage="$stage" '
    $1 == "path:" && index($2, "build/flatpak/stage") {
        printf "        path: %s\n", stage
        next
    }
    { print }
' "$manifest" > "$built_manifest"
printf '%s\n' "Building ${app_id} on ${runtime}//${runtime_version}"
flatpak-builder --disable-rofiles-fuse --force-clean \
    --state-dir="$work/state" \
    --repo="$repo" \
    --install-deps-from=flathub \
    "$build" "$built_manifest"

output="$work/Nyxt-x86_64.flatpak"
rm -f "$output"
flatpak build-bundle "$repo" "$output" "$app_id" "$branch"
printf '%s\n' "Wrote $output"
