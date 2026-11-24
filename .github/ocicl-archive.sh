#!/bin/sh
# SPDX-FileCopyrightText: Atlas Engineer LLC
# SPDX-License-Identifier: BSD-3-Clause

set -eu

root=$(CDPATH= cd -- "$(dirname "$0")/.." && pwd)
cd "$root"

for command in awk curl git jq tar sha256sum; do
    if ! command -v "$command" >/dev/null 2>&1; then
        printf '%s\n' "$command is required." >&2
        exit 1
    fi
done

if [ -n "${OCICL_ARCHIVE_VERSION:-}" ]; then
    version=$OCICL_ARCHIVE_VERSION
elif [ "${GITHUB_REF_TYPE:-}" = tag ]; then
    version=$GITHUB_REF_NAME
else
    version=$(awk -F'"' '/^[[:space:]]*:version / { print $2; exit }' nyxt.asd)
fi
if [ -z "$version" ]; then
    printf '%s\n' "Could not find :version in nyxt.asd" >&2
    exit 1
fi

work=${1:-build/ocicl-archive}
rm -rf "$work"
ocicl=$work/ocicl
mkdir -p "$ocicl"
sources=$(mktemp)
trap 'rm -f "$sources"' EXIT

awk -F', ' '
    NF == 0 { next }
    NF < 3 {
        printf "Bad ocicl.csv line: %s\n", $0 > "/dev/stderr"
        exit 1
    }
    !seen[$2]++ {
        dir = $3
        sub(/\/.*/, "", dir)
        printf "%s\t%s\n", $2, dir
    }
' ocicl.csv > "$sources"

while IFS=$(printf '\t') read -r source dir; do
    case $source in
        git+*)
            rest=${source#git+}
            commit=${rest##*@}
            commit=${commit%%#*}
            repo_url=${rest%@*}
            dest=$ocicl/$dir
            printf '%s\n' "git $repo_url $commit"
            git init -q "$dest"
            git -C "$dest" remote add origin "$repo_url"
            git -C "$dest" fetch --depth 1 origin "$commit"
            git -C "$dest" checkout --quiet FETCH_HEAD
            rm -rf "$dest/.git"
            ;;
        ghcr.io/*)
            body=${source#ghcr.io/}
            repo=${body%@sha256:*}
            digest=${body##*@sha256:}
            token=$(curl -fsS "https://ghcr.io/token?service=ghcr.io&scope=repository:${repo}:pull" | jq -r .token)
            manifest=$(curl -fsS \
                -H "Authorization: Bearer $token" \
                -H "Accept: application/vnd.oci.image.manifest.v1+json" \
                "https://ghcr.io/v2/${repo}/manifests/sha256:${digest}")
            count=$(printf '%s' "$manifest" | jq '.layers | length')
            if [ "$count" != 1 ]; then
                printf '%s\n' "$source has $count layers" >&2
                exit 1
            fi
            blob=$(printf '%s' "$manifest" | jq -r '.layers[0].digest' | cut -d: -f2)
            printf '%s\n' "$dir $blob"
            curl -fsSL -H "Authorization: Bearer $token" -o "$work/layer.tgz" \
                "https://ghcr.io/v2/${repo}/blobs/sha256:${blob}"
            tar -xzf "$work/layer.tgz" -C "$ocicl"
            rm -f "$work/layer.tgz"
            ;;
        *)
            printf '%s\n' "Unknown source $source" >&2
            exit 1
            ;;
    esac
done < "$sources"

missing=0
while IFS= read -r line; do
    [ -n "$line" ] || continue
    path=$(printf '%s\n' "$line" | awk -F', ' '{ print $3 }')
    if [ ! -e "$ocicl/$path" ]; then
        printf '%s\n' "Missing $path" >&2
        missing=1
    fi
done < ocicl.csv
if [ "$missing" -ne 0 ]; then
    exit 1
fi

archive=$root/nyxt-${version}-ocicl.tar.gz
rm -f "$archive"
tar --sort=name --mtime=@0 --owner=0 --group=0 --numeric-owner \
    -C "$work" -cf - ocicl | gzip -n > "$archive"
sha256sum "$archive"
