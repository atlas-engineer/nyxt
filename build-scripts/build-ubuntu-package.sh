#!/usr/bin/env bash
# SPDX-FileCopyrightText: Atlas Engineer LLC
# SPDX-License-Identifier: BSD-3-Clause

# Inspired by https://gitlab.com/ralt/linux-packaging/-/blob/eae586eaad5d6448121c53412ff3f2de712b24ca/.ci/build.sh.

set -xe

sudo gem install --no-document fpm &> /dev/null

export PATH=~/.gem/ruby/$(ls ~/.gem/ruby)/bin:$PATH

git clone --depth=1 --branch=sbcl-2.2.6 https://github.com/sbcl/sbcl.git ~/sbcl &> /dev/null
(
    cd ~/sbcl
    set +e
    sh make.sh --fancy --with-sb-linkable-runtime --with-sb-dynamic-core --with-sb-core-compression --dynamic-space-size=3072 &> sbcl-build.log
    code=$?
    set -e
    test $code = 0 || (cat sbcl-build.log && exit 1)

    sudo sh install.sh &> /dev/null
)

export SBCL_HOME=/usr/local/lib/sbcl

mkdir -p ~/common-lisp
for repo in https://gitlab.com/ralt/linux-packaging.git \
					 https://github.com/privet-kitty/wild-package-inferred-system \
					 https://github.com/cffi/cffi \
					 https://github.com/edicl/cl-ppcre \
					 https://github.com/cl-babel/babel \
					 https://gitlab.common-lisp.net/alexandria/alexandria.git \
					 https://github.com/trivial-features/trivial-features
					 do
						git clone --depth=1 "$repo" ~/common-lisp/$(basename "$repo")/ &> /dev/null
				 done
## Modern ASDF needed.
git clone --depth=1 --branch=3.3.5 https://gitlab.common-lisp.net/asdf/asdf.git ~/common-lisp/asdf/ &> /dev/null

mkdir -p ~/.config/common-lisp/source-registry.conf.d/
echo "(:tree \"$(pwd)/\")" >> ~/.config/common-lisp/source-registry.conf.d/linux-packaging.conf

echo
echo "==> ASDF diagnostic"
ls -la ~/.config/common-lisp/source-registry.conf.d/
sbcl \
  --eval '(require "asdf")' \
  --eval '(format t "- ASDF version: ~a~%" (asdf:asdf-version))' \
  --eval '(format t "- ASDF default registries: ~a~%" asdf:*default-source-registries*)' \
  --eval '(format t "- ASDF user source registry directory: ~a~%" (asdf/source-registry:user-source-registry-directory))' \
  --quit

## TODO: Warning, linux-packaging deps submodule might be missing!
echo
echo "==> Build package"
sbcl \
    --disable-debugger \
    --eval '(require "asdf")' \
		--eval '(asdf:load-system :nyxt/submodules)' \
    --eval "(format t \"==> Loading linux-packaging...~%\")" \
    --eval "(asdf:load-system :linux-packaging)" \
    --eval "(asdf:load-system :nyxt)" \
    --eval "(format t \"==> Loading :nyxt-ubuntu-package...~%\")" \
    --eval "(asdf:load-system :nyxt-ubuntu-package)" \
    --eval "(asdf:make :nyxt-ubuntu-package)" \
    --quit
