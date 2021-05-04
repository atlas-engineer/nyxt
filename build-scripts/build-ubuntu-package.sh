#!/usr/bin/env bash
# SPDX-FileCopyrightText: Atlas Engineer LLC
# SPDX-License-Identifier: BSD-3-Clause

# Inspired by https://gitlab.com/ralt/linux-packaging/-/blob/eae586eaad5d6448121c53412ff3f2de712b24ca/.ci/build.sh.

set -xe

sudo gem install --no-document fpm &> /dev/null

export PATH=~/.gem/ruby/$(ls ~/.gem/ruby)/bin:$PATH

git clone --depth=1 --branch=sbcl-2.1.0 https://github.com/sbcl/sbcl.git ~/sbcl &> /dev/null
(
    cd ~/sbcl
    set +e
    sh make.sh --fancy --with-sb-linkable-runtime --with-sb-dynamic-core &> sbcl-build.log
    code=$?
    set -e
    test $code = 0 || (cat sbcl-build.log && exit 1)

    sudo sh install.sh &> /dev/null
)

export SBCL_HOME=/usr/local/lib/sbcl

mkdir -p ~/common-lisp
git clone --depth=1 https://gitlab.com/ralt/linux-packaging.git ~/common-lisp/linux-packaging/ &> /dev/null
## Modern ASDF needed.
git clone --depth=1 --branch=3.3.4 https://gitlab.common-lisp.net/asdf/asdf.git ~/common-lisp/asdf/ &> /dev/null

mkdir -p ~/.config/common-lisp/source-registry.conf.d/
echo "(:tree \"$(pwd)/\")" >> ~/.config/common-lisp/source-registry.conf.d/linux-packaging.conf
## TODO: We should not have to do the following as it should be taken care of by :nyxt/quicklisp.
echo "(:tree \"$PWD/_build/submodules\")" >> ~/.config/common-lisp/source-registry.conf.d/asdf.conf

echo
echo "==> ASDF diagnostic"
ls -la ~/.config/common-lisp/source-registry.conf.d/
sbcl \
  --eval '(require "asdf")' \
  --eval '(format t "- ASDF version: ~a~%" (asdf:asdf-version))' \
  --eval '(format t "- ASDF default registries: ~a~%" asdf:*default-source-registries*)' \
  --eval '(format t "- ASDF user source registry directory: ~a~%" (asdf/source-registry:user-source-registry-directory))' \
  --quit

echo
echo "==> Build package"
sbcl \
    --disable-debugger \
    --eval '(require "asdf")' \
		--eval '(asdf:load-system :nyxt/quicklisp)' \
    --eval "(format t \"==> Quicklisp local project directories: ~s~%\" ql:*local-project-directories*)" \
    --eval "(format t \"==> Quicklisp local systems: ~a~%\" (ql:list-local-systems))" \
    --eval "(format t \"==> Quickloading linux-packaging...~%\")" \
    --eval "(ql:quickload :linux-packaging :silent t)" \
    --eval "(format t \"==> Quickloading :nyxt...~%\")" \
    --eval "(ql:quickload :nyxt :silent t)" \
    --eval "(format t \"==> Quickloading :nyxt-ubuntu-package...~%\")" \
    --eval "(ql:quickload :nyxt-ubuntu-package :silent t)" \
    --eval "(asdf:make :nyxt-ubuntu-package)" \
    --quit
