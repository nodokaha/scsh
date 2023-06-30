#!/bin/sh
exec /usr/local/bin/scsh -lm scheme/install-lib-module.scm -le scheme/configure.scm -o posix -o install-lib -e install-main-quiet -s "$0" "$@"
!#
