#!/bin/sh
exec /usr/local/bin/scsh +lp /usr/local/0.7/install-lib-1.3.0 +lp /usr/local/share/scsh-0.7 -ll scheme/install-lib-module.scm -lel configure.scm -o posix -o install-lib -e install-main-quiet -s "$0" "$@"
!#
