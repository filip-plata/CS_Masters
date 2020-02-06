#!/usr/bin/env bash

set -e

DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" >/dev/null 2>&1 && pwd )"
PROG=stoper

xst -ifn ${DIR}/${PROG}.xst
ngdbuild ${PROG} -uc ${DIR}/${PROG}.ucf
map ${PROG}
par -w ${PROG}.ncd ${PROG}_par.ncd
bitgen -w ${PROG}_par.ncd -g StartupClk:JTAGClk
