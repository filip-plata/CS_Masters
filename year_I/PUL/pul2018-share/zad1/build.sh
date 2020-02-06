#!/usr/bin/env bash

DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" >/dev/null 2>&1 && pwd )"

xst -ifn ${DIR}/calc.xst
ngdbuild calc -uc ${DIR}/calc.ucf
map calc
par -w calc.ncd calc_par.ncd
bitgen -w calc_par.ncd -g StartupClk:JTAGClk
