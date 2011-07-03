#!/usr/bin/env bash

cd ../klio
gsi -:da- prelude lists base64 datetime http sqlite3 kws ../tests/kws-db -e '(kws#kws port-number: 8000 server-root: "../benchmarks/sandbox/" dispatcher: kws-db#dispatcher multithread: #t)'


