#!/usr/bin/env bash

cd ../klio
gsi prelude lists base64 datetime http kws -e '(kws#kws port-number: 8000 server-root: "../benchmarks/sandbox/" multithread: #t)'


