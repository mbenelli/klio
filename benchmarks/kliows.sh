#!/usr/bin/env bash

cd ../klio
gsi -:h10000 prelude lists charsets strings base64 datetime http kws -e '(kws#kws port-number: 8000 server-root: "../benchmarks/sandbox/" multithread: #f)'
