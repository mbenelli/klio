#!/usr/bin/env sh

cd ../klio
gsi -:da- prelude lists charsets strings base64 rfc1123 uri http-srv sqlite3 kws ../tests/kws-db -e '(kws#kws port-number: 8000 server-root: "../benchmarks/sandbox/" dispatcher: kws-db#dispatcher multithread: #t)'

