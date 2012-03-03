#!/usr/bin/env bash

cd ../klio
gsi -:da- prelude lists strings base64 rfc1123 http binary-io fetchwrite json kws ../tests/kws-fw -e '(begin (thread-start! kws-fw#update-thread) (kws#kws port-number: 8000 server-root: "../benchmarks/sandbox/" dispatcher: kws-fw#dispatcher multithread: #t))'

