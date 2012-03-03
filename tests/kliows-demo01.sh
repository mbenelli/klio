#!/usr/bin/env bash

export KLIO_PATH=../klio

gsi -:da- \
$KLIO_PATH/prelude \
$KLIO_PATH/lists \
$KLIO_PATH/charsets \
$KLIO_PATH/strings \
$KLIO_PATH/base64 \
$KLIO_PATH/rfc1123 \
$KLIO_PATH/http \
$KLIO_PATH/binary-io \
$KLIO_PATH/sqlite3 \
$KLIO_PATH/fetchwrite \
$KLIO_PATH/json \
$KLIO_PATH/kws \
demo01 \
-e '(begin (demo01#init-active-alarms-from-db) (pp demo01#active-alarms) (thread-start! demo01#update-thread) (kws#kws port-number: 8000 server-root: "." dispatcher: demo01#dispatch multithread: #t))'

