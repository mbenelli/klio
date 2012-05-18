#!/bin/sh

export KLIO_PATH=~/.klio

gsi -:da- \
$KLIO_PATH/prelude \
$KLIO_PATH/lists \
$KLIO_PATH/charsets \
$KLIO_PATH/strings \
$KLIO_PATH/base64 \
$KLIO_PATH/rfc1123 \
$KLIO_PATH/uri \
$KLIO_PATH/http-srv \
$KLIO_PATH/binary-io \
$KLIO_PATH/sqlite3 \
$KLIO_PATH/fetchwrite \
$KLIO_PATH/json \
$KLIO_PATH/kws \
scada \
-e '(begin (scada#init-active-alarms-from-db) (pp scada#active-alarms) (thread-start! scada#update-thread) (kws#kws port-number: 8000 server-root: "." dispatcher: scada#dispatch multithread: #t))'

