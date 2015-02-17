#!/usr/bin/env bash

export KLIO_PATH=../klio

gsi -:da \
$KLIO_PATH/prelude \
$KLIO_PATH/lists \
$KLIO_PATH/charsets \
$KLIO_PATH/strings \
$KLIO_PATH/base64 \
$KLIO_PATH/datetime \
$KLIO_PATH/rfc1123 \
$KLIO_PATH/uri \
$KLIO_PATH/http-srv \
$KLIO_PATH/kws \
-e '(kws#kws port-number: 8000 server-root: "./sandbox" multithread: #t)'

