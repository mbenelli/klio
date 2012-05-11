#!/bin/sh

export KLIO_PATH=../../klio

gsi -:da- \
$KLIO_PATH/prelude \
$KLIO_PATH/lists \
$KLIO_PATH/charsets \
$KLIO_PATH/strings \
$KLIO_PATH/base64 \
$KLIO_PATH/rfc1123 \
$KLIO_PATH/uri \
$KLIO_PATH/http-srv \
$KLIO_PATH/input-parse \
$KLIO_PATH/ssax \
$KLIO_PATH/sxml \
$KLIO_PATH/kws \
simple \
-e '(begin (kws#kws port-number: 8080 server-root: "." dispatcher: simple#dispatch multithread: #t))'

