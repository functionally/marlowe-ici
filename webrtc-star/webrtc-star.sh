#!/usr/bin/env nix-shell
#!nix-shell -i bash -p nodejs-17_x nginx


npm install libp2p-webrtc-star-signalling-server

mkdir -p nginx.tmp

trap "nginx -p $PWD -c nginx.conf -e nginx.tmp/error.log -s stop" EXIT

nginx -p $PWD -c nginx.conf -e nginx.tmp/error.log

npx webrtc-star --host 127.0.0.1 --port 4009 --metrics false
