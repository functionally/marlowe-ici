#!/usr/bin/env nix-shell
#!nix-shell -i "make -f" -p gnumake nodejs-17_x html-tidy


model.cid: ../docs/index.html ../docs/view.css ../docs/controller.js
	ipfs add --quieter --pin=false --recursive ../docs | tee model.cid
	cat model.cid | xargs ipfs name publish --lifetime=168h --key=marlowe-ici
	-ipfs pin remote rm --service=pinata --name=marlowe-ici
	cat model.cid | xargs ipfs pin remote add --service=pinata --name=marlowe-ici

../docs/index.html ../docs/view.css ../docs/controller.js: model.html view.css controller.js
	cp model.html ../docs/index.html
	cp view.css controller.js ../docs/

webrtc-star:
	cd webrtc-star; ./webrtc-star.sh

serve-dev:
	npx webpack-dev-server

serve: controller.js
	npx http-server --port 4010 --ssl --cert ../webrtc-star/fullchain.pem --key ../webrtc-star/privkey.pem

controller.js: src/controller.js
	npx webpack

lint: src/controller.js model.html eslint.json
	npx eslint -c eslint.json src/controller.js
	tidy -quiet -o /dev/null model.html

install:
	npm install


.SUFFIXES:

.PHONY: webrtc-star serve-dev serve lint install
