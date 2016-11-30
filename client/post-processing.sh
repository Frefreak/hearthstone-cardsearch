#!/usr/bin/env bash
# invoke after successfully built and card-data.json/card-image.json are generated

#echo "var cards=String.raw\`" > card-data.js
#cat ../card-data.json >> card-data.js
#echo "\`" >> card-data.js

#echo "var images=String.raw\`" > card-image.js
#cat ../card-image.json >> card-image.js
#echo "\`" >> card-image.js

stack build && ccjs .stack-work/dist/x86_64-linux/Cabal-1.24.0.0_ghcjs/build/client/client.jsexe/all.js --compilation_level=ADVANCED_OPTIMIZATIONS --externs=node --externs='./jquery-3.1.1.min.js'  --externs='./materialize.min.js'> client.js 2>/dev/null
#stack build && ccjs .stack-work/dist/x86_64-linux/Cabal-1.24.0.0_ghcjs/build/client/client.jsexe/all.js --compilation_level=SIMPLE_OPTIMIZATIONS > client.js
