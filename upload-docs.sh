#!/bin/bash
name=$(cabal info . | grep library | awk '{print $2;}')
echo $name
plain_name="${name%-*}"
cabal haddock --hyperlink-source --html-location="http://hackage.haskell.org/package/$plain_name/docs" --contents-location="http://hackage.haskell.org/package/$plain_name"
cp -R ./dist/doc/html/dson/ "$name-docs"
tar cvzf --format=ustar -f "$name-docs.tar.gz" "$name-docs"
read -p "Username: " username
read -s -p "Password: " password
curl -X PUT -H 'Content-Type: application/x-tar' -H 'Content-Encoding: gzip' --data-binary "@$name-docs.tar.gz" "https://$username:$password@hackage.haskell.org/package/$name/docs"
rm -rf dson*docs*
