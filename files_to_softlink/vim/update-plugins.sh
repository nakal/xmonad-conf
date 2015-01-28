#!/bin/sh

echo "Updating pathogen..."
cd $HOME/.vim/autoload/vim-pathogen
git pull

echo "Updating plugins..."
cd $HOME/.vim/bundle
find . -type d -mindepth 1 -maxdepth 1 -exec sh -c "cd {} ; git pull ; cd .." \;

exit 0
