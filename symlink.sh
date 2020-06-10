#!/usr/bin/env bash

ln -sf $PWD/autorandr $HOME/.config/
ln -sf $PWD/bash-config $HOME/.bash
ln -sf $PWD/dunst $HOME/.config/
ln -sf $PWD/kitty $HOME/.config/
ln -sf $PWD/nvim $HOME/.config/
ln -sf $PWD/ranger $HOME/.config/
ln -sf $PWD/stylish-haskell $HOME/.config/
ln -sf $PWD/xmonad/xmonad.hs $HOME/.xmonad/
ln -sf $PWD/xmonad/build $HOME/.xmonad/
ln -sf $PWD/xmonad/xmonad.start $HOME/.xmonad/
ln -sf $PWD/xmobar/xmobarrc $HOME/.xmobarrc

for file in bash-config/bash_commands/*
do
    echo $file
    if [ -x "$file" ]; then
        ln -sf $PWD/$file $HOME/.local/bin
    fi
done
