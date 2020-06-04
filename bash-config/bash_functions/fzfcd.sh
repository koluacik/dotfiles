#!/usr/bin/env bash

function fzfcd() {
  cd $(find $HOME -type d \
    ! -path "*/.*" \
    ! -path "*/git-clones/*" \
    | fzf --no-color)
}
