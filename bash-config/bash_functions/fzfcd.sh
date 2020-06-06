#!/usr/bin/env bash

function fzfcd() {
  target=$(find $HOME -type d \
    ! -path "*/.*" \
    ! -path "*/git-clones/*" \
    | fzf --no-color)
  if [ -n "$target" ]; then
    cd "$target"
  fi
}
