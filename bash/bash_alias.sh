alias ls='ls -X --color=auto'
alias cal='ncal -bM3'
alias fehh='feh -B "white" -g "1280x720+360+180" -. -Z \
    --action1 '\''xclip -selection clipboard -t image/png -i %F'\'' \
    --action2 '\''cp %F ~/documents/pictures/screenshots/'\'' '
