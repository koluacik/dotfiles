export VISUAL=nvim
export EDITOR=nvim
export GPG_TTY=$(tty)

PATH=/home/deniz/.local/bin:$PATH
eval "$(stack --bash-completion-script stack)"
source <(kitty + complete setup bash)

#avoid nested rangers
ranger() {
    if [ -z "$RANGER_LEVEL" ]; then
        /usr/bin/ranger "$@"
    else 
        exit
    fi
}
