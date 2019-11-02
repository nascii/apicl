[[ $- != *i* ]] && return

[[ -e /etc/bashrc ]] && source /etc/bashrc

shopt -s checkwinsize

run_prompt_commands() {
    eval "$(resize)" &> /dev/null
    stty sane
}

PS1=' \[\e[36m\w\] \[\e[33m\]\[\e[1m\]$\[\e[0m\] '
PROMPT_COMMAND="run_prompt_commands"
