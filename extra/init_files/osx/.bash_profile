export CLICOLOR=1
export LSCOLORS=exfxcxdxbxegedabagacad
export PS1='\033[0;31m\w \033[0;32m\$ \033[0m'
export TERM='xterm-256color'
export PATH=~/bin:$PATH

[[ -s ~/.bashrc ]] && source ~/.bashrc

if which rbenv > /dev/null; then eval "$(rbenv init -)"; fi

# todo: fix so only once
source /usr/local/opt/autoenv/activate.sh


