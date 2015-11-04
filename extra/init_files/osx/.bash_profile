export CLICOLOR=1
export LSCOLORS=exfxcxdxbxegedabagacad

# SETUP PROMPT

# perfect ans: http://askubuntu.com/questions/24358/how-do-i-get-long-command-lines-to-wrap-to-the-next-line
# cyan=$(tput setaf 6) # \e[36m
# reset=$(tput sgr0)   # \e[0m
# export PS1='\[$cyan\]My prompt\[$reset\]>'

# target:
# export PS1='\e[31m\w \e[32m\$ \e[0m'

# impl
red=$(tput setaf 1)   #\e[31m
green=$(tput setaf 2) #\e[32m
reset=$(tput sgr0)    #\e[0m
export PS1='\[$red\]\w \[$green\]\$ \[$reset\]'


export TERM='xterm-256color'
export PATH=~/bin:$PATH

[[ -s ~/.bashrc ]] && source ~/.bashrc

if which rbenv > /dev/null; then eval "$(rbenv init -)"; fi

# todo: fix so only once
source /usr/local/opt/autoenv/activate.sh


