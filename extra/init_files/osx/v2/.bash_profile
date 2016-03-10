
export CLICOLOR=1
export LSCOLORS=exfxcxdxbxegedabagacad

print_color_test () {
	for b in {0..7} 9; do for f in {0..7} 9; do for attr in "" bold; do echo -e "$(tput setab $b; tput setaf $f; [ -n "$attr" ] && tput $attr) $f ON $b $attr $(tput sgr0)"; done; done; done
} 

# red=$(tput setaf 1)   #\e[31m
# green=$(tput setaf 2) #\e[32m
# bold=$(tput bold)
# reset=$(tput sgr0)    #\e[0m
# export PS1='\[$red\]\[$bold\]\w \[$green\]\$ \[$reset\]'


if tput setaf 1 &> /dev/null; then
    tput sgr0
    if [[ $(tput colors) -ge 256 ]] 2>/dev/null; then
	MAGENTA=$(tput setaf 9)
	ORANGE=$(tput setaf 172)
	GREEN=$(tput setaf 190)
	TRUEGREEN=$(tput setaf 2)
	PURPLE=$(tput setaf 141)
	WHITE=$(tput setaf 256)
    else
	MAGENTA=$(tput setaf 5)
	ORANGE=$(tput setaf 4)
	GREEN=$(tput setaf 2)
	PURPLE=$(tput setaf 1)
	WHITE=$(tput setaf 7)
    fi
    BOLD=$(tput bold)
    RESET=$(tput sgr0)
else
    MAGENTA="\033[1;31m"
    ORANGE="\033[1;33m"
    GREEN="\033[1;32m"
    BLUE="\033[1;34m"
    PURPLE="\033[1;35m"
    WHITE="\033[1;37m"
    BOLD=""
    RESET="\033[m"
fi

if [ -z "$TRUEGREEN" ] ; then 
   TRUEGREEN=$GREEN
fi       

export PS1="\[$WHITE\]\[$BOLD\]\u\[$RESET\]@\[$WHITE\]\[$BOLD\]\h\[$RESET\]:\[$TRUEGREEN\]\w \[$PURPLE\]$ \[$RESET\]"


export TERM='xterm-256color'
export PATH=~/bin:"$(brew --prefix coreutils)/libexec/gnubin:$PATH"
export MANPATH="$(brew --prefix coreutils)/libexec/gnuman:$MANPATH"

export HOMEBREW_CASK_OPTS="--appdir=/Applications"

#ls="gls --color"

alias ls="ls --color"

# java and groovy


# these are also set for gui in preferences with: http://diaryproducts.net/EnvPane
export JAVA_VERSION=1.8
export JAVA_HOME=/Library/Java/JavaVirtualMachines/jdk1.8.0_65.jdk/Contents/Home
export GROOVY_HOME=/usr/local/opt/groovy/libexec
export GRADLE_HOME=/usr/local/opt/gradle/libexec


# perforce config for shell
. ~/.p4config


[[ -s ~/.bashrc ]] && source ~/.bashrc

# The next line updates PATH for the Google Cloud SDK.
source '/Users/joelholder/google-cloud-sdk/path.bash.inc'

# The next line enables shell command completion for gcloud.
source '/Users/joelholder/google-cloud-sdk/completion.bash.inc'
