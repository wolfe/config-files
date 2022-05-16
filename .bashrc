# Executed by default by interactive (but not login) scripts, but since
# this is included by .bash_profile, this is called by all.

[[ -s $HOME/.aws_credentials ]] && source $HOME/.aws_credentials

export GWT_HOME=/usr/local/gwt
export LPDEST=$PRINTER

################################################################
# INCLUDING GIT IN PROMPT
################################################################

function parse_git_dirty {
  [[ $(git status 2> /dev/null | tail -n1) != *"nothing to commit"* ]] && echo "*"
}
function parse_git_branch {
  git branch --no-color 2> /dev/null | sed -e '/^[^*]/d' -e "s/* \(.*\)/[\1$(parse_git_dirty)]/"
}
function append_to_prompt {
    tw=$(terraform workspace show)
    [[ "$tw" == 'default' ]] && echo "$(parse_git_branch)" || echo "<$tw>"
}
PS1="\w\$(parse_git_branch): "

################################################################
# ALIASES
################################################################
alias cp="cp -i"
alias mv="mv -i"
alias ls="ls -F"  # -C
alias lookup="cat ~/Dropbox/notes/address/* | grep -i"
alias grep="grep -s"
alias egrep="egrep -s"
alias fgrep="fgrep -s"
alias ssh="ssh -X"
alias rehash="hash -r"
alias dirs="dirs -v"

if [[ "$OSTYPE" == *"linux"* ]] ; then
    function open {
        for i in "$@"; do xdg-open "$i"; done
    }
fi

################################################################

################################################################
# Global Environment variables
################################################################

export EDITOR='/usr/bin/emacs'
export VISUAL=$EDITOR

# bind 'set show-all-if-ambiguous on'		# Tab once for complete

ulimit -c 0 # Maximum core file size

# CDPATH
for i in ~/*
do
if [ -d "$i" ]
    then
    CDPATH=$CDPATH:$i
fi
done
export CDPATH='.':~$CDPATH
export PATH=/usr/texbin:$PATH

# LATEX
export TEXINPUTS=.:./tex:$HOME/tex:$HOME/tex/combgames:$HOME/prosper:$HOME/courses/problems::
export TEXPKS=./tex:./fonts/pk:$HOME/fonts/pk::
export TEXFONTS=./tex:./fonts/tfm:$HOME/fonts/tfm::
export XDVIFONTS=$TEXPKS
export DVIPSHEADERS=$TEXINPUTS
export MFINPUTS=.:./tex:./fonts/mf:$HOME/fonts/mf::
export TEXBIB=$TEXINPUTS

################################################################
# FUNCTIONS
################################################################
function checksingletex {
    # Check that the user provides exactly one argument, and that file.tex exists
    if [ $# != 1 ]
        then echo Enter exactly one argument
        return 1
    elif [ -e $*.tex ]
        then return 0
    else echo file $*.tex does not exist
        return 1
    fi
}
export -f checksingletex

function lx {
    if ( checksingletex $* ) ; then
        latex $*
        xdvi $*
    fi
}
export -f lx

function lp {
    if ( checksingletex $* ) ; then
        latex $*
        dvips $* -o $*.ps
        ps2pdf -dAutoRotatePages=/None $*.ps $*.pdf
        open $*.pdf
    fi
}
export -f lp

function swap {
  # Check that the user provides exactly two arguments, and that files exist
  if [ $# != 2 ]; then echo Enter exactly two arguments; return 1; fi
  if [ ! -e $1 ]; then echo file $1 does not exists ; return 1; fi
  if [ ! -e $2 ]; then echo file $2 does not exists ; return 1; fi
  if [ $2 == $1 ]; then echo files are identical ; return 1; fi
  mv $1 swaptemp.tmp
  mv $2 $1
  mv swaptemp.tmp $2
}
export -f swap

alias rsynccommand='rsync -Crlpuz --exclude "*~"'
function rsyncbase {
  HOMEDIR=~/
  RHOME=qvtrace@simulink0
  PWD=`pwd`/
  RDIR=`echo $PWD | sed s%$HOMEDIR%${RHOME}:%`
}

function put {
  rsyncbase
  rsynccommand . $RDIR # put
}
export -f put

function get {
  rsyncbase
  rsynccommand $RDIR . # get
}
export -f get

function sync {
  echo "* PUT *"
  put
  echo "* GET *"
  get
}
export -f sync

function rgrep {
    DIR="."
    ARGS=""
    SEARCH=""
    PATTERN="*"
    for arg; do
        if [[ -d "$arg" && -e "$SEARCH" ]]; then DIR="$arg";
        elif [[ $arg == -* ]]; then ARGS=" ${ARGS} ${arg}"
        elif [[ "$SEARCH" == "" ]]; then SEARCH="$arg"
        else PATTERN="$arg"
        fi
    done
    grep -r "$SEARCH" "$ARGS" "$DIR" --include="$PATTERN" | cut -c -240 ;
}

function rpass {
  MATRIX="0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz"
  LENGTH=8

  n=1
  PASS=""
  while [ $n -le $LENGTH ]
  do
  	PASS="$PASS${MATRIX:$(($RANDOM%${#MATRIX})):1}"
  	let n+=1
  done
  echo "$PASS"      # ==> Or, redirect to a file, as desired.
}

function bundle {
	echo "# To unbundle, sh this file"
	for i in $*
	do
		echo "cat >"$i" <<'GO.SYSIN DD *'"
		cat $i
        if [ `tail -1 $i | wc -l` = 0 ] # file ends in no newline
        then echo ; echo "Appended newline to file $i" > /dev/stderr
        fi
		echo "GO.SYSIN DD *"
	done
}

function source_if_exists { if [ -e $1 ] ; then source $1 ; fi }

source_if_exists $HOME/bin/git-completion.bash

# Add the following variables:
export WORKON_HOME=$HOME/.virtualenvs
export PROJECT_HOME=$HOME/devel

function shout {
    SAVED_VOLUME=`sudo osascript -e 'get output volume of (get volume settings)'`
    sudo osascript -e 'set volume output volume 100'
    say $*
    sudo osascript -e "set volume output volume $SAVED_VOLUME"
}

function markdown {
    pandoc --from markdown --to html $*
}

function loopdirs {
    for i in *
    do
        if [ -e $i/.git ]; then
            # Print directory name in yellow and branch name in green
            echo -en "\033[1;33m"
            printf '  %-10s' "${i}"
            echo -en "\033[0m";
            pushd "./${i}" > /dev/null
            branch="$(git rev-parse --abbrev-ref HEAD)"
            echo -e " \033[1;32m${branch}\033[0m"
            $*
            popd > /dev/null
        fi
    done
}

function ppjson {
    cat $1 | python -c'import fileinput, json; print(json.dumps(json.loads("".join(fileinput.input())), indent=2, sort_keys=True))' | sed 's/[ \t]*$//'
}

function _gitlsmerged_ {
    git branch -r --merged | egrep -v "$(git rev-parse --abbrev-ref HEAD)|master|dev|matisse|main|automated|1.13" | xargs -n 1 echo
}

function gitlsmerged {
    for var in $(_gitlsmerged_); do
        echo git push --delete $(cut -d '/' -f 1 <<< "$var") $(cut -d '/' -f 2- <<< "$var")
    done
}

function gitrmmerged {
    for var in $(_gitlsmerged_); do
        git push --delete $(cut -d '/' -f 1 <<< "$var") $(cut -d '/' -f 2- <<< "$var")
    done
}

export JAVA_HOME=/usr/lib/jvm/default-java
export NODE_PATH="/usr/local/lib/node_modules"

docker-remove-containers() {
    docker stop $(docker ps -aq)
    docker rm $(docker ps -aq)
}

docker-armageddon() {
    docker-remove-containers
    docker network prune -f
    docker rmi -f $(docker images --filter dangling=true -qa)
    docker volume rm $(docker volume ls --filter dangling=true -q)
    docker rmi -f $(docker images -qa)
}

function my-ip {
    hostname -I | xargs -n 1 | grep 10.10  # Using xargs just to trim whitespace
}

function check-ip {
    MY_IP=`my-ip`
    if [[ ${MY_IP} != ${DESKTOP_IP} ]] ; then echo "Expected ip ${DESKTOP_IP}; Actual ip ${MY_IP}"; fi
}

export PATH=~/.local/bin:$PATH  # Used by pip install

# stty intr ^J  # So I can map ^C to copy

source_if_exists $HOME/analyzere/.bashrc

function npmfind {
    find $1 -not \( -name node_modules -prune \) "${@:2}"
}
