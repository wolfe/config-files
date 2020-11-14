# Executed by default by interactive (but not login) scripts, but since
# this is included by .bash_profile, this is called by all.

[[ -s $HOME/.aws_credentials ]] && source $HOME/.aws_credentials

export USERNAME=wolfe
export CAMPUSIP="wolfe@138.236.64.77"
export CAMPUS="wolfe@baal.gac.edu"
export w=$CAMPUS
export HOST=`hostname | sed 's/\..*//'`
export GWT_HOME=/usr/local/gwt

if [ `hostname` == 'qvtrace' ]; then
    export PRINTER=RICOH-RICOH-Aficio-SP-3510SF
elif [ `hostname` == 'nash' ]; then
    export PRINTER=QRA
fi
export LPDEST=$PRINTER

alias pg_dump="pg_dump --no-tablespaces --no-owner"
alias skey='eval `/usr/bin/ssh-agent` ; ssh-add ~/.ssh/id_rsa'

################################################################
# Generic bash meta-functions
################################################################
# copies function named $1 to name $2
# Syntax: prepend_to_function <name> [statements...]
function prepend_to_function()
{
    local name=$1
    shift
    local body="$@"
    eval "$(echo "${name}(){"; echo ${body}; declare -f ${name} | tail -n +3)"
}
# Syntax: append_to_function <name> [statements...]
function append_to_function()
{
    local name=$1
    shift
    local body="$@"
    eval "$(declare -f ${name} | head -n -1; echo ${body}; echo '}')"
}
function copy_function() {
    declare -F $1 > /dev/null || return 1
    eval "$(echo "${2}()"; declare -f ${1} | tail -n +2)"
}

################################################################
# GIT
################################################################
# Shows the [branch], and an * if there are uncommitted changes. Example:
# ~/projects/my_project[master*]:
# export PATH=/usr/local/git/bin:$PATH
export PATH=$HOME/bin:$PATH
export PATH=/usr/local/sbin:$PATH
export PATH=/usr/local/bin:$PATH
alias gitl='git log --pretty=format:"%h - %an, %aD : %s"'
alias her=heroku
function parse_git_dirty {
  [[ $(git status 2> /dev/null | tail -n1) != *"nothing to commit"* ]] && echo "*"
}
function parse_git_branch {
  git branch --no-color 2> /dev/null | sed -e '/^[^*]/d' -e "s/* \(.*\)/[\1$(parse_git_dirty)]/"
}

PS1='\w$(parse_git_branch): '
# PS1="$HOST> "
PS2=">> "


# date=`/bin/date`
# day="${date:0:3}, ${date:4:6}, ${date:24:4}"

################################################################
# ALIASES
################################################################
alias e=emacs
alias gdb="gdb -q"
alias t=less
alias cp="cp -i"
alias mv="mv -i"
alias ls="ls -F"  # -C
alias l="ls -A"
alias lookup="cat ~/Dropbox/notes/address/* | grep -i"
alias grep="grep -s"
alias egrep="egrep -s"
alias fgrep="fgrep -s"
alias ssh="ssh -X"

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

# PATH
export MANPATH=~/man:$MANPATH
for i in ~/bin/*
do
if [ -d "$i" ]
    then
    PATH=$i:$PATH
fi
done
export PATH

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

function jpgdate {
    for i in $*
      do
      echo $i `awk -f ~/www-docs/pics/getdate.awk $i`
      done
}
export -f jpgdate

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
export -f rsyncbase
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

function backup {
  cp -r . ~/tmp/`pwd | sed s%.*/%%`-`date '+%Y-%m-%d'`
}
export -f backup

function oldrgrep {
    PATTERN="${@: -1}"
    ARGS="${@:1:$(($#-1))}"
    grep -r $ARGS . --include=$PATTERN | cut -c -240 ;
    echo grep -r $ARGS . --include=$PATTERN
}

function rgrep {
    DIR="."
    ARGS=""
    SEARCH=""
    PATTERN="*"
    for arg; do
        if [ -d "$arg" ]; then DIR="$arg";
        elif [[ $arg == -* ]]; then ARGS=" ${ARGS} ${arg}"
        elif [[ "$SEARCH" == "" ]]; then SEARCH="$arg"
        else PATTERN="$arg"
        fi
    done
    grep -r "$SEARCH" "$ARGS" "$DIR" --include="$PATTERN" | cut -c -240 ;
}

function rmsvns { find . -name '.svn' -prune -exec rm -r -f {} \; ; }
function rmpycs { find . -name '*.pyc' -exec rm -f {} \; ; }
function rmgits { find . -name '.git*' -prune -exec rm -r -f {} \; ; }

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

function disable_account {
  sudo dscl . -create /Users/$* UserShell /usr/bin/false
}
function enable_account {
  sudo dscl . -create /Users/$* UserShell /bin/bash
}

function disable_kids {
  disable_account guest
  disable_account lila
  disable_account tovia
}
function enable_kids {
  enable_account guest
  enable_account lila
  enable_account tovia
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

function onmaster {
    if [[ $(git rev-parse --abbrev-ref HEAD) != "master" ]]
    then
        return 1
    fi
}

function gitlsmerged {
    onmaster \
    && (git branch -r --merged | egrep -v 'master|release-' | sed 's/origin\///' | xargs -n 1 echo) \
    && echo "- release branches -" \
    && (git branch -r --merged | grep 'release-' | sed 's/origin\///' | xargs -n 1 echo) \
    || (echo "Must be on master branch for command to work" && return 1)
}

function gitrmmerged {
    onmaster \
    && (git branch -r --merged | egrep -v 'master|release-' | sed 's/origin\///' | xargs -n 1 git push --delete origin) \
    || (echo "Must be on master branch for command to work" && return 1)
}

export JAVA_HOME=/usr/lib/jvm/default-java
export NODE_PATH="/usr/local/lib/node_modules"

function my-ip {
    hostname -I | xargs -n 1 | grep 10.10  # Using xargs just to trim whitespace
}

function check-ip {
    MY_IP=`my-ip`
    if [[ ${MY_IP} != ${DESKTOP_IP} ]] ; then echo "Expected ip ${DESKTOP_IP}; Actual ip ${MY_IP}"; fi
}

export PATH=~/.local/bin:$PATH  # Used by pip install
export PATH=~/prime/bin:$PATH  # AnalyzeRE
export AWS_PROFILE=dev
alias saml_login="saml2aws login -r us-east-2 -a"
export PRIME_PROVISIONING_API_URL=https://graphene-ohio-development-us-east-2-provisioning-api.analyzere.net
