# Executed by default by interactive (but not login) scripts, but since
# this is included by .bash_profile, this is called by all.

source $HOME/.shellrc.sh

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


if [[ "$OSTYPE" == *"linux"* ]] ; then
    function open {
        for i in "$@"; do xdg-open "$i"; done
    }
fi

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

function lx {
    if ( checksingletex $* ) ; then
        latex $*
        xdvi $*
    fi
}

function lp {
    if ( checksingletex $* ) ; then
        latex $*
        dvips $* -o $*.ps
        ps2pdf -dAutoRotatePages=/None $*.ps $*.pdf
        open $*.pdf
    fi
}

function swap {
  # Check that the user provides exactly two arguments, and that files exist
  if [ $# != 2 ]; then echo Enter exactly two arguments; return 1; fi
  if [ ! -e $1 ]; then echo file $1 does not exists ; return 1; fi
  if [ ! -e $2 ]; then echo file $2 does not exists ; return 1; fi
  if [[ $2 == $1 ]]; then echo files are identical ; return 1; fi
  mv $1 swaptemp.tmp
  mv $2 $1
  mv swaptemp.tmp $2
}

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

function get {
  rsyncbase
  rsynccommand $RDIR . # get
}

function sync {
  echo "* PUT *"
  put
  echo "* GET *"
  get
}

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

PROTECTED_BRANCHES="master|matisse-*"

function _git_ls_merged_remote_ {
    _PROTECTED_BRANCHES="$(git rev-parse --abbrev-ref HEAD)|${PROTECTED_BRANCHES}"
    git branch -r --merged | grep -E -v "${_PROTECTED_BRANCHES}" | xargs -n 1 echo
}
function _git_ls_merged_local_ {
    _PROTECTED_BRANCHES="$(git rev-parse --abbrev-ref HEAD)|${PROTECTED_BRANCHES}"
    git branch --merged | grep -E -v "${_PROTECTED_BRANCHES}" | xargs -n 1 echo
}

function gitlsmerged {
    git fetch --all --prune
    echo "================================================================================"
    for var in $(_git_ls_merged_local_); do
        echo git branch --delete "$var"
    done
    for var in $(_git_ls_merged_remote_); do
        echo git push --delete $(cut -d '/' -f 1 <<< "$var") $(cut -d '/' -f 2- <<< "$var")
    done
}

function gitrmmerged {
    for var in $(_git_ls_merged_local_); do
        git branch --delete "$var"
    done
    for var in $(_git_ls_merged_remote_); do
        git push --delete $(cut -d '/' -f 1 <<< "$var") $(cut -d '/' -f 2- <<< "$var")
    done
}

# export JAVA_HOME=/usr/lib/jvm/default
# export NODE_PATH="/usr/local/lib/node_modules"

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
export PATH=~/bin:$PATH
export PATH=./node_modules/.bin:$PATH  # Use local npm before global

# stty intr ^J  # So I can map ^C to copy

source_if_exists $HOME/analyzere/users/wolfe/.bashrc

function npmfind {
    find $1 -not \( -name node_modules -prune \) "${@:2}"
}

function terror {
    terraform "$1" -var-file=config/$(terraform workspace show).tfvars "${@:2}"
}

function check-staged-changes {
    docker run -v $(pwd):/path zricethezav/gitleaks:latest protect --staged -v \
       --source="/path" \
       -l="debug" \
       --report-path /path/gitleaks-report.json \

    if [[ $(cat gitleaks-report.json | wc -c) == 3 ]]
    then
        rm gitleaks-report.json
    else
       echo "Error: gitleaks has detected sensitive information in your changes. Please remove the secret(s) highlighted in gitleaks-report.json file and retry."
       echo "Aborting commit."
       echo "You can circumvent this git hook with --no-verify."
    fi
}

function gitleaks {
    docker run -v $(pwd):/path zricethezav/gitleaks:latest detect \
           --source="/path" \
           --report-path /path/gitleaks-report.json
    if [[ $(cat gitleaks-report.json | wc -c) == 3 ]]
    then
        rm gitleaks-report.json
    else
        grep RuleID gitleaks-report.json  | sort | uniq -c > gitleaks-summary.txt
    fi
}

function gitleaks-dirs {
    for i in */ # are-platform matisse #
    do
        (cd $i ; /usr/bin/cp -f ../.gitleaks.toml . ; gitleaks $i >& gitleaks.out)
    done
}

#THIS MUST BE AT THE END OF THE FILE FOR SDKMAN TO WORK!!!
export SDKMAN_DIR="$HOME/.sdkman"
[[ -s "$HOME/.sdkman/bin/sdkman-init.sh" ]] && source "$HOME/.sdkman/bin/sdkman-init.sh"

function gitblame {
    git blame $* | while read hash others;
    do
        echo $hash $(git log -1 --pretty='%<(12,trunc)%s' $hash) '|' $others
    done
}

function find {
    /usr/bin/find $1 -name node_modules -prune -o "${@:2}"
}

eval "$(pyenv init --path)"
