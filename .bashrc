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
  [[ $(/usr/local/git/bin/git status 2> /dev/null | tail -n1) != "nothing to commit (working directory clean)" ]] && echo "*"
}
function parse_git_branch {
  /usr/local/git/bin/git branch --no-color 2> /dev/null | sed -e '/^[^*]/d' -e "s/* \(.*\)/[\1$(parse_git_dirty)]/"
}

PS1='\w$(parse_git_branch): '
# PS1="$HOST> "
PS2=">> "


# date=`/bin/date`
# day="${date:0:3}, ${date:4:6}, ${date:24:4}"

################################################################
# ALIASES
################################################################
alias du="du -h"
alias ant="ant test-noselenium"
alias cgsuite="java -classpath ~/Desktop/cgsuite/out cgsuite.ui.Main"
alias cgsuite="cd ~/Desktop/cgsuite-0.7 ; java -Xmx512m -jar ./cgsuite.jar"
alias tomcat="~/tomcat/bin/startup.sh"
alias shutdown="~/tomcat/bin/shutdown.sh"
alias s="ssh $w"
alias z="ssh zeus.gac.edu -l wolfe"
alias z="ssh zariski.mcs.gac.edu"
alias v="ssh mcs-jsp.gac.edu -l wolfe"
alias tom="ssh mcs-jsp.gac.edu -l tomcat"
alias e=emacs
alias g=egrep
alias gdb="gdb -q"
alias t=less
alias emacs="emacs -geometry 80x55" #  -fn 9x15
alias emacs2="'emacs' -fn 12x24"
if [ `hostname` == cart.mcs.gac.edu ]
   then alias emacs="emacs -geometry 103x40 -fn 12x24"
fi

alias remake="make --always-make"
alias cp="cp -i"
alias mv="mv -i"
alias ghostview="gv"
alias ls="ls -F"  # -C
alias l="ls -A"
alias f="finger -s"
# alias dvips="dvips -t letter"
alias lookup="cat ~/Dropbox/notes/address/* | grep -i"
alias grep="grep -s"
alias egrep="egrep -s"
alias fgrep="fgrep -s"
alias ssh="ssh -x"
alias from='grep ^From: $MAIL'
if [ $HOST == Micah ]
then
    alias pdf="acroread"
else
    alias pdf="xpdf"
fi
alias ppt="ooimpress"

if [[ "$OSTYPE" == *"linux"* ]] ; then alias open="xdg-open"; fi
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
function fix {
    killall ggv-postscript-viewer >& /dev/null
    find ~/.mozilla -name '*lock*' -exec rm {} \;
    find . -name '*' -exec ls -d {} \; > /dev/null
    find ~/www-docs/text -name '*' -exec ls -d {} \; > /dev/null
}
export -f fix

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

function www {
    if ( checksingletex $* ) ; then
        latex $*
        dvips $* -o $*.ps
        # pdflatex $*
        ps2pdf $*.ps $*.pdf
        chmod og+r $*.ps
        mv -f $*.ps $*.pdf www/
    fi
}
export -f www

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

function sp {
    if ( checksingletex $* ) ; then
        latex $*
        dvips -t landscape $* -o $*.ps
        ps2pdf $*.ps $*.pdf
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

alias rsynccommand='rsync -CrlpgoDuzb --exclude "*~"'
function rsyncbase {
  HOMEDIR=~/
  RHOME=wolfe@dan.mcs.gac.edu
  PWD=`pwd`/
  RDIR=`echo $PWD | sed s%$HOMEDIR%${RHOME}:%`
export -f rsyncbase
}

function put {
  rsyncbase
#  rsync -Cavuzb --exclude "*~" . $RDIR # put
  rsynccommand . $RDIR # put
}
export -f put

function get {
  rsyncbase
#  rsync -avuzb --exclude "*~" $RDIR . # get
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
function namename()
{
  local name=${1##*/}
  local name0="${name%.*}"
  echo "${name0:-$name}"
}
function ext()
{
  local name=${1##*/}
  local name0="${name%.*}"
  local ext=${name0:+${name#$name0}}
  echo "${ext:-.}"
}

function logs {
    killall tail
    for i in /Applications/MAMP/logs/*error* /Applications/MAMP/logs/*query*
      do
      echo $i
      rm $i
      touch $i
      tail -f $i &
      done
}

function alllogs {
    killall tail
    for i in /Applications/MAMP/logs/*
      do
      rm $i
      touch $i
      tail -f $i &
      done
}

function clearcaches {
    rm -f ~/dashboard/cache/*/*
}


function stusysh { export db=stusys_halifax dbl=stusys_halifax dbp=17011 ; }
function stusysa { export db=stusys_antigonish dbl=stusys_antigonis dbp=17011 ; }
function stusyst { export db=stusys_test dbl=stusys_test dbp=17011 ; }
function sheepdog { export db=sheepdog_new dbl=sheepdog_new dbp=kn8szo2h ; }
function cleanns { export db=cleanns_live dbl=shelley dbp=cleanNS11 ; }
# function drupal { export db=drupal dbl=drupal dbp=ljf2Ao9X ; }
function crm { export db=crmbeta dbl=norexdb dbp=d8sk2md9el ; }
function letsrent { export db=letsrent3 dbl=letsrent3 dbp=86aweKAc ; }
function letsrent1 { export db=letsrent dbl=chesterdev dbp=cribs ; }
function letsrent2 { export db=letsrent2 dbl=chesterdev dbp=cribs ; }
function clayton { export db=claytonp_newsletter dbl=claytonp_news dbp=autron902 ; }
function jameson { export db=jamesonconsult dbl=jameson dbp=consult902 ; } #jameson@woz
function nsboats { export db=nsba dbl=nsba dbp=h3G6dq4j ; }
function hc { export db=healthconnex dbl=healthconnex dbp=hAJe3heP ; }
function heart { export db=heartofthetree dbl=heartofthetree dbp=sP32Uphe ; }
function heart2 { export db=heartoftreetest dbl=heartoftreetest dbp=sP32Uphe ; }

function root { export dbl=root dbp=root db= ; }
function scotts { export dbl=scotts dbp=DN2Fbg6sB5YM db=scotts ; }
function novi { export dbl=Novi dbp=norexnovibrokers db= ; }

function dash { `(php -- $* | cat) << '//END_OF_INPUT'
<?php
$dir = ($argc <= 1) ? "dashboard" : "$argv[1]";
$dbase = null;
if (file_exists("$dir/include/db-config.php")) {
  include_once "$dir/include/db-config.php";
} else if (file_exists("$dir/wp-config.php")) {
  $strs = explode("\n", file_get_contents("$dir/wp-config.php"));
  $strs = preg_grep("/^require_once|^<.php/", $strs, PREG_GREP_INVERT);
  eval(implode("\n", $strs));
  $dbase = DB_NAME;
  $dbuser = DB_USER;
  $dbpass = DB_PASSWORD;
}
echo $dbase ? "export db=$dbase dbl=$dbuser dbp=$dbpass" : "echo Credentials for $dir not found";
//END_OF_INPUT
`
}


function createperm {
    if [ $# == 1 ]
	then _createdb_helper $1 justperms
	else echo "createperm takes exactly one argument, the name of the repository"
	fi
}

function _createdb_helper {  # Set login credentials for by reading the db-config.php file
        dash $1
        if [ $# == 2 ]
        then command="grant all on $db.* to $dbl@localhost identified by '$dbp' ; flush privileges ; "
        else command="grant all on $db.* to $dbl@localhost identified by '$dbp' ; create database $db ;"
        fi
		echo $command
        root
        echo $command | mysqln
        dash $1
}

function createdbc {  # Echo the right mysql command for createdb
    if [ $# == 1 ]
	then
		command="grant all on $db.* to $dbl@localhost identified by '$dbp' ; create database $db ;"
		echo $command
	else
		echo "createdb takes exactly one argument, the name of the repository"
	fi
}

function createmysqldb {  # Set login credentials for by reading the db-config.php file
    if [ $# == 1 ]
	then
		dash $*
		command="grant all on $db.* to $dbl@localhost identified by '$dbp' ; create database $db ;"
		echo $command
        root
        echo $command | mysqln
        dash $*
	else
		echo "createdb takes exactly one argument, the name of the repository"
	fi
}

# VERSION THAT WORKED WITH MAMP
function mysqlp { ln -sf /Applications/MAMP/tmp/mysql/mysql.sock /tmp/mysql.sock ; mysql -u $dbl -p$dbp $*; }     # (not used often -- login to mysql, do not select db)
function mysqlp { mysql -u $dbl -p$dbp $*; }     # (not used often -- login to mysql, do not select db)
function mysqln { mysqlp $db $* ; }               # Run mysql on the current db
function mysqlc { echo mysql -u $dbl -p$dbp $db;} # Show me the mysql command which I would type to
function backupdb {
  if [ $# == 1 ]
    then host=$1
  else host='localhost'
  fi
  echo Backing up db $db on host $host
  file=~/tmp/backup-$db-$host-`date "+%Y-%m-%d@%H:%M"`.db
  mysqld -h $host > $file
  cat $file | grep 'CREATE'
}
function mysqld { mysqldump -u $dbl -p$dbp $db $* ; } # Dump the db
function pushbuzz {
   echo "Backing up to ~/tmp/buzz.db"
   mysqld -h buzz.norex.ca > ~/tmp/buzz.db
   echo "Pushing local db to buzz"
   mysqld | mysqln -h buzz.norex.ca
   echo "Done"
}
function pullbuzz {
   echo "Backing up to ~/tmp/local.db"
   mysqld > ~/tmp/local.db
   echo "Pulling local db from buzz"
   mysqld -h buzz.norex.ca | mysqln
   echo "Done"
}
function log {
  echo `date` $* >> ~/timecards/toprocess
  echo `date` $* >> ~/timecards/bydate/`date "+%Y-%m-%d"`
}

function rgrep { grep -r -i $1 .  --include=$2 | cut -c -240 ; }
function dgrep { grep -r -i $1 .  --include=$2 | egrep -v '/core/PEAR|core/tinymce'; }
function rmsvns { find . -name '.svn' -prune -exec rm -r -f {} \; ; }
function rmpycs { find . -name '*.pyc' -exec rm -f {} \; ; }
function rmgits { find . -name '.git*' -prune -exec rm -r -f {} \; ; }

function mysqlnolog {
         killall mysqld
         /Applications/MAMP/Library/bin/mysqld_safe --port=3306 --socket=/Applications/MAMP/tmp/mysql/mysql.sock --lower_case_table_names=0 --pid-file=/Applications/MAMP/tmp/mysql/mysql.pid --log-error=/Applications/MAMP/logs/mysql_error_log $* &

}
function mysqllog {
		 mysqlnolog --general_log=1 --general_log_file=/Applications/MAMP/logs/mysql_query_log
}
# set global general_log="ON";
# set global general_log_file="/Applications/MAMP/logs/mysql_query_log";

function mysqlslow {
		 mysqlnolog --log-slow-queries=/Applications/MAMP/logs/mysql_query_log --long_query_time=1
}
function mysqlbigpack {
		 mysqlnolog --max_allowed_packet=64M
}

export svntrunk=svn://svn.clientview.ca/norex2/trunk
export svndavid=svn://svn.clientview.ca/norex2/branches/david

function fix {
  for i in $*
    do
	svn export $i ${i}-new
    done
  echo -n "Click enter after editing properties to remove dependence on $*:"
  read $dummy
  rm -rf $*
  for i in $*
    do
      mv $i-new $i
      svn add $i
      svn commit $i -m "Making $i module project-specific"
    done
}


function fix {
  # export EDITOR='/bin/cat'
  # export VISUAL=$EDITOR
  # modules=`svn propedit svn:externals ../modules | cut -d ' ' -f 1 | grep -v '^No$'`
  modules=`echo *`
  echo -n "Modules that need updating: $modules.  Return to continue, control-C to exit"
  read $dummy
  for i in $modules
   do
     svn copy -rBASE $i $i.new
   done
  echo -n "About to commit changes to $modules.  Return to continue, control-C to exit"
  read $dummy
  svn propdel svn:externals .
  svn commit -m "removing modules $modules"
  for i in $modules
   do
     rm -r -f $i
     svn mv $i.new $i
   done
  svn commit -m "re-adding modules $modules"
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
		echo "cat >"$i" <<'//GO.SYSIN DD *'"
		cat $i
        if [ `tail -1 $i | wc -l` = 0 ] # file ends in no newline
        then echo ; echo "Appended newline to file $i" > /dev/stderr
        fi
		echo "//GO.SYSIN DD *"
	done
}

function phonenet {
	killall adb
    ~/sdk-android/tools/adb forward tcp:8080 tcp:8080
	echo "Go into Firefox preferences => Advanced => Network tab => Settings"
	echo "Set manual HTTP Proxy and SSL Proxy BOTH to localhost Port 8080"
}

function tea {
  sleep 150
  say your tea is ready David
}

function clear {
  echo "delete from confirmation_required; delete from mail_users; " | mysqln
}

function list {
  echo "select id,timestamp,password from confirmation_required; select * from mail_users; " | mysqln
}

function source_if_exists { if [ -e $1 ] ; then source $1 ; fi }

alias ip=ifconfig

source_if_exists $HOME/bin/git-completion.bash

export s=wolfe@199.204.208.10
alias s="ssh $s"
alias m="python manage.py"
function cucns-getdb {
  pushd ~/tmp
  scp cucns@cuzone.ca:dbdumps/latest_dump.tar.gz .
  tar xfz latest_dump.tar.gz
}
alias mom="ssh dwolfe@bas.homelinux.com -p 3333"
alias momexec="ssh hcxmom@bas.homelinux.com -p 3333 "
alias mom7="ssh sheepdog@bas.homelinux.com -p 26"
export cucns=cucns@204.244.124.128

export WORKON_HOME=$HOME/envs
export VIRTUALENVWRAPPER_PYTHON=`which python2.7`
source_if_exists source /usr/local/bin/virtualenvwrapper.sh
alias iwk-env="source ~/envs/iwk/bin/activate"
alias milely-env="source ~/envs/milely/bin/activate"
alias env="echo Use w instead of env"
copy_function workon old_workon
function workon {
    old_workon $1
    if [ -e ~/src/$1 ] ; then cd ~/src/$1 ; fi
}
alias iwk-env2="source ~/python-environments/iwk-env2/bin/activate"
alias django-env="source ~/python-environments/django/bin/activate"
function rrun0 {
    sudo rabbitmqctl stop
    sleep 3
    sudo rabbitmq-server &
    sleep 10
    sudo rabbitmqctl add_user iris 'Me887_{'
    sudo rabbitmqctl add_vhost iris
    sudo rabbitmqctl add_user iwk 'Me887_{'
    sudo rabbitmqctl add_vhost iwk
    sudo rabbitmqctl set_permissions -p iwk iwk ".*" ".*" ".*"
    sudo rabbitmqctl set_permissions -p iris iris ".*" ".*" ".*"
    ./manage.py celeryd &
}
alias rrun1="rm dev.db ; find . -name '*.pyc' -exec rm {} \; ; m syncdb --noinput ; m migrate ; python generate.py"
alias rrun2="m createsuperuser --user=wolfe --email=wolfe@gac.edu; m createinitialrevisions ; m runserver"
alias rrun="rrun1 ; rrun2"
alias crun="rm dev.db ; m syncdb --noinput ; m createsuperuser --user=wolfe --email=wolfe@gac.edu; m createinitialrevisions ; m runserver"
alias run="rmpycs ; m syncdb ; m migrate ; m runserver"
alias pushb="git checkout healthconnexb ; git pull . healthconnexa ; git push healthconnexb healthconnexb ; git checkout healthconnexa"
function mtest {
    rm -f settings_local.pyc
    mv settings_local.py settings_local.py.bak
    if [ $# == 0 ]
    then
        fab test
    else
        m test $*
    fi
    mv settings_local.py.bak settings_local.py
}

alias php=/Applications/MAMP/bin/php/php5.2.17/bin/php
# export PATH=/System/Library/Frameworks/Python.framework/Versions/2.6/bin:$PATH
export PATH=/Users/wolfe/.gem/ruby/1.8/bin:$PATH
alias ssh-miley="ssh -i ~/.ssh/milely.pem ec2-user@ec2-184-72-132-17.compute-1.amazonaws.com"

function mamp {
    /usr/local/bin/mysql.server stop
    export PATH=/Applications/MAMP/Library/bin:$PATH
    open /Applications/MAMP
}
function nomamp {
    echo "Be sure that MAMP has been stopped"
    export PATH=/usr/local/bin:$PATH
    /usr/local/bin/mysql.server start
}
export PATH="/Library/PostgreSQL/9.2/bin:$PATH"

function reset-iwk-mysql {
    export db=iwk dbl=iwk dbp=c6fcca503ae74900e410e9e4e005cfad ;
    echo "drop database iwk; create database iwk;" | mysqln
    cat $1 | mysqln
}

function reset-iwk {
    echo "type rdb iwk $1"
}
function reset-milely {
    echo "type rdb milely $1"
}
function rdb {
    if [ $# == 2 ]
    then
        (echo "drop database $1; create database $1;" | psql -v ON_ERROR_STOP=1) && cat $2 | psql $1 > /dev/null
    else
        echo "rdb takes two arguments, db name and file name"
    fi
}

export PGPASSWORD="LilaTov"

# I have some postgres configuration issues that are solved by:
# ln -s -f /private/tmp/.s.PGSQL.5432 /var/pgsql_socket/

################################################################
# MAMP
################################################################

export EVENT_NOKQUEUE=1 # For memcached to work with MAMP
# export PATH=/Applications/MAMP/Library/bin:$PATH
function runher {
    python src/manage.py syncdb --settings=settings-heroku;
    python src/manage.py migrate --settings=settings-heroku;
    python src/manage.py runserver --settings=settings-heroku
}
alias gphm="git push heroku master"
alias gpfhm="git push --force heroku master"

# export PATH=/Users/wolfe/envs/iris/bin:/Library/Frameworks/Python.framework/Versions/2.7/bin:/opt/local/bin:/opt/local/sbin:/Library/Frameworks/Python.framework/Versions/2.6/bin:/opt/local/bin:/opt/local/sbin:/opt/local/bin:/opt/local/sbin:/Library/Frameworks/Python.framework/Versions/2.6/bin:/Library/Frameworks/Python.framework/Versions/Current/bin:/Applications/MAMP/Library/bin:/Users/wolfe/.gem/ruby/1.8/bin:/System/Library/Frameworks/Python.framework/Versions/2.6/bin:/usr/texbin:/Users/wolfe/bin/test:/Users/wolfe/bin/pyflakes-0.5.0:/Users/wolfe/bin/bin:/usr/sbin:/sw/bin:/Users/wolfe/bin/goprinting:/usr/local/git/bin:/Users/wolfe/sw/ImageMagick-6.3.7/bin:/usr/bin:/bin:/usr/sbin:/sbin:/usr/X11/bin:/usr/local/git/bin:/Users/wolfe/bin


PATH=$PATH:$HOME/.rvm/bin # Add RVM to PATH for scripting
# [[ -s "$HOME/.rvm/scripts/rvm" ]] && . "$HOME/.rvm/scripts/rvm"

# bundle coach_view/static/css/demo_bar.less iwk_messages/static/datatables/css/style.less static/css/*.less static/img/sprite/icon.less survey/static/css/*.less > bun
alias run_scotts='dev_appserver.py --admin_console_server= --port=8080 /Users/wolfe/scotts-app'
alias deploy_scotts='cd ~/scotts-app ; appcfg.py backends . update ; appcfg.py update .'

# In Postgres:
# create role wolfe with password 'lkj2309';
# create database randomdb;
# grant all privileges on database randomdb to wolfe;

alias pipsheep='pip install -f https://s3.amazonaws.com/sheepdog-assets/feta/index.html --no-index'

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
export PATH=/Library/PostgreSQL/9.2:$PATH:/usr/local/share/npm/bin

function redist {
    python setup.py sdist && pip uninstall $* && pip install dist/*
}
### Added by the Heroku Toolbelt
export PATH=/usr/local/heroku/bin:$PATH
export PATH=/Applications/Postgres.app/Contents/MacOS/bin:$PATH

export STELLA_OUTBOUND_MAIL=david.wolfe@sheepdog.com
function auto_fix {
    if [ $# != 1 ]; then echo Enter exactly one arguments; return 1; fi
    if [ ! -d $1 ]; then echo Argument must be an app; fi
    if [ ! -f $1/models.py ]; then echo Argument must be an app; fi
    if [ ! -f $1/migrations/0001_initial.py ]; then echo App requires initial south migration; fi
    if [ ! -f `ls $1/migrations/0002*` ]; then echo App requires an additional migration after the iniatial; fi
    if [ ! -f manage.py ]; then echo Run from package root; fi
    files=`ls $1/migrations/0*.py`
    echo ${files: -2}
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

if [ -d /usr/local/MATLAB ]; then MATLAB_PATH=/usr/local/MATLAB/R2015b; fi

## QRA
  export CLANG_LLVM_ROOT="${HOME}/clang-llvm"
  export MATLAB_ROOT="/usr/local/MATLAB/R2015b"
  export QCLANG_ROOT=${HOME}/qclang
  export QVTRACE_ROOT=${HOME}/qvtrace

alias qanalyze=${HOME}/qclang/scripts/qanalyze

function rebuild {
    rm -f ~/qvtrace/devops/resources/tomcat/logs/catalina.out
    rm -f ~/qvtrace/models/*
    pushd ~/qvtrace/devops > /dev/null
    mvn install -DskipTests && ./launch
    popd > /dev/null
}

function r {
    rebuild && sleep 1
}

function relaunch {
    pushd ~/qvtrace/devops > /dev/null
    ./launch
    popd > /dev/null
}

function meld2 {
    meld ~/qvtrace/testing/models/$1 ~/qvtrace/testing/models/tmp/$1
}

function cp2 {
    cp ~/qvtrace/testing/models/tmp/$1 ~/qvtrace/testing/models/$1
}

function meldsort2 {
    IGNORE='^\s*"(sourceIOID|targetIOID)":'
    IGNORE='^\s*"(name": ".*\d{4}(-\d\d){2}T(\d\d-){3}\d{3}|id|_id|x|y|lz|sourceIOID|targetIOID)"'
    meld <(sort $1     | grep -Ev "${IGNORE}") \
         <(sort tmp/$1 | grep -Ev "${IGNORE}")
}

function do2 {
    cd ~/qvtrace/parsers \
        && git stash && git checkout master && git stash pop \
        && (mvn test -Dtest=TmpTest | tee /tmp/a) \
        && git stash && git checkout matrix-node && git stash pop \
        && (mvn test -Dtest=TmpTest | tee /tmp/b)
}

# grep Object bun | grep -v '\* @see' | egrep -v '^ *//' | wc

function ppjson {
    cat $1 | python -c'import fileinput, json; print(json.dumps(json.loads("".join(fileinput.input())), indent=2))' | sed 's/[ \t]*$//'
}

function trm {
    rm ~/qvtrace/testing/models/qra/const_matrix/valid.q?t
    rm ~/qvtrace/testing/models/qra/lookup_1d_linear_floatIn_floatOut/valid.q?t
    rm ~/qvtrace/testing/models/qra/switch_16bit_8way/valid.q?t
    rm ~/qvtrace/testing/models/lm-contract1/2_tustin/r4a.q?t
    rm ~/qvtrace/testing/models/lm-contract1/2_tustin/r4b.q?t
}
function tpop {
    git checkout ~/qvtrace/testing/models/qra/const_matrix/
    git checkout ~/qvtrace/testing/models/qra/lookup_1d_linear_floatIn_floatOut/
    git checkout ~/qvtrace/testing/models/qra/switch_16bit_8way/
    git checkout ~/qvtrace/testing/models/lm-constract1/2_tustin/
    git checkout ~/qvtrace/testing/models/lm-constract1/2_tustin/
}
