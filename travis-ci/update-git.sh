#!/bin/bash
#
# Script to update the combined Git repository for running Travis CI. Intended
# to be called by a cron job, like so:
#
# */5 * * * * cd ~/crowbar/travis-ci && ./update-git.sh >>update-git.log 2>&1

GIT_DIR=~/travis-ci-crowbar_framework

function usage() {
  echo "
  Script to update the combined git repository.

    --no-push   Do not push to remote.
    --help      Display this help message.
  "
  exit 1
}

function log() { printf "$(date '+%F %T %z'): %s\n" "$@" >&2; }
function die() { log "$*"; exit 1; }

function run() {
  local cmd="$1" msg="$2" output=""
  [ "$msg" ] && log "$msg"
  output=`$cmd 2>&1`
  if [ $? -ne 0 ]; then
    log "Command failed: $cmd"
    die "Output: $output"
  fi
}

function update_with_dev_tool() {
  cd ..
  run "./dev fetch" "Running ./dev fetch..."
  run "./dev sync"  "Running ./dev sync..."
  run "./dev setup-unit-tests --no-gem-cache" \
      "Running ./dev setup-unit-tests --no-gem-cache"
}

function rsync_files() {
  local TMP_DIR=/tmp/crowbar-dev-test/crowbar_framework
  cd $GIT_DIR
  log "Copying files..."
  git reset -q --hard HEAD
  git clean -f -d -q
  rsync -aq --delete --exclude=BDD/*.beam $TMP_DIR/{../Gemfile,*} .
}

# A number of JSON files are generated during barclamp installation. These
# files often change even though the hashes are the same due to the
# undeterministic nature of the serialization.
function remove_unchanged_files() {
  log "Checking changed files..."
  cd $GIT_DIR
  for file in `git diff --name-only --diff-filter=M | egrep "\.(yml|yaml|json)$"`; do
    cp $file{,.tmp}
    git checkout HEAD $file
    $curr_dir/compare $file{,.tmp}
    if [ $? -eq 0 ]; then
      # File didn't really change, so remove it
      rm $file.tmp
    else
      # Restore changed file
      mv $file.tmp $file
    fi
  done
}

function commit_and_push() {
  local output="" git_hash=""
  cd $GIT_DIR
  log "Committing files..."
  git add *

  output=`git commit -a -m "Update to latest in https://github.com/crowbar/crowbar master" 2>&1`
  if [ $? -ne 0 ]; then
    if echo $output | grep -q "nothing to commit"; then
      log "Nothing to commit"
      git_push=false
    else
      log "Commit failed:"
      die $output
    fi
  else
    git_hash=`echo $output | awk '{print $2}' | cut -d']' -f1`
    log "Commited $git_hash!"
  fi

  [[ $git_push = true ]] && git push -q
}

git_push=true
for opt in "$@"; do
  case $opt in
    --no-push) git_push=false;;
    --help)    usage;;
    *) die "Unknown option: $opt";;
  esac
done

if [ ! -d $GIT_DIR ]; then
  die "$GIT_DIR does not exist"
fi

curr_dir=`pwd`

update_with_dev_tool
rsync_files
remove_unchanged_files
commit_and_push

cd $curr_dir
