#!/bin/bash
#
# Script to update the combined Git repository for running Travis CI.

# This variable must point to the check-out of the "combined" Travis CI
# repository which is to be updated:
: ${TRAVIS_GIT_DIR:=~/travis-ci-crowbar}
# It can be cloned from: https://github.com/crowbar/travis-ci-crowbar

function load_rvm() {
  if [[ -s "$HOME/.rvm/scripts/rvm" ]]; then
    source "$HOME/.rvm/scripts/rvm"
  elif [[ -s "/usr/local/rvm/scripts/rvm" ]]; then
    source "/usr/local/rvm/scripts/rvm"
  fi
}

function usage() {
  echo "
  Script to update the combined git repository.

    --no-fetch  Don't run ./dev fetch
    --no-sync   Don't run ./dev sync
    --gem-cache Use gem cache
    --reload    Run ./dev tests reload not ./dev tests setup
    --no-push   Do not push to remote.
    --help      Display this help message.
  "
  exit 1
}

function log() { printf "$(date '+%F %T %z'): %s\n" "$@" >&2; }
function die() { log "$*"; exit 1; }

function run() {
  local cmd="$1" msg="$2" output=""
  log "${msg:-Running '$cmd' ...}"
  if ! $cmd; then
    die "Command '$cmd' failed"
  fi
}

function update_with_dev_tool() {
  cd "$CROWBAR_DIR"
  log "Updating $CROWBAR_DIR ..."
  [[ $dev_fetch = true ]] && run "./dev fetch"
  [[ $dev_sync  = true ]] && run "./dev sync"
  log "Setting up test environment using $CROWBAR_DIR ..."
  if [[ $dev_test_mode = setup ]]; then
      if [[ $use_gem_cache = true ]]; then
          run "./dev tests setup"
      else
          run "./dev tests setup --no-gem-cache"
      fi
  elif [[ $dev_test_mode = reload ]]; then
      run "./dev tests reload"
  else
      die "BUG: invalid mode $dev_test_mode"
  fi
}

function rsync_files() {
  local DEV_TEST=/tmp/crowbar-dev-test
  cd "$TRAVIS_GIT_DIR"
  log "Copying files..."
  git reset -q --hard HEAD
  git clean -f -d -q
  rsync -aq --delete --exclude=.git/ \
    $DEV_TEST/opt/dell/{barclamps,crowbar_framework} \
    $DEV_TEST/opt/dell/crowbar_framework/Gemfile{,.lock} \
    .
}

# A number of JSON files are generated during barclamp installation. These
# files often change even though the hashes are the same due to the
# undeterministic nature of the serialization.
function remove_unchanged_files() {
  log "Checking changed files..."
  cd "$TRAVIS_GIT_DIR"
  for file in `git diff --name-only --diff-filter=M | egrep "\.(yml|yaml|json)$"`; do
    cp $file{,.tmp}
    git checkout HEAD $file
    "$TRAVIS_CI_DIR/compare" $file{,.tmp}
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
  cd "$TRAVIS_GIT_DIR"
  log "Committing files..."
  git pull
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

  if [[ $git_push = true ]]; then
      git push -q
  fi
}

dev_sync=true
dev_fetch=true
use_gem_cache=false
dev_test_mode=setup
git_push=true
for opt in "$@"; do
  case $opt in
    --no-fetch)  dev_fetch=false;;
    --no-sync)   dev_sync=false;;
    --gem-cache) use_gem_cache=true;;
    --reload)    dev_test_mode=reload;;
    --no-push)   git_push=false;;
    --help)     usage;;
    *) die "Unknown option: $opt";;
  esac
done

if [ ! -d $TRAVIS_GIT_DIR ]; then
  die "$TRAVIS_GIT_DIR does not exist"
fi

CROWBAR_DIR=$( cd `dirname $0` && cd .. && pwd ) || \
    die "Couldn't determine location of Crowbar repository"
TRAVIS_CI_DIR="$CROWBAR_DIR/travis-ci"

load_rvm
update_with_dev_tool
rsync_files
remove_unchanged_files
commit_and_push
