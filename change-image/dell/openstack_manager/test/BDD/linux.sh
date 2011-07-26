if [ ! -e /usr/bin/erl ]
then
  apt-get -y install erlang-base erlang-inets
fi

# Clean-up
rm -f erl_crash.dump
rm -f *.beam

# Compile
erlc bdd_utils.erl
erlc bdd_webrat.erl
erlc bdd_catchall.erl
erlc bdd_selftest.erl
erlc crowbar.erl
erlc swift
erlc bdd.erl

# Run Self-test
erl -s bdd_selftest test all -s init stop -noshell

