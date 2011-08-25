REM CLEANUP
del erl_crash.dump
del *.beam

REM Compile
erlc bdd_utils.erl
erlc bdd_webrat.erl
erlc bdd_catchall.erl
erlc bdd_selftest.erl
erlc crowbar.erl
erlc swift.erl
erlc bdd.erl

REM Self Diagnostics
erl -s bdd_selftest test all -s init stop -noshell
