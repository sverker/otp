#!/bin/bash

NEW_BEAM=/home/garazdawi/git/otp/bin/cerl
OLD_BEAM=/home/garazdawi/git/otp2/bin/cerl

MAX_SCHEDULER=`expr $1 - 1`

$OLD_BEAM +S $MAX_SCHEDULER -sname test_server -pz "/home/garazdawi/git/otp/release/tests/test_server" -eval "ts_run:ct_run_test(\"/home/garazdawi/git/otp/release/tests/asn1_test\", [{abort_if_missing_suites,true},{suite,asn1_SUITE},{logdir,\"../test_server\"},{config,[\"../test_server/ts.config\",\"../test_server/ts.unix.config\"]},{vars,[{verbose,0}]},batch,{scale_timetraps,true},{testcase, testTimer_ber}])"  -noinput -s erlang halt

rm -rf bm.new
for i in $(seq 0 $MAX_SCHEDULER)
do
    taskset -c 0-$i $NEW_BEAM -sname test_server -pz "/home/garazdawi/git/otp/release/tests/test_server" -eval "ts_run:ct_run_test(\"/home/garazdawi/git/otp/release/tests/asn1_test\", [{abort_if_missing_suites,true},{suite,asn1_SUITE},{logdir,\"../test_server\"},{config,[\"../test_server/ts.config\",\"../test_server/ts.unix.config\"]},{vars,[{verbose,0}]},batch,{scale_timetraps,true},{testcase, testTimer_ber}])"  -noinput -s erlang halt >> bm.new
done

rm -rf bm.old
for i in $(seq 0 $MAX_SCHEDULER)
do
    taskset -c 0-$i $OLD_BEAM -sname test_server -pz "/home/garazdawi/git/otp/release/tests/test_server" -eval "ts_run:ct_run_test(\"/home/garazdawi/git/otp/release/tests/asn1_test\", [{abort_if_missing_suites,true},{suite,asn1_SUITE},{logdir,\"../test_server\"},{config,[\"../test_server/ts.config\",\"../test_server/ts.unix.config\"]},{vars,[{verbose,0}]},batch,{scale_timetraps,true},{testcase, testTimer_ber}])"  -noinput -s erlang halt >> bm.old
done

grep encoding bm.new | awk 'BEGIN{ cnt = 0; sched = 1; } { if ( cnt % 4 == 2) {printf sched " " $3 " " $6 " "; sched++} if (cnt %4 == 3) {print $3 " " $6} cnt++; }' > bm.new.data

grep encoding bm.old | awk 'BEGIN{ cnt = 0; sched = 1; } { if ( cnt % 4 == 2) {printf sched " " $3 " " $6 " "; sched++} if (cnt %4 == 3) {print $3 " " $6} cnt++; }' > bm.old.data

gnuplot draw.gplot
