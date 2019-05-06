#!/bin/sh

# check args
if [ $# != 1 ]; then
    echo "usage : ./do_upgrade_package.sh last_ver_path";
    exit 1;
fi

./rebar clean

./rebar compile

cd reb_web

./make_dtl.sh

cd ..

./rebar generate

./rebar generate-appups previous_release=$1

./rebar generate-upgrade previous_release=$1




