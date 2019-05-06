#!/bin/sh

# check args
if [ $# != 1 ]; then
    echo "usage : ./run_parse_db_export.sh db_export_txt_file_name";
    exit 1;
fi

if [ ! -f $1 ]; then
    echo "db export txt file can't found";
    exit 1;
fi

./extract_table_info.sh $1 dsconfig
#./extract_table_info.sh $1 dservice_state
./extract_table_info.sh $1 svcdef
./extract_table_info.sh $1 svcdef_idl


