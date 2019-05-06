#!/bin/sh

# check args
if [ $# != 2 ]; then
    echo "usage : ./extract.sh dsfreg_db_txt_dump table_name";
    exit 1;
fi

if [ ! -f $1 ]; then
    echo "db dump file can't found";
    exit 1;
fi


# save args
TableName=$2;

inFile=$1;
outFile="table_"$2".import"

lineHead="{"$TableName",*";
lineEnd="*}."


# change txt read param
IFS=$'\n'


# self function
function parseLine()
{
    if [ $lineRet -eq 0 ]; then
        parseHead $1
    else
        parseBody $1
    fi
    lineRet=$?
    return 0
}

function parseHead()
{
    if [[ $1 == $lineHead ]]; then
        echo $1 >> $outFile
        if [[ $1 == $lineEnd ]]; then
            return 0;
        else
            return 1;
        fi
    else
        return 0;
    fi
}

function parseBody()
{
    echo $1 >> $outFile
    if [[ $1 == $lineEnd ]]; then
        return 0;
    else
        return 1;
    fi
}


# main code
echo "create "$outFile
cat /dev/null > $outFile 

echo "parse begin ..."

lineRet=0;

# while read line
# do
#     parseLine $line
# done <$inFile

for LINE in `cat $inFile`  
do   
    parseLine $LINE 
done



echo "parse over."

