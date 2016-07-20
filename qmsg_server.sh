#!/bin/bash
PORT=8999
PROG=`basename $0`
FILTER=~/tmp/tmp_filter_.sh

cat << EOF > $FILTER
#!/bin/bash

while true
do
    read CMD
    USER_ID=\$(echo \$CMD | grep -o "uid,[^}]*" | awk -F, '{print \$2}')
    echo "**##[{uid,\$USER_ID},{num,\$RANDOM}]##**"
done
EOF

chmod a+x $FILTER

NUM=$(ps -ef|grep ncat | grep ${PORT} | grep -v grep | wc -l)

if [ $NUM -gt 0 ];then
    echo "$PROG already running ..."
    exit 1
fi

if [ -x "$(command -v ncat)" ]; then
    echo "$PROG starting now ..."
    ncat -4 -k -l $PORT -e $FILTER &
else
    echo "no exists ncat command, please install it ..."
fi
