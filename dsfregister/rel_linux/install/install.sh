#/bin/bash
if [ `whoami` = root ]
then
  echo "install..."
  echo "set limit nofile..."
  LIMIT_FILE=/etc/security/limits.conf
  if grep "^\* *soft *nofile *[0-9]*" $LIMIT_FILE > /dev/null
  then
    sed 's/^\* *soft *nofile *[0-9]*/\*       soft    nofile   10240/' $LIMIT_FILE > limits.conf.temp
    mv limits.conf.temp $LIMIT_FILE
  else
    echo '*       soft    nofile   10240' >> $LIMIT_FILE
  fi

  if grep "^\* *hard *nofile *[0-9]*" $LIMIT_FILE > /dev/null
  then
    sed 's/^\* *hard *nofile *[0-9]*/\*       hard    nofile   32768/' $LIMIT_FILE > limits.conf.temp
    mv limits.conf.temp $LIMIT_FILE
  else
    echo '*       hard    nofile   32768' >> $LIMIT_FILE
  fi
  echo "chown run dir..."
  chown unicorn.unicorn -R /opt/unicorn
  chmod a+x /opt/unicorn/dsfreg/bin/unicorn /opt/unicorn/dsfreg/bin/install_upgrade.escript
  chmod a+x /opt/unicorn/monitor/bin/monitor /opt/unicorn/monitor/bin/install_upgrade.escript
  echo "add dsfreg autostart script"
  cp autostart.in /etc/rc.d/init.d/unicorn
  chmod a+x /etc/rc.d/init.d/unicorn
  echo "add unicorn as system service"
  /sbin/chkconfig --add unicorn
else
  echo "ERROR: please use root user run the script !!!!!!!"
fi
