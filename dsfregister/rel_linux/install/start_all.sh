#!/bin/sh
su - unicorn -c "/opt/unicorn/dsfreg/bin/unicorn start" & /opt/unicorn/monitor/bin/monitor start

