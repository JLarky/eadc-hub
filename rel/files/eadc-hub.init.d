#! /bin/sh
### BEGIN INIT INFO
# Provides:          eadc-hub
# Required-Start:    $network $remote_fs $syslog
# Required-Stop:     $network $remote_fs $syslog
# Default-Start:     2 3 4 5
# Default-Stop:      0 1 6
# Short-Description: ADC(Advanced Direct Connect) hub software written using Erlang/OTP
# Description:       Script to start/stop eadc-hub (https://github.com/JLarky/eadc-hub).

### END INIT INFO

# Author: JLarky <jlarky@gmail.com>
#
# PATH should only include /usr/* if it runs after the mountnfs.sh script

NAME=eadc-hub
PATH=/sbin:/usr/sbin:/bin:/usr/bin
DAEMON=/usr/lib/eadc-hub/bin/eadc

# Exit if the package is not installed
[ -x "$DAEMON" ] || exit 0

# Read configuration variable file if it is present
[ -r /etc/default/$NAME ] && . /etc/default/$NAME

# Load the VERBOSE setting and other rcS variables
. /lib/init/vars.sh

$DAEMON $@
