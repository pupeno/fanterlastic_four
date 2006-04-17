#!/usr/bin/env bash

# Exit reasons:
# 0: Success
# 1: Unrecognized option.
# 2: Unrecognized action (should be either "start" or "stop").
# 3: Can't start, already runnig (or so it seems).
# 4: Can't stop, not running (or so it seems).

#==============================================================
#Defaults, can be customized in CONFIGFILE
NAME="fanterlasticfour"

NAMEFILE="/var/pid/fanterlasticfour"
LOGDIR="/var/log/fanterlasticfour/"
PIPEDIR="/var/tmp/fanterlasticfour/"

ERL="/usr/bin/erl"
RUN_ERL="/usr/bin/run_erl"
ERL_CALL="/usr/bin/erl_call"
#==============================================================

CONFIGPREFIX="____CONFIGPREFIX____"

ACTION="start"
VERBOSE=0
CONFIGFILE="$CONFIGPREFIX/fanterlasticfour/fanterlasticfour.sh"

while getopts ":a:c:v" Option 
  do
  case $Option in
      a)
	  ACTION="$OPTARG"
	  ;;
      c)
	  CONFIGFILE="$OPTARG"
	  ;;
      v)
	  VERBOSE=1
	  ;;
      *)
	  echo "Unrecognized option."
	  exit 1;
	  ;;
  esac
done

if [ -r $CONFIGFILE ] ; then
    source $CONFIGFILE
else
    echo "WARNING: could not read config file $CONFIGFILE. Continuing with defaults."
fi

case "$ACTION" in
    "start")
	if [[ $VERBOSE ]] ; then
	    echo "Starting."
	    # Echo the line.
	    echo "$RUN_ERL -daemon $PIPEDIR $LOGDIR \"$ERL -sname $NAME -boot fanterlasticfour -config $CONFIGPREFIX/fanterlasticfour/fanterlasticfour\""
	fi

        # Are we (likely to be) already running ?
	if [ -e $NAMEFILE ] ; then
	    echo "ERROR: File $NAMEFILE exists. Probably you have another Fanterlastic Four instance running. Aborting."
	    exit 3
	fi

	# Save the name for future reference.
	echo "$NAME" > $NAMEFILE

	# Run Fanterlastic Four.
	$RUN_ERL -daemon $PIPEDIR $LOGDIR "$ERL -sname $NAME -boot fanterlasticfour -config $CONFIGPREFIX/fanterlasticfour/fanterlasticfour"

	if [[ $VERBOSE ]] ; then
	    echo "Done."
	fi
	;;

    "stop")
	if [[ $VERBOSE ]] ; then
	    echo "Stoping."
	    echo "$ERL_CALL -sname $NAME -a \"init stop\""
	fi
	
	if [ -r $NAMEFILE ] ; then
            # Get the name of the node we want to stop.
	    NAME=$(cat $NAMEFILE)

	    # Stop Fanterlastic Four.
	    $ERL_CALL -sname $NAME -a "init stop"

	    # Remove the name file.
	    rm $NAMEFILE
	else
	    echo "ERROR: File $NAMEFILE doesn't exist or is not readable. Aborting."
	    exit 4
	fi
	
	if [[ $VERBOSE ]] ; then
	    echo "Done."
	fi
	;;
    *)
	echo "Action should be either \"start\" or \"stop\"."
	exit 2
	;;
esac
	
