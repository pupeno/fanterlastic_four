#!/usr/bin/env bash

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

ACTION="start"
VERBOSE=false
CONFIGFILE="/etc/fanterlasticfour/scriptconfig"

while getopts ":a:c:" Option 
  do
  case $Option in
      a)
	  ACTION="$OPTARG"
	  ;;
      c)
	  CONFIGFILE="$OPTARG"
	  ;;
      *)
	  echo "Unrecognized option."
	  exit 1;
	  ;;
  esac
done

source $CONFIGFILE

case "$ACTION" in
    "start")
	echo "Starting."
	# Save the name for future reference.
	# TODO: Check that $NAMEFILE doesn't exist, otherwise, abort.
	echo "$NAME" > $NAMEFILE
	# Echo the line.
	echo "$RUN_ERL -daemon $PIPEDIR $LOGDIR \"$ERL -sname $NAME -boot fanterlasticfour\""
	# Run Fanterlastic Four.
	$RUN_ERL -daemon $PIPEDIR $LOGDIR "$ERL -sname $NAME -boot fanterlasticfour"
	echo "Done."
	;;
    "stop")
	echo "Stoping."
	# Get the name of the node we want to stop.
	# TODO: Check that $NAMEFILE exists, otherwise, abort.
	$NAME=$(cat $NAMEFILE)
	echo "$ERL_CALL -sname $NAME -a \"init stop\""
	$ERL_CALL -sname $NAME -a "init stop"
	echo "Done."
	;;
    *)
	echo "Action should be either \"start\" or \"stop\"."
	exit 2
	;;
esac
	
