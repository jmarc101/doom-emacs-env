#!/bin/bash

# Customize these paths and URLs
ICS2ORGPERS="$HOME/.doom.d/scripts/ical2org-pers"  # Path to your ical2org script
ICS2ORGWORK="$HOME/.doom.d/scripts/ical2org-optable"  # Path to your ical2org script
ICSFILE="basic.ics"  # Path to the temporary downloaded .ics file
WORKORGFILE="$HOME/.doom.d/org/calendars/gcal-work.org"  # Path to the output .org file
PERSORGFILE="$HOME/.doom.d/org/calendars/gcal-pers.org"  # Path to the output .org file
URL1="https://calendar.google.com/calendar/ical/jm.prudhomme%40optable.co/public/basic.ics"
URL2="https://calendar.google.com/calendar/ical/jm.prudhomme%40gmail.com/public/basic.ics"

# Download the first calendar
wget -q -O $ICSFILE $URL1
$ICS2ORGWORK < $ICSFILE > $WORKORGFILE

# Download the second calendar
wget -q -O $ICSFILE.1 $URL2
$ICS2ORGPERS < $ICSFILE.1 > $PERSORGFILE
