#!/bin/bash

# Customize these paths and URLs
ICS2ORGPERS="path/to/ical2org"  # Path to your ical2org script
ICS2ORGWORK="path/to/ical2org"  # Path to your ical2org script
ICSFILE="basic.ics"  # Path to the temporary downloaded .ics file
WORKORGFILE="path/to/org/schedule/file"  # Path to the output .org file
PERSORGFILE="path/to/org/schedule/file"  # Path to the output .org file
URL1="url first to agenda"
URL2="url second to agenda"

# Download the first calendar
wget -q -O $ICSFILE $URL1
$ICS2ORGWORK < $ICSFILE > $WORKORGFILE

# Download the second calendar
wget -q -O $ICSFILE.1 $URL2
$ICS2ORGPERS < $ICSFILE.1 > $PERSORGFILE

rm -rf $ICSFILE
rm -rf $ICSFILE.1
