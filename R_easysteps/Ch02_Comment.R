# Script name:  Comment.R
# Created on:   March 1,2019
# Author:       Mike McGrath
# Purpose:      Echo user input
# Version:      1.0
# Execution:    Must be run as Source to await user input.

# Request user input.
name <- readline( "Please enter your name: " )

# Concatenate input and strings.
greeting <- paste( "Welcome", name, "!" )

# Output concatenated string.
print( greeting )
