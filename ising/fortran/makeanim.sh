#!/bin/bash

ANIMFILE=ising.gif

rm -f $ANIMFILE
convert -delay 20 step*.pgm $ANIMFILE
