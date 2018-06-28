#!/usr/bin/python
from random import choice
from time import sleep
from subprocess import call

PEOPLE = ['Lila', 'Toveea', 'Both']
DIRECTIONS = ['Left', 'Right']
LIMBS = ['Hand', 'Foot']
COLOURS = ['Red', 'Blue', 'Yellow', 'Green']


def turn():
    choices = (choice(PEOPLE),
               choice(DIRECTIONS),
               choice(LIMBS),
               choice(COLOURS))
    return "%s, %s, %s, %s." % choices


while True:
    call(["say", turn()])
    sleep(3)
