"""
Copyright (C) 2018 Michael Cornwell

This program is free software: you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation, either version 3 of the License, or
(at your option) any later version.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program.  If not, see <http://www.gnu.org/licenses/>.
"""
import time
from musks_ultimate_message_safe import MusksUltimateMessageSafe

DRAMATIC_WAIT_IN_SECONDS = 1

def StartSafeProgram():
    print("Welcome to Elon Musk's Ultimate Message Safe. Only Elon may enter.")
    print("The safe requires the following four (4) inputs:")
    print()
    print("    [STARTING_NUMBER]     The number to start the safe on.")
    print("    [TURNS_LEFT]          The number of turns left to turn the dial.")
    print("    [TURNS_RIGHT]         The number of turns right to turn the dial.")
    print("    [TURNS_LEFT]          The number of turns left to turn the dial to get the message.")
    print()
    starting_number = int(input("What number should the safe start on?"))
    turns_left = int(input("How many turns left?"))
    turns_right = int(input("How many turns right?"))
    turns_left_2 = int(input("How many turns left?"))
    safe = MusksUltimateMessageSafe()

    print("You set the safe on number( {0} ).".format(starting_number))
    time.sleep(DRAMATIC_WAIT_IN_SECONDS)
    print("You start to turn( {0} )turns to the left, slowly.".format(turns_left))
    time.sleep(DRAMATIC_WAIT_IN_SECONDS)

    big_number = safe.turn_ultimate_dial_left_to_get_a_big_number(starting_number, turns_left)

    print("You lose grip on the dial and it continues turning on its own.")
    time.sleep(DRAMATIC_WAIT_IN_SECONDS)
    print("Something kicks in and boosts the spinning of the combination lock. The safe spins out of control to {0}.".format(big_number))
    time.sleep(DRAMATIC_WAIT_IN_SECONDS)

    smaller_number = safe.turn_ultimate_dial_right_to_get_a_small_number(turns_right, big_number)

    print("The safe turns( {0} )rotations to the right and settles on ".format(smaller_number))
    time.sleep(DRAMATIC_WAIT_IN_SECONDS)

    message = safe.turn_ultimate_dial_left_until_the_message_appears(turns_left_2, smaller_number)

    print("You turn the safe left( {0} ) times and the following message appears...  ".format(turns_left_2))
    time.sleep(DRAMATIC_WAIT_IN_SECONDS)
    print(message)
    time.sleep(DRAMATIC_WAIT_IN_SECONDS)
    return


if __name__ == '__main__':
    StartSafeProgram()

