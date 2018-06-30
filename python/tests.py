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
from unittest import TestCase
import sympy
from musks_ultimate_message_safe import MusksUltimateMessageSafe


class MusksUltimateMessageSafeTest(TestCase):

    def setUp(self):
        pass

    def test_turn_ultimate_dial_left_to_get_a_big_number(self):
        safe = MusksUltimateMessageSafe()
        starting_number = 1
        rotations_left = 1
        big_integer = safe.turn_ultimate_dial_left_to_get_a_big_number(starting_number, rotations_left)
        expected_result = sympy.Integer("226169770062649671975095064650885165073473873466476969629424020831684677671512"
                                        "831015195781658332172748345988614697960374030307145316544864031281391887096765"
                                        "740000987048034")
        self.assertEqual(big_integer, expected_result)

    def test_turn_utlimate_dial_right_to_get_a_small_number(self):
        safe = MusksUltimateMessageSafe()
        rotations_right = 221
        big_number = sympy.Integer("28995220320916676973132026074599907928106961378772759180718885321628947773482461446"
                                   "70540447794367184535471847894388467904654907613165448640312813918870967657356029405"
                                   "36930")
        smaller_number = safe.turn_ultimate_dial_right_to_get_a_small_number(rotations_right, big_number)
        expected_result = sympy.Integer("270862412836236458252642268867205902245058122461237727136720590227132772636859"
                                        "002293279263292374615627102836529223686157599858806364179862406156652631202476"
                                        "237223526325261823741786229064802786300663426740")
        self.assertEqual(str(smaller_number), str(expected_result))

    def test_turn_ultimate_dial_left_until_the_message_appears(self):
        safe = MusksUltimateMessageSafe()
        rotations_left = 10
        smaller_number = sympy.Integer("13258259824895728903745280937498234")
        message = safe.turn_ultimate_dial_left_until_the_message_appears(rotations_left, smaller_number)
        expected_message = 'RȃȂɕȫǑǹǔ\x0e'
        self.assertEqual(message, expected_message)

