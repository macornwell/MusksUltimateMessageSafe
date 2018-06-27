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

import sympy
import zlib
from string import ascii_lowercase

class MusksUltimateMessageSafe:
    BITE_SHIFT_PLACES = 4
    ULTIMATE_ANSWER = 42
    HIGH = 1
    LUCKY_NUMBER = 3
    COURSE_CORRECTION = 320
    TWENTY_TWO_SIDED_DIE = [0, 8, 10, 12, 14, 16, 20, 22, 24, 25, 26, 27, 28, 29, 30, 31, 32, 33, 34, 36, 38, 40]
    INDEX_TO_LETTER = {index: letter for index, letter in enumerate(ascii_lowercase, start=0)}
    LETTER_TO_INDEX = {letter: index for index, letter in enumerate(ascii_lowercase, start=0)}
    OVER_HEXED = 0xF + 0x01
    ULTIMATE_WINDAGE_ADJUSTMENT = ULTIMATE_ANSWER + 32
    SECRET_SEED = b'x\x9c\r\x8e\x81\t\x00@\x08\x02W\xb2\xcf\xb4\xf6_\xecEH\x0c.{O\xa5\xb3\x01=\xf1\xe4:\x0fn j\xb0;' \
                  b'\x15s\xd3\xbd\x99\x12\xad\x8b\xde\xf1\x11\x0f\xdb\xa5\xa5\xec\x80S/\x11\xf1\x1bo\xb8\xed~' \
                  b'\xe5gnsnW\xc5T\x9d\xd0&:rqr`\xc8U6\xe1\xab\xafv\x8d<\xa2q\x8f\x90&L\xeb\x1a\x1f\xd2\x9b#!'

    def _get_letter_for_value(self, value):
        if value == len(self.TWENTY_TWO_SIDED_DIE):
            return self.INDEX_TO_LETTER[1]
        if value == len(self.TWENTY_TWO_SIDED_DIE) + 1:
            return self.INDEX_TO_LETTER[2]
        if value >= len(self.INDEX_TO_LETTER):
            return self.INDEX_TO_LETTER[0]
        return self.INDEX_TO_LETTER[value]

    def _continue_turning_left_until_click_is_heard(self, whole_values, offsets):
        as_ints = []
        for i in range(len(whole_values)):
            value = whole_values[i]
            offset = offsets[i]
            as_ints.append((value * self.ULTIMATE_ANSWER) + offset)
        return as_ints

    def _unscrunch_binary(self, scrunched):
        results = []
        scrunched = int(scrunched)
        while scrunched > 1:
            if scrunched & 0b1:
                results = [self.LUCKY_NUMBER] + results
            else:
                results = [self.HIGH] + results
            scrunched >>= 1
        return results

    def _convert_number_to_letters(self, number):
        number = str(number)
        as_letters = ''
        skip_next = False
        for i in range(0, len(number)):
            if skip_next:
                skip_next = False
                continue
            num = number[i]
            as_int = int(num)
            if as_int == 1 or as_int == 2:
                if i == len(number) - 1:
                    continue
                num += number[i + 1]
                as_letters += self._get_letter_for_value(int(num))
                skip_next = True
            else:
                as_letters += self._get_letter_for_value(as_int)
        return as_letters

    def _get_value_for_letter(self, letter):
        for i in range(len(ascii_lowercase)):
            if letter == ascii_lowercase[i]:
                try:
                    return self.TWENTY_TWO_SIDED_DIE[i]
                except:
                    return -1
        return -1

    def _unpack_to_offset_list(self, letters):
        final = []
        for l in letters:
            final.append(self._get_value_for_letter(l))
        return final

    def _turn_ultimate_dial_right_to_get_big_number(self, starting_number):
        bigger = starting_number + self.ULTIMATE_ANSWER
        even_bigger = bigger * self.ULTIMATE_ANSWER
        huge = even_bigger ** self.ULTIMATE_ANSWER
        seed = sympy.Integer(zlib.decompress(self.SECRET_SEED).decode('utf-8'))
        big_number = huge + seed
        return big_number

    def _turn_ultimate_dial_left_to_get_smaller_number(self, big_number):
        big_number = str(big_number)
        scrunched = big_number[:self.OVER_HEXED]
        packed = big_number[self.OVER_HEXED:]
        packed_1 = packed[:self.ULTIMATE_WINDAGE_ADJUSTMENT]
        packed_2 = packed[self.ULTIMATE_WINDAGE_ADJUSTMENT:]
        packed_1_letters = self._convert_number_to_letters(packed_1)
        packed_2_letters = self._convert_number_to_letters(packed_2)
        offset_1 = self._unpack_to_offset_list(packed_1_letters)
        offset_2 = self._unpack_to_offset_list(packed_2_letters)
        whole_values = self._unscrunch_binary(scrunched)
        whole_values = self._continue_turning_left_until_click_is_heard(whole_values, offset_2)
        whole_values = self._continue_turning_left_until_click_is_heard(whole_values, offset_1)
        return whole_values

    def _turn_ultimate_dial_right_until_message_appears(self, data):
        message = ''
        for int_obj in data:
            int_obj -= IDIOTS_LUGGAGE_SPINNER
            shift = int_obj >> self.BITE_SHIFT_PLACES
            if shift == self.COURSE_CORRECTION:
                shift = int(shift / 10)
            c = chr(shift)
            message += c
        return message

    def open_and_discover_message(self, starting_number):
        big_number = self._turn_ultimate_dial_right_to_get_big_number(starting_number)
        whole_values = self._turn_ultimate_dial_left_to_get_smaller_number(big_number)
        message = self._turn_ultimate_dial_right_until_message_appears(whole_values)
        return message


def main():
    pass

if __name__ == '__main__':
    main()
