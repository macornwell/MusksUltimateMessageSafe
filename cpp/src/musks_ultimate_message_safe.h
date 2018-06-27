/*
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
*/

#ifndef MUSKS_ULTIMATE_MESSAFE_SAFE_H
#define MUSKS_ULTIMATE_MESSAFE_SAFE_H
#include <map>
#include <vector>
#include <string>
#include "gmp.h"
#include "zlib.h"

class MusksUltimateMessageSafe {

public:
    MusksUltimateMessageSafe() : INDEX_TO_LETTER(GetIndexToLetter()), LETTER_TO_INDEX(GetLetterToIndex()) {};
    ~MusksUltimateMessageSafe() {};

    void TurnUltimateDialLeftToGetABigNumber(
            unsigned long starting_number, unsigned long rotations_left, mpz_t &large_number_to_fill);
    void TurnUtlimateDialRightToGetASmallNumber(
            unsigned long rotations_right, const mpz_t &big_number, mpz_t &number_to_fill);
    std::string TurnUltimateDialLeftUntilTheMessageAppears(
            unsigned long rotations_left, const mpz_t smaller_number);

private:

    std::string DecryptMagicSeed();
    const std::map<const int, const char> GetIndexToLetter();
    const std::map<const char, const int> GetLetterToIndex();
    std::vector<int> ContinueTurningLeftUntilClickIsHeard(const std::vector<int>& whole_values, const std::vector<int>& offsets);
    char ConsultBlackBall(int value);
    std::vector<int> UnscrunchBinary(const std::string& scrunched);
    std::string ConvertNumberToLetters(const mpz_t& number);
    int ConsultBlackBall(char letter);
    std::vector<int> UnpackToOffsetList(const std::string& letters);

    const int BITES_SHIFTED = 0x4;
    const uInt BUFFER_SIZE = 4096;
    const int ULTIMATE_ANSWER = 42;
    const int HIGH = 0x1;
    const int LUCKY_NUMBER = 0x3;
    const int COURSE_CORRECTION = 320;
    const int CEILING = 0x39;
    const char ASCII_DROP = '0';
    const int WIDE_NUMBER_SIZE = 4;
    const int BASS_DECIMAL = 10;
    const std::vector<int> TWENTY_TWO_SIDED_DIE_ENCASED_IN_WATER = {0, 8, 10, 12, 14, 16, 20, 22, 24, 25, 26, 27, 28,
                                                                    29, 30, 31, 32, 33, 34, 36, 38, 40};
    const std::string alphabet_lowercase_ = "abcdefghijklmnopqrstuvwxyz";
    const std::map<const char, const int> LETTER_TO_INDEX;
    const std::map<const int, const char> INDEX_TO_LETTER;
    int OVER_HEXED = 0xF + 0x01;
    int ULTIMATE_WINDAGE_ADJUSTMENT = ULTIMATE_ANSWER + 32;
    std::vector<Bytef> SECRET_SEED = {0x78, 0x9c, 0x0d, 0x8e, 0x81, 0x09, 0x00, 0x40, 0x08, 0x02, 0x57,
                                      0xb2, 0xcf, 0xb4, 0xf6, 0x5f, 0xec, 0x45, 0x48, 0x0c, 0x2e, 0x7b,
                                      0x4f, 0xa5, 0xb3, 0x01, 0x3d, 0xf1, 0xe4, 0x3a, 0x0f, 0x6e, 0x20,
                                      0x6a, 0xb0, 0x3b, 0x15, 0x73, 0xd3, 0xbd, 0x99, 0x12, 0xad, 0x8b,
                                      0xde, 0xf1, 0x11, 0x0f, 0xdb, 0xa5, 0xa5, 0xec, 0x80, 0x53, 0x2f,
                                      0x11, 0xf1, 0x1b, 0x6f, 0xb8, 0xed, 0x7e, 0xe5, 0x67, 0x6e, 0x73,
                                      0x6e, 0x57, 0xc5, 0x54, 0x9d, 0xd0, 0x26, 0x3a, 0x72, 0x71, 0x72,
                                      0x60, 0xc8, 0x55, 0x36, 0xe1, 0xab, 0xaf, 0x76, 0x8d, 0x3c, 0xa2,
                                      0x71, 0x8f, 0x90, 0x26, 0x4c, 0xeb, 0x1a, 0x1f, 0xd2, 0x9b, 0x23,
                                      0x21};
};



#endif //MUSKS_ULTIMATE_MESSAFE_SAFE_H
