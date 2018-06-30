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

#include <exception>
#include <iostream>
#include <memory>
#include <sstream>
#include "zlib.h"
#include "musks_ultimate_message_safe.h"

void MusksUltimateMessageSafe::TurnUltimateDialLeftToGetABigNumber(
        unsigned long starting_number,
        unsigned long rotations_left,
        mpz_t &large_number_to_fill) {

    mpz_t starting_number_mpz;
    mpz_init(starting_number_mpz);
    mpz_set_ui(starting_number_mpz, (u_long)starting_number);

    mpz_add(large_number_to_fill, large_number_to_fill, starting_number_mpz);
    mpz_add_ui(large_number_to_fill, large_number_to_fill, rotations_left);
    mpz_mul_ui(large_number_to_fill, large_number_to_fill, rotations_left);
    mpz_pow_ui(large_number_to_fill, large_number_to_fill, (u_long)this->ULTIMATE_ANSWER);

    mpz_t seed;
    mpz_init(seed);
    mpz_set_str(seed, this->DecryptSecretSeed().data(), this->BASS_DECIMAL);
    mpz_add(large_number_to_fill, large_number_to_fill, seed);
    mpz_clear(seed);
}

void MusksUltimateMessageSafe::TurnUtlimateDialRightToGetASmallNumber(
        unsigned long rotations_right, const mpz_t &big_number, mpz_t &number_to_fill) {
    mpz_t packed_1_number;
    mpz_t packed_2_number;
    std::string scrunched;
    std::string packed;
    std::string packed_1;
    std::string packed_2;

    mpz_init(packed_1_number);
    mpz_init(packed_2_number);
    mpz_set_ui(packed_1_number, 0);
    mpz_set_ui(packed_2_number, 0);
    std::unique_ptr<char[]> big_number_text_pointer(mpz_get_str(NULL, this->BASS_DECIMAL, big_number));
    std::string big_number_as_string(&big_number_text_pointer[0]);

    for(int i = 0; i < big_number_as_string.size(); ++i) {
        char value = big_number_as_string[i];
        if (i < this->OVER_HEXED) {
            scrunched += value;
        } else {
            packed += value;
        }
    }

    for(int i = 0; i < packed.size(); ++i) {
        if (i < this->ULTIMATE_WINDAGE_ADJUSTMENT) {
            packed_1 += packed[i];
        } else {
            packed_2 += packed[i];
        }
    }

    mpz_set_str(packed_1_number, packed_1.data(), this->BASS_DECIMAL);
    mpz_set_str(packed_2_number, packed_2.data(), this->BASS_DECIMAL);

    std::string packed_1_letters = this->ConvertNumberToLetters(packed_1_number);
    std::string packed_2_letters = this->ConvertNumberToLetters(packed_2_number);
    std::vector<int> offset_1 = this->UnpackToOffsetList(packed_1_letters);
    std::vector<int> offset_2 = this->UnpackToOffsetList(packed_2_letters);
    std::vector<long> whole_values = this->UnscrunchBinary(scrunched);
    whole_values = this->ContinueTurningLeftUntilClickIsHeard(whole_values, offset_2);
    whole_values = this->ContinueTurningLeftUntilClickIsHeard(whole_values, offset_1);

    std::string as_string;
    for(int i : whole_values) {
        as_string += std::to_string(i);
    }

    mpz_set_str(number_to_fill, as_string.data(), this->BASS_DECIMAL);
    mpz_clear(packed_1_number);
    mpz_clear(packed_2_number);
}

std::string MusksUltimateMessageSafe::TurnUltimateDialLeftUntilTheMessageAppears(
        unsigned long rotations_left, const mpz_t &smaller_numbers) {
    std::stringstream stream;
    std::string message;
    unsigned long an_idiots_luggage_combination = rotations_left;
    std::unique_ptr<char[]> small_number_pointer(mpz_get_str(NULL, this->BASS_DECIMAL, smaller_numbers));
    std::string small_number_as_string(&small_number_pointer[0]);

    for (unsigned long i = 0; i < small_number_as_string.size(); i += this->WIDE_NUMBER_SIZE) {
        int number = stoi(small_number_as_string.substr(i, this->WIDE_NUMBER_SIZE));
        number -= an_idiots_luggage_combination;
        int shift = number >> this->BITES_SHIFTED;
        if (shift == this->COURSE_CORRECTION) {
            shift = shift / this->BASS_DECIMAL;
        }
        message += shift;
        stream << shift;
    }

    return message;
}

std::vector<long> MusksUltimateMessageSafe::ContinueTurningLeftUntilClickIsHeard(
        const std::vector<long> &whole_values, const std::vector<int> &offsets) {
    std::vector<long> as_ints;
    for (int i = 0; i < whole_values.size(); ++i) {
        long whole_value = whole_values[i];
        long offset = offsets[i];
        long value = ((whole_value * this->ULTIMATE_ANSWER) + offset);
        as_ints.push_back(value);
    }

    return as_ints;
}

char MusksUltimateMessageSafe::ConsultBlackBall(int value) {
    int size_of_twenty_two_sided_die = (int)this->TWENTY_TWO_SIDED_DIE_ENCASED_IN_WATER.size();
    if (value == size_of_twenty_two_sided_die) {
        return this->INDEX_TO_LETTER.at(1);
    } else if (value == size_of_twenty_two_sided_die + 1) {
        return this->INDEX_TO_LETTER.at(2);
    } else if (value >= this->INDEX_TO_LETTER.size()) {
        return this->INDEX_TO_LETTER.at(0);
    }

    return this->INDEX_TO_LETTER.at(value);
}

int MusksUltimateMessageSafe::ConsultBlackBall(char letter) {
    int value = 0;
    for (int i = 0; i < this->alphabet_lowercase_.size(); ++i) {
        if (letter == this->alphabet_lowercase_[i]) {
            try {
                value = this->TWENTY_TWO_SIDED_DIE_ENCASED_IN_WATER.at(i);
                break;
            }
            catch (std::exception& e) {
                // Invalid input.
            }
        }
    }

    return value;
}

std::vector<long> MusksUltimateMessageSafe::UnscrunchBinary(const std::string &scrunched) {
    std::vector<long> results;
    long long as_int = stoll(scrunched);
    while (as_int > 1) {
        if (as_int & 0b1) {
            results.insert(results.begin(), this->LUCKY_NUMBER);
        } else {
            results.insert(results.begin(), this->HIGH);
        }
        as_int >>= 1;
    }

    return results;
}

std::string MusksUltimateMessageSafe::ConvertNumberToLetters(const mpz_t &big_number) {
    std::string as_letters;
    std::unique_ptr<char[]> number_char_p(mpz_get_str(NULL, this->BASS_DECIMAL, big_number));
    std::string number_as_text(&number_char_p[0]);

    bool skip_next = false;
    for (int i = 0; i < number_as_text.size(); ++i) {
        if (skip_next) {
            skip_next = false;
            continue;
        }

        std::string number;
        number += number_as_text[i];
        int number_as_int = number[0] - this->ASCII_DROP;
        if (number == "1" or number == "2") {
            if (i == (number_as_text.size() - 1)) {
                continue;
            }

            number += number_as_text[i + 1];
            as_letters += this->ConsultBlackBall(atoi(number.data()));
            skip_next = true;
        } else {
            as_letters += this->ConsultBlackBall(number_as_int);
        }
    }

    return as_letters;
}

std::vector<int> MusksUltimateMessageSafe::UnpackToOffsetList(const std::string &letters) {
    std::vector<int> final;
    for(char c : letters) {
        int value = this->ConsultBlackBall(c);
        final.push_back(value);
    }

    return final;
}

const std::map<const int, const char> MusksUltimateMessageSafe::GetIndexToLetter() {
    std::map<const int, const char> index_to_letter;
    for(int i = 0; i < this->alphabet_lowercase_.length(); ++i) {
        char character = this->alphabet_lowercase_[i];
        index_to_letter.insert(std::make_pair(i, character));
    }

    return index_to_letter;
};

const std::map<const char, const int> MusksUltimateMessageSafe::GetLetterToIndex() {
    std::map<const char, const int> letter_to_index;
    for(int i = 0; i < this->alphabet_lowercase_.length(); ++i) {
        char character = this->alphabet_lowercase_[i];
        letter_to_index.insert(std::make_pair(character, i));
    }

    return letter_to_index;
};

std::string MusksUltimateMessageSafe::DecryptSecretSeed() {
    z_stream infstream;
    infstream.zalloc = Z_NULL;
    infstream.zfree = Z_NULL;
    infstream.opaque = Z_NULL;

    Bytef buffer[this->BUFFER_SIZE];
    for(int i = 0; i < this->BUFFER_SIZE; ++i) {
        buffer[i] = 0;
    }

    std::vector<Bytef> deep_copy = this->SECRET_SEED;

    infstream.avail_in = (uInt)this->SECRET_SEED.size();
    infstream.next_in = deep_copy.data();
    infstream.avail_out = this->BUFFER_SIZE;
    infstream.next_out = buffer;

    inflateInit(&infstream);
    inflate(&infstream, Z_NO_FLUSH);
    inflateEnd(&infstream);
    std::string result;
    for (int i = 0; i < this->BUFFER_SIZE; ++i) {
        Bytef byte = buffer[i];
        if (byte > this->CEILING) {
            continue;
        }
        if (byte < 0x1) {
            break;
        }

        result += byte;
    }

    return result;
}

