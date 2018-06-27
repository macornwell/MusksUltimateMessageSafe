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

#include <gtest/gtest.h>
#include <memory>
#include "src/musks_ultimate_message_safe.h"

class MusksUltimateMessafeSafeTest : public ::testing::Test {
protected:
    virtual void SetUp() {
        mpz_init(this->integer1_);
        mpz_set_ui(this->integer1_, 0);
        mpz_init(this->integer2_);
        mpz_set_ui(this->integer2_, 0);
    }

    virtual void TearDown() {
    }

    mpz_t integer1_;
    mpz_t integer2_;
    MusksUltimateMessageSafe safe_;
};

TEST_F(MusksUltimateMessafeSafeTest, TurnUltimateDialLeftToGetABigNumber1) {
    unsigned long starting_number = 1;
    unsigned long rotations_left = 1;
    this->safe_.TurnUltimateDialLeftToGetABigNumber(starting_number, rotations_left, this->integer1_);

    std::unique_ptr<char[]> result_pointer(mpz_get_str(NULL, 10, this->integer1_));
    std::string result(&result_pointer[0]);
    std::string expected_result = "226169770062649671975095064650885165073473873466476969629424020831684677671512831015195781658332172748345988614697960374030307145316544864031281391887096765740000987048034";
    EXPECT_EQ(expected_result, result);
}

TEST_F(MusksUltimateMessafeSafeTest, TurnUltimateDialLeftToGetABigNumber2) {
    unsigned long starting_number = 58;
    unsigned long rotations_left = 22;
    this->safe_.TurnUltimateDialLeftToGetABigNumber(starting_number, rotations_left, this->integer2_);

    std::unique_ptr<char[]> result_pointer = std::unique_ptr<char[]>(mpz_get_str(NULL, 10, this->integer2_));
    std::string result = &result_pointer[0];
    std::string expected_result = "226169770062649671975095064650885185563006315551083811999265990475439956554521464222954542507037221461515179149470325984246370121316544864031281391887096765735602940536930";
    EXPECT_EQ(expected_result, result);
}

TEST_F(MusksUltimateMessafeSafeTest, TurnUtlimateDialRightToGetASmallNumber) {
    unsigned long rotations_right = 220;
    mpz_set_str(this->integer1_, "226169770062649671975095064650885165073473873466476969629424020831684677671512831015195781658332172748345988614697960374030307145316544864031281391887096765740000987048034", 10);
    this->safe_.TurnUtlimateDialRightToGetASmallNumber(rotations_right, this->integer1_, this->integer2_);

    std::unique_ptr<char[]> result_pointer = std::unique_ptr<char[]>(mpz_get_str(NULL, 10, this->integer2_));
    std::string result = &result_pointer[0];
    std::string expected_result = "2626247227942284226834692704529223722786581623682436283827962788263630402604636427002828262653042292271023641778228817842282531427083049598422933128243623663834161627722642237617765828296427863002283632142792";
    EXPECT_EQ(expected_result, result);
}

TEST_F(MusksUltimateMessafeSafeTest, TurnUltimateDialLeftUntilTheMessageAppears) {
    unsigned long rotations_left = 10;
    mpz_set_str(this->integer1_, "13258259824895728903745280937498234", 10);
    std::string message = safe_.TurnUltimateDialLeftUntilTheMessageAppears(rotations_left, this->integer1_);
    std::string expected_message = "R\x03\x02U+\xD1\xF9\xD4\x0E";
    EXPECT_EQ(expected_message, message);
}




