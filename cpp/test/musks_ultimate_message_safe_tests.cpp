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
    }

    virtual void TearDown() {
    }

    MusksUltimateMessageSafe safe_;
};

TEST_F(MusksUltimateMessafeSafeTest, TurnUltimateDialLeftToGetABigNumber1) {
    mpz_t integer1;
    mpz_init(integer1);

    unsigned long starting_number = 1;
    unsigned long rotations_left = 1;
    this->safe_.TurnUltimateDialLeftToGetABigNumber(starting_number, rotations_left, integer1);

    std::unique_ptr<char[]> result_pointer(mpz_get_str(NULL, 10, integer1));
    std::string result(&result_pointer[0]);
    std::string expected_result = "226169770062649671975095064650885165073473873466476969629424020831684677671512831015195781658332172748345988614697960374030307145316544864031281391887096765740000987048034";
    EXPECT_EQ(expected_result, result);
}

TEST_F(MusksUltimateMessafeSafeTest, TurnUltimateDialLeftToGetABigNumber2) {
    mpz_t integer1;
    mpz_init(integer1);

    unsigned long starting_number = 58;
    unsigned long rotations_left = 80;
    this->safe_.TurnUltimateDialLeftToGetABigNumber(starting_number, rotations_left, integer1);

    std::unique_ptr<char[]> result_pointer = std::unique_ptr<char[]>(mpz_get_str(NULL, 10, integer1));
    std::string result = &result_pointer[0];
    std::string expected_result = "289952203209166769731320260745999079281069613787727591807188853216289477734824614467054044779436718453547184789438846790465490761316544864031281391887096765735602940536930";
    EXPECT_EQ(expected_result, result);
}

TEST_F(MusksUltimateMessafeSafeTest, TurnUtlimateDialRightToGetASmallNumber) {
    mpz_t integer1;
    mpz_init(integer1);
    mpz_t integer2;
    mpz_init(integer2);

    unsigned long rotations_right = 221;
    mpz_set_str(integer1, "289952203209166769731320260745999079281069613787727591807188853216289477734824614467054044779436718453547184789438846790465490761316544864031281391887096765735602940536930", 10);
    this->safe_.TurnUtlimateDialRightToGetASmallNumber(rotations_right, integer1, integer2);

    std::unique_ptr<char[]> result_pointer = std::unique_ptr<char[]>(mpz_get_str(NULL, 10, integer2));
    std::string result = &result_pointer[0];
    std::string expected_result = "270862412836236458252642268867205902245058122461237727136720590227132772636859002293279263292374615627102836529223686157599858806364179862406156652631202476237223526325261823741786229064802786300663426740";
    EXPECT_EQ(expected_result, result);
}

TEST_F(MusksUltimateMessafeSafeTest, TurnUltimateDialLeftUntilTheMessageAppears) {
    mpz_t integer1;
    mpz_init(integer1);
    unsigned long rotations_left = 10;
    mpz_set_str(integer1, "13258259824895728903745280937498234", 10);
    std::string message = safe_.TurnUltimateDialLeftUntilTheMessageAppears(rotations_left, integer1);
    std::string expected_message = "R\x03\x02U+\xD1\xF9\xD4\x0E";
    EXPECT_EQ(expected_message, message);
}


