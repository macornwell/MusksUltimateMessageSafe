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
#include <iostream>
#include <chrono>
#include <thread>
#include <string>
#include "gmp.h"
#include "musks_ultimate_message_safe.h"

const int DRAMATIC_WAIT_IN_SECONDS = 1;

void StartSafeProgram() {
    std::cout << "Welcome to Elon Musk's Ultimate Message Safe. Only Elon may enter." << std::endl
              << "The safe requires the following four (4) inputs:" << std::endl
              << std::endl
              << "    [STARTING_NUMBER]     The number to start the safe on." << std::endl
              << "    [TURNS_LEFT]          The number of turns left to turn the dial." << std::endl
              << "    [TURNS_RIGHT]         The number of turns right to turn the dial." << std::endl
              << "    [TURNS_LEFT]          The number of turns left to turn the dial to get the message." << std::endl
              << std::endl;

    std::cout << "What number should the safe start on?" << std::endl;
    unsigned long starting_number = 0;
    std::cin >> starting_number;
    std::cout << "How many turns left?" << std::endl;
    unsigned long turns_left = 0;
    std::cin >> turns_left;
    std::cout << "How many turns right?" << std::endl;
    unsigned long turns_right = 0;
    std::cin >> turns_right;
    std::cout << "How many turns left?" << std::endl;
    unsigned long turns_left_2 = 0;
    std::cin >> turns_left_2;

    MusksUltimateMessageSafe safe;

    std::string message;
    mpz_t big_number = {};
    mpz_init(big_number);
    mpz_set_ui(big_number, 0);
    mpz_t smaller_number = {};
    mpz_init(smaller_number);
    mpz_set_ui(smaller_number, 0);

    std::cout << "You set the safe on number( " << starting_number <<  " )." << std::endl;
    std::this_thread::sleep_for(std::chrono::seconds(DRAMATIC_WAIT_IN_SECONDS));
    std::cout << "You start to turn( " << turns_left << " )turns to the left, slowly." << std::endl;
    std::this_thread::sleep_for(std::chrono::seconds(DRAMATIC_WAIT_IN_SECONDS));

    safe.TurnUltimateDialLeftToGetABigNumber(starting_number, turns_left,big_number);

    std::cout << "You lose grip on the dial and it continues turning on its own." << std::endl;
    std::this_thread::sleep_for(std::chrono::seconds(DRAMATIC_WAIT_IN_SECONDS));
    std::unique_ptr<char[]> large_number_pointer(mpz_get_str(NULL, 10, big_number));
    std::string large_number(&large_number_pointer[0]);
    std::cout << "Something kicks in and boosts the spinning of the combination lock. The safe spins out of control to "
              << large_number << "." << std::endl;
    std::this_thread::sleep_for(std::chrono::seconds(DRAMATIC_WAIT_IN_SECONDS));

    safe.TurnUtlimateDialRightToGetASmallNumber(turns_right, big_number, smaller_number);

    std::unique_ptr<char[]> small_number_pointer(mpz_get_str(NULL, 10, smaller_number));
    std::string small_number_string(&small_number_pointer[0]);
    std::cout << "The safe turns( " << turns_right << " )rotations to the right and settles on "
              << small_number_string << std::endl;
    std::this_thread::sleep_for(std::chrono::seconds(DRAMATIC_WAIT_IN_SECONDS));

    message = safe.TurnUltimateDialLeftUntilTheMessageAppears(turns_left_2, smaller_number);

    std::cout << "You turn the safe left (" << std::to_string(turns_left_2)
              <<") times and the following message appears...  ";
    std::this_thread::sleep_for(std::chrono::seconds(DRAMATIC_WAIT_IN_SECONDS));
    std::cout << std::endl << std::endl << "\"" << message << "\"" << std::endl;
    std::this_thread::sleep_for(std::chrono::seconds(DRAMATIC_WAIT_IN_SECONDS));
}

int main(int argc, char *argv[]) {
    StartSafeProgram();
    return 0;
}