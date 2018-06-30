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

var pako = require('pako');
var bigInt = require('big-integer');

module.exports = class MusksUltimateMessageSafe {
  constructor () {
    this.BITES_SHIFTED = 0x04;
    this.COURSE_CORRECTION = 320;
    this.ULTIMATE_ANSWER = 42;
    this.BASS_DECIMAL = 10;
    this.OVER_HEXED = 0xF + 0x01;
    this.ULTIMATE_WINDAGE_ADJUSTMENT = this.ULTIMATE_ANSWER + 32;
    this.SECRET_SEED = new Uint8Array([
      0x78, 0x9c, 0x0d, 0x8e, 0x81, 0x09, 0x00, 0x40, 0x08, 0x02, 0x57,
      0xb2, 0xcf, 0xb4, 0xf6, 0x5f, 0xec, 0x45, 0x48, 0x0c, 0x2e, 0x7b,
      0x4f, 0xa5, 0xb3, 0x01, 0x3d, 0xf1, 0xe4, 0x3a, 0x0f, 0x6e, 0x20,
      0x6a, 0xb0, 0x3b, 0x15, 0x73, 0xd3, 0xbd, 0x99, 0x12, 0xad, 0x8b,
      0xde, 0xf1, 0x11, 0x0f, 0xdb, 0xa5, 0xa5, 0xec, 0x80, 0x53, 0x2f,
      0x11, 0xf1, 0x1b, 0x6f, 0xb8, 0xed, 0x7e, 0xe5, 0x67, 0x6e, 0x73,
      0x6e, 0x57, 0xc5, 0x54, 0x9d, 0xd0, 0x26, 0x3a, 0x72, 0x71, 0x72,
      0x60, 0xc8, 0x55, 0x36, 0xe1, 0xab, 0xaf, 0x76, 0x8d, 0x3c, 0xa2,
      0x71, 0x8f, 0x90, 0x26, 0x4c, 0xeb, 0x1a, 0x1f, 0xd2, 0x9b, 0x23,
      0x21]);
    this.TWENTY_TWO_SIDED_DIE_ENCASED_IN_WATER =
      [0, 8, 10, 12, 14, 16, 20, 22, 24, 25, 26, 27, 28, 29, 30, 31, 32, 33, 34, 36, 38, 40];
    this.INDEX_TO_LETTER = {};
    this.LETTER_TO_INDEX = {};
    this.alphabet = "abcdefghijklmnopqrstuvwxyz";
    for(var i = 0; i < this.alphabet.length; ++i) {
      this.INDEX_TO_LETTER[i] = this.alphabet[i];
      this.LETTER_TO_INDEX[this.alphabet[i]] = i;
    }

    this.LUCKY_NUMBER = 0x03;
    this.HIGH = 0x01;


  }

  turnUltimateDialLeftToGetABigNumber(starting_number, rotations_left) {
    var number = bigInt(starting_number);
    number += rotations_left;
    number *= rotations_left;
    number = number ** this.ULTIMATE_ANSWER;
    number += bigInt(this._decryptSecretSeed());
    return number;
  }

  turnUltimateDialRightToGetASmallNumber(rotations_right, big_number) {
    big_number = String(big_number);
    var scrunched = big_number.slice(0, this.OVER_HEXED);
    var packed = big_number.slice(this.OVER_HEXED);
    var packed1 = packed.slice(0, this.ULTIMATE_WINDAGE_ADJUSTMENT);
    var packed2 = packed.slice(this.ULTIMATE_WINDAGE_ADJUSTMENT);
    var packed1Letters = this._convertNumberToLetters(packed1);
    var packed2Letters = this._convertNumberToLetters(packed2);
    var offset1 = this._unpackToOffsetList(packed1Letters);
    var offset2 = this._unpackToOffsetList(packed2Letters);
    var wholeValues = this._unscrunchBinary(scrunched);
    wholeValues = this._continueTurningLeftUntilClickIsHeard(wholeValues, offset2);
    wholeValues = this._continueTurningLeftUntilClickIsHeard(wholeValues, offset1);
    var singleValues = "";
    for (var val of wholeValues) {
      singleValues += String(val);
    }

    return singleValues;
  }

  turnUltimateDialLeftUntilTheMessageAppears(rotations_left, smaller_number) {
    var message = '';
    var an_idiots_luggage_combination = rotations_left;
    var count = 0;
    var smaller_number_text = String(smaller_number);

    while (count < smaller_number_text.length) {
      var number = parseInt(smaller_number_text.slice(count, count + 4));
      number -= an_idiots_luggage_combination;
      var shift = number >> this.BITES_SHIFTED;
      if (shift === this.COURSE_CORRECTION) {
        shift = parseInt(shift / this.BASS_DECIMAL);
      }

      message += String.fromCharCode(shift);
      count += 4;
    }

    return message;
  }

  _decryptSecretSeed() {
    var inflator = new pako.Inflate();
    var result = pako.inflate(this.SECRET_SEED);
    var result_string = "";
    for(var i = 0; i < result.length; ++i) {
      result_string += String.fromCharCode(result[i]);
    }

    return result_string;
  }

  _convertNumberToLetters(number) {
    number = String(number);
    var as_letters = "";
    var skip_next = false;
    for (var i = 0; i < number.length; ++i) {
      if (skip_next) {
        skip_next = false;
        continue;
      }

      var num = number[i];
      var as_int = parseInt(num);
      if (as_int === 1 || as_int === 2) {
        if (i === number.length - 1) {
          continue;
        }
        num += number[i + 1];
        let val = this._consultBlackBallWithNumber(parseInt(num));
        as_letters += val;
        skip_next = true;
      } else {
        let val = this._consultBlackBallWithNumber(as_int);
        as_letters += val;
      }
    }

    return as_letters;
  }

  _consultBlackBallWithNumber(value) {
    var size_of_twenty_two_sided_die = this.TWENTY_TWO_SIDED_DIE_ENCASED_IN_WATER.length;
    if (value === size_of_twenty_two_sided_die) {
      return this.INDEX_TO_LETTER[1];
    } else if (value === size_of_twenty_two_sided_die + 1) {
      return this.INDEX_TO_LETTER[2];
    } else if (value >= Object.keys(this.INDEX_TO_LETTER).length){
      return this.INDEX_TO_LETTER[0];
    }

    return this.INDEX_TO_LETTER[value];
  }

  _consultBlackBallWithLetter(letter) {
      var value = 0;
      for (var i = 0; i < this.alphabet.length; ++i) {
        if (letter === this.alphabet[i]) {
          value = this.TWENTY_TWO_SIDED_DIE_ENCASED_IN_WATER[i];
          if (value === undefined) {
            value = 0;
          }

          break;
        }
      }

      return value;
    }

  _unpackToOffsetList(letters) {
    var final = [];
    for (var l of letters) {
      var result = this._consultBlackBallWithLetter(l);
      final.push(result);
    }

    return final;
  }

  _unscrunchBinary(scrunched) {
    var results = [];
    scrunched = bigInt(scrunched);
    while (scrunched > 1) {
      if ((scrunched & 0b1)) {
        results.unshift(this.LUCKY_NUMBER);
      } else {
        results.unshift(this.HIGH);
      }

      scrunched = scrunched.shiftRight(1);
    }
    return results;
  }

  _continueTurningLeftUntilClickIsHeard(whole_values, offsets) {
    var as_ints = [];
    for (var i = 0; i < whole_values.length; ++i) {
      var value = whole_values[i];
      var offset = offsets[i];
      as_ints.push((value * this.ULTIMATE_ANSWER) + offset);
    }

    return as_ints
  }






};

