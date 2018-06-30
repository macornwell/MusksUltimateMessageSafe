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

var assert = require('assert');
var bigInt = require('big-integer');
var muskSafe = require('./musks-ultimate-message-safe.js');

describe('MusksUltimateMessageSafe', function() {
  describe('#turnUltimateDialLeftToGetABigNumber()', function() {
    it('should handle starting_value of 1 and rotations_left of 1', function() {
      var safe = new muskSafe();
      var starting_value = 1;
      var rotations_left = 1;
      var big_int = safe.turnUltimateDialLeftToGetABigNumber(starting_value, rotations_left);
      var expected = "2.2616977006264967e+170";
      var result = String(big_int);
      assert.equal(result, expected);
    });
  });

  describe('#turnUltimateDialRightToGetASmallNumber', function() {
    it('Should be able to handle rotations right 221 and provided big int', function() {
      var safe = new muskSafe();
      var rotations_right = 221;
      var big_number = bigInt("28995220320916676973132026074599907928106961378772759180718885321628947773482461446" +
        "70540447794367184535471847894388467904654907613165448640312813918870967657356029405" +
        "36930");
      var smaller_number = safe.turnUltimateDialRightToGetASmallNumber(rotations_right, big_number);
      var expected_result = bigInt("270862412836236458252642268867205902245058122461237727136720590227132772636859" +
        "002293279263292374615627102836529223686157599858806364179862406156652631202476" +
        "237223526325261823741786229064802786300663426740");
      assert.equal(String(smaller_number), String(expected_result));
    });
  });


  describe('#turnUltimateDialLeftUntilMessageAppears', function() {
    it('Should be able to handle rotations 10 and provided smaller_number', function() {
      var safe = new muskSafe();
      var rotations_left = 10;
      var smaller_number = bigInt("13258259824895728903745280937498234");
      var message = safe.turnUltimateDialLeftUntilTheMessageAppears(rotations_left, smaller_number);
      var expected_message = 'RȃȂɕȫǑǹǔ\x0e';
      assert.equal(message, expected_message);
    });
  });
});
