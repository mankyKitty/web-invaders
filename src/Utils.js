/* global exports */
"use strict";

// module Utils

exports.drawImageFromElement = function(ctx) {
  return function(elem) {
    return function(x) {
      return function(y) {
        ctx.drawImage(elem, x, y);
        return ctx;
      }
    }
  }
};
