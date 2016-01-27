/* global exports */
"use strict";

// module Utils

exports.drawImageFinalForm = function(ctx) {
  return function(elem) {
    return function(sx) {
      return function(sy) {
        return function(sw) {
          return function(sh) {
            return function(dx) {
              return function(dy) {
                return function(dw) {
                  return function(dh) {
                    return function() {
                      ctx.drawImage(elem, sx, sy, sw, sh, dx, dy, dw, dh);
                      return ctx;
                    };
                  };
                };
              };
            };
          };
        };
      };
    };
  };
};

exports.drawImageFromElement = function(ctx) {
  return function(elem) {
    return function(x) {
      return function(y) {
        return function() {
          ctx.drawImage(elem, x, y);
          return ctx;
        };
      };
    };
  };
};

exports.drawImageFromElementScale = function(ctx) {
  return function(elem) {
    return function(x) {
      return function(y) {
        return function(dx) {
          return function(dy) {
            return function() {
              ctx.drawImage(elem, x, y, dx, dy);
              return ctx;
            };
          };
        };
      };
    };
  };
};
