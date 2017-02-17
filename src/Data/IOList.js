'use strict';

function $$String(string, encoding) {
  this.string = string;
  this.encoding = encoding;
}

exports['append\''] = function(a) {
  return function(b) {
    if (a === null) {
      return b;
    }
    if (b === null) {
      return a;
    }
    return [a, b];
  };
};

//----------------------------------------------------------------------------//

exports.empty = null;

exports.fromByteString = function(byteString) {
  return byteString;
};

exports.fromString = function(string) {
  return function(encoding) {
    return new $$String(string, encoding);
  };
};

exports.fromArray = function(array) {
  return array;
};

//----------------------------------------------------------------------------//

exports.foldl = function(onByteString) {
  return function(onString) {
    return function go(zero) {
      return function(list) {
        if (list === null) {
          return zero;
        }
        if (list.constructor === Buffer) {
          return onByteString(zero)(list);
        }
        if (list.constructor === Array) {
          var result = zero;
          var length = list.length;
          for (var i = 0; i < length; ++i) {
            result = go(result)(list[i]);
          }
          return result;
        }
        return onString(zero)(list.string)(list.encoding);
      };
    };
  };
};
