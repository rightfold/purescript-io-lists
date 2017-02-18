'use strict';

var Node_Encoding = require('../Node.Encoding');

function $$String(string, encoding) {
  this.string = string;
  this.encoding = encoding;
}

exports['append\''] = function(a) {
  if (a === null) {
    return function(b) {
      return b;
    };
  }
  return function(b) {
    if (b === null) {
      return a;
    }
    return [a, b];
  };
};

//----------------------------------------------------------------------------//

exports.empty = null;

exports.fromByteString = function(byteString) {
  if (byteString.length === 0) {
    return null;
  }
  return byteString;
};

exports.fromString = function(string) {
  return function(encoding) {
    if (string.length === 0) {
      return null;
    }
    return new $$String(string, encoding);
  };
};

exports.fromArray = function(array) {
  array = array.filter(function(part) {
    return part !== null;
  });
  if (array.length === 0) {
    return null;
  }
  return array;
};

//----------------------------------------------------------------------------//

exports.isEmpty = function(list) {
  return list === null;
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

//----------------------------------------------------------------------------//

exports.write = function(stream) {
  return function(list) {
    return function(callback) {
      return function() {
        var go = function(list, isLast) {
          if (list === null) {
            if (isLast) {
              callback();
            }
            return true;
          }
          if (list.constructor === Buffer) {
            if (isLast) {
              return stream.write(list, callback);
            } else {
              stream.write(list);
              return;
            }
          }
          if (list.constructor === Array) {
            var length = list.length;
            for (var i = 0; i < length - 1; ++i) {
              go(list[i], false);
            }
            if (length !== 0) {
              if (isLast) {
                return go(list[length - 1], true);
              } else {
                go(list[length - 1], false);
                return;
              }
            }
            return;
          }
          var nodeEncoding = Node_Encoding.encodingToNode(list.encoding);
          if (isLast) {
            return stream.write(list.string, nodeEncoding, callback);
          } else {
            stream.write(list.string, nodeEncoding);
            return;
          }
        };
        return go(list, true);
      };
    };
  };
};
