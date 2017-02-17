'use strict';

var memoryStreams = require('memory-streams');

exports.newMemoryStream = function() {
  return new memoryStreams.WritableStream();
};

exports.memoryStreamToByteString = function(stream) {
  return function() {
    return stream.toBuffer();
  };
};
