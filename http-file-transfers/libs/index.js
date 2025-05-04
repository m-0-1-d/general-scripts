"use strict";

const MIME_TYPES = require("./mimeTypes.js");
const STATUS_CODES = require("./statusCodes.js");
const REASON_PHRASES = require("./reasonPhrases.js");
const readRequestBody = require("./readRequestBody.js");

module.exports = {
  MIME_TYPES,
  STATUS_CODES,
  REASON_PHRASES,
  readRequestBody,
};
