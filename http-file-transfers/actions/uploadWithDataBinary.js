"use strict";

const path = require("node:path");
const fsp = require("node:fs").promises;

const { MIME_TYPES, readRequestBody } = require("../libs");

exports.uploadWithDataBinary = async (req) => {
  const contentName = req.headers["x-filename"];
  const contentType = req.headers["content-type"];
  const contentLength = req.headers["content-length"];

  const extension = MIME_TYPES[contentType];
  if (!extension) throw new Error("Incorrect content type.");

  const requestBuffer = await readRequestBody(req);
  const fileName = `upload_with_data_binary_${Date.now()}${extension}`;
  const filePath = path.join(__dirname, "..", "images", fileName);
  await fsp.writeFile(filePath, requestBuffer);

  return { contentName, contentType, contentLength };
};
