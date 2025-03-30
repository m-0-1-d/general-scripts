"use strict";

const path = require("node:path");
const fsp = require("node:fs").promises;

const { readRequestBody } = require("../libs");

exports.uploadWithForm = async (req) => {
  const requestBuffer = await readRequestBody(req);
  const requestString = requestBuffer.toString("hex");

  const [, boundary] = req.headers["content-type"].split("boundary=");
  const boundaryHex = Buffer.from(`--${boundary}`).toString("hex");

  const [_, imageHex, nameHex] = requestString.split(boundaryHex);

  const nameData = Buffer.from(nameHex, "hex")
    .toString("utf8")
    .split("\n")
    .filter((item) => item.length > 1);

  const [imageMetadata] = Buffer.from(imageHex, "hex")
    .toString("utf8")
    .split("\r\n\r\n");
  const imageMetadataHex = Buffer.from(imageMetadata + "\r\n\r\n").toString(
    "hex"
  );

  const imageHexWithoutMetadata = imageHex.replace(imageMetadataHex, "");
  const imageBuffer = Buffer.from(imageHexWithoutMetadata, "hex");
  const fileName = `upload_with_form_${Date.now()}.jpg`;
  const filePath = path.join(__dirname, "..", "images", fileName);
  await fsp.writeFile(filePath, imageBuffer);

  return { nameData, fileName };
};
