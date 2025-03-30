"use strict";

const path = require("node:path");
const fsp = require("node:fs").promises;

const { readRequestBody } = require("../libs");

exports.uploadWithBase64 = async (req) => {
  const files = JSON.parse((await readRequestBody(req)).toString());

  const filesNames = [];
  await Promise.all(
    files.map(async (file) => {
      const buffer = Buffer.from(file.content, "base64");
      filesNames.push(file.name);
      const [ name, extension ] = file.name.split(".");
      const fileName = `upload_with_base64_${name}${Date.now()}.${extension}`;
      const filePath = path.join(__dirname, "..", "images", fileName);
      await fsp.writeFile(filePath, buffer);
    })
  );

  return filesNames;
};
