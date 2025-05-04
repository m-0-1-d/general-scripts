"use strict";

const http = require("node:http");
const path = require("node:path");
const fsp = require("node:fs").promises;

(async () => {
  const pathFile1 = path.join(__dirname, "..", "images", "test1.png");
  const pathFile2 = path.join(__dirname, "..", "images", "test2.png");

  const file1 = await fsp.readFile(pathFile1);
  const file2 = await fsp.readFile(pathFile2);

  const data = JSON.stringify([
    { name: "test1.png", content: file1.toString("base64") },
    { name: "test2.png", content: file2.toString("base64") },
  ]);
  const options = {
    port: 3033,
    method: "PUT",
    hostname: "localhost",
    path: "/upload-with-base64",
    headers: {
      "Content-Type": "application/json",
      "Content-Length": Buffer.byteLength(data),
    },
  };

  const req = http.request(options, (res) => {
    let responseData = "";
    res.on("data", (chunk) => {
      responseData += chunk;
    });
    res.on("end", () => {
      console.log(responseData);
    });
  });

  req.on("error", (error) => {
    console.error(error);
  });

  req.write(data);
  req.end();
})();
