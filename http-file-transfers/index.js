"use strict";

const http = require("node:http");

const actions = require("./actions");
const { STATUS_CODES, REASON_PHRASES } = require("./libs");

const API_PORT = 3033;
const HANDLERS = {
  "/upload-with-form": actions.uploadWithForm,
  "/upload-with-base64": actions.uploadWithBase64,
  "/upload-with-data-binary": actions.uploadWithDataBinary,
};

http
  .createServer(async (req, res) => {
    try {
      const { url } = req;

      const body = await HANDLERS[url](req);

      if (body) {
        return res.end(JSON.stringify({ status: STATUS_CODES.OK, body }));
      } else {
        res.end(
          JSON.stringify({
            status: STATUS_CODES.NOT_FOUND,
            body: { message: REASON_PHRASES.NOT_FOUND },
          })
        );
      }
    } catch (error) {
      console.error(error);
      res.end(
        JSON.stringify({
          status: STATUS_CODES.INTERNAL_SERVER_ERROR,
          body: { message: REASON_PHRASES.INTERNAL_SERVER_ERROR },
        })
      );
    }
  })
  .listen(API_PORT, () =>
    console.log(`HTTP server started on port ${API_PORT}.`)
  );
