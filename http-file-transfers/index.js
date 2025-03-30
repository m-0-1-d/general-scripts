"use strict";

const http = require("node:http");

const actions = require("./actions");
const { STATUS_CODES, REASON_PHRASES } = require("./libs");

const API_PORT = 3033;

http
  .createServer(async (req, res) => {
    try {
      const { url } = req;

      let body = null;

      switch (url) {
        case "/upload-with-form":
          body = await actions.uploadWithForm(req);
          break;
        case "/upload-with-data-binary":
          body = await actions.uploadWithDataBinary(req);
          break;
        case "/upload-with-base64":
          body = await actions.uploadWithBase64(req);
          break;
      }

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
