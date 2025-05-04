"use strict";

module.exports = async (req) => {
  const buffers = [];
  for await (const chunk of req) buffers.push(chunk);
  return buffers.length === 0 ? null : Buffer.concat(buffers);
};
