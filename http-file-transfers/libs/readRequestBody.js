"use strict";

module.exports = async (req) => {
  const buffers = [];
  for await (const chunk of req) buffers.push(chunk);
  if (buffers.length === 0) return null;
  return Buffer.concat(buffers);
};
