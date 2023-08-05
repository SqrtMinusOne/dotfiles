// [[file:../../Desktop.org::*Firefox Color][Firefox Color:1]]
const JsonUrl = require('json-url');
const jsonCodec = JsonUrl('lzma');

const json = JSON.parse(process.argv[2]);
jsonCodec.compress(json).then((r) => process.stdout.write(r));
// Firefox Color:1 ends here
