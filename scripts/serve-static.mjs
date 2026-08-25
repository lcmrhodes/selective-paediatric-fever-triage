import { createReadStream, existsSync, readFileSync, statSync } from "node:fs";
import { createServer } from "node:http";
import path from "node:path";
import process from "node:process";

// Subpath-aware static development server —-

const root = path.resolve(process.cwd(), "dist");
const host = process.env.HOST || "127.0.0.1";
const port = Number(process.env.PORT || 4173);
const configuredBase = process.env.BASE_PATH || "/";
const corruptModelForQa = process.env.QA_CORRUPT_MODEL === "1";
const basePath = `/${configuredBase.split("/").filter(Boolean).join("/")}${configuredBase === "/" ? "" : "/"}`;
const contentTypes = {
  ".css": "text/css; charset=utf-8",
  ".html": "text/html; charset=utf-8",
  ".js": "text/javascript; charset=utf-8",
  ".json": "application/json; charset=utf-8",
  ".png": "image/png",
  ".svg": "image/svg+xml",
  ".webmanifest": "application/manifest+json; charset=utf-8"
};

createServer((request, response) => {
  const requestUrl = new URL(request.url || "/", `http://${request.headers.host || "localhost"}`);
  if (!requestUrl.pathname.startsWith(basePath)) {
    response.writeHead(404).end("Not found");
    return;
  }
  const relative = decodeURIComponent(requestUrl.pathname.slice(basePath.length));
  let target = path.resolve(root, relative || "index.html");
  if (!target.startsWith(`${root}${path.sep}`) && target !== path.join(root, "index.html")) {
    response.writeHead(404).end("Not found");
    return;
  }
  if (!existsSync(target) || statSync(target).isDirectory()) target = path.join(root, "404.html");
  response.writeHead(200, {
    "Content-Type": contentTypes[path.extname(target)] || "application/octet-stream",
    "Cache-Control": "no-store",
    "Content-Security-Policy": "default-src 'self'; script-src 'self'; style-src 'self'; img-src 'self' data:; connect-src 'self'; worker-src 'self'; manifest-src 'self'; frame-ancestors 'none'; base-uri 'none'; form-action 'self'",
    "Referrer-Policy": "no-referrer",
    "X-Content-Type-Options": "nosniff"
  });
  if (corruptModelForQa && target.includes(`${path.sep}models${path.sep}model-`)) {
    response.end(Buffer.concat([readFileSync(target), Buffer.from(" ")]));
    return;
  }
  createReadStream(target).pipe(response);
}).listen(port, host, () => {
  process.stdout.write(`Static Spot Sepsis PWA: http://${host}:${port}${basePath}\n`);
});
