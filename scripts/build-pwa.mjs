import { createHash } from "node:crypto";
import { copyFile, mkdir, readFile, rm, writeFile } from "node:fs/promises";
import path from "node:path";
import process from "node:process";
import { build } from "esbuild";

// Validated release inputs —-

const root = process.cwd();
const output = path.join(root, "dist");
const assetsOutput = path.join(output, "assets");
const modelsOutput = path.join(output, "models");
const packageMetadata = JSON.parse(await readFile(path.join(root, "package.json"), "utf8"));
const modelBytes = await readFile(path.join(root, "models", "browser_model_bundle.json"));
const modelBundle = JSON.parse(modelBytes.toString("utf8"));

function hashBytes(value) {
  return createHash("sha256").update(value).digest("hex");
}

function shortHash(value) {
  return hashBytes(value).slice(0, 12);
}

const appVersion = packageMetadata.version;
const modelVersion = modelBundle.model_version;
const modelHash = hashBytes(modelBytes);
const modelName = `model-${modelHash.slice(0, 12)}.json`;
const modelUrlFromWorker = `../models/${modelName}`;

// Content-hashed application assets —-

await rm(output, { recursive: true, force: true });
await mkdir(assetsOutput, { recursive: true });
await mkdir(modelsOutput, { recursive: true });

const appBuild = await build({
  entryPoints: [path.join(root, "app", "assets", "app.js")],
  bundle: true,
  format: "esm",
  platform: "browser",
  target: ["es2022"],
  minify: true,
  write: false
});
const appCode = appBuild.outputFiles[0].contents;
const appName = `app-${shortHash(appCode)}.js`;
await writeFile(path.join(assetsOutput, appName), appCode);

const workerBuild = await build({
  entryPoints: [path.join(root, "app", "src", "prediction-worker.ts")],
  bundle: true,
  format: "esm",
  platform: "browser",
  target: ["es2022"],
  minify: true,
  write: false,
  define: {
    __SPOT_APP_VERSION__: JSON.stringify(appVersion),
    __SPOT_MODEL_VERSION__: JSON.stringify(modelVersion),
    __SPOT_MODEL_HASH__: JSON.stringify(modelHash),
    __SPOT_MODEL_URL__: JSON.stringify(modelUrlFromWorker)
  }
});
const workerCode = workerBuild.outputFiles[0].contents;
const workerName = `prediction-worker-${shortHash(workerCode)}.js`;
await writeFile(path.join(assetsOutput, workerName), workerCode);

const styles = await readFile(path.join(root, "app", "assets", "styles.css"));
const stylesName = `styles-${shortHash(styles)}.css`;
await writeFile(path.join(assetsOutput, stylesName), styles);

const githubMark = await readFile(path.join(root, "app", "assets", "github-mark.svg"));
const githubName = `github-mark-${shortHash(githubMark)}.svg`;
await writeFile(path.join(assetsOutput, githubName), githubMark);

const icon192 = await readFile(path.join(root, "app", "assets", "icons", "spot-sepsis-192.png"));
const icon512 = await readFile(path.join(root, "app", "assets", "icons", "spot-sepsis-512.png"));
const icon192Name = `icon-192-${shortHash(icon192)}.png`;
const icon512Name = `icon-512-${shortHash(icon512)}.png`;
await writeFile(path.join(assetsOutput, icon192Name), icon192);
await writeFile(path.join(assetsOutput, icon512Name), icon512);
await writeFile(path.join(modelsOutput, modelName), modelBytes);

const releaseConfiguration = `window.SPOT_RELEASE=Object.freeze(${JSON.stringify({
  workerUrl: `./assets/${workerName}`,
  appVersion,
  modelVersion,
  modelId: modelHash.slice(0, 8)
})});\n`;
const releaseName = `release-${shortHash(releaseConfiguration)}.js`;
await writeFile(path.join(assetsOutput, releaseName), releaseConfiguration);

// Subpath-safe document and manifest —-

let html = await readFile(path.join(root, "app", "index.html"), "utf8");
html = html
  .replace("./assets/styles.css", `./assets/${stylesName}`)
  .replace("./assets/app.js", `./assets/${appName}`)
  .replaceAll("./assets/icon-192.png", `./assets/${icon192Name}`)
  .replace("./assets/github-mark.svg", `./assets/${githubName}`)
  .replace(
    `<script type="module" src="./assets/${appName}"></script>`,
    `<script src="./assets/${releaseName}"></script>\n  <script type="module" src="./assets/${appName}"></script>`
  );
await writeFile(path.join(output, "index.html"), html);
await writeFile(path.join(output, "404.html"), html);

const manifest = {
  id: "./",
  name: "Spot Sepsis",
  short_name: "Spot Sepsis",
  description: "Research demonstration of the Spot Sepsis Stage 1 and Stage 2 prediction models.",
  start_url: "./",
  scope: "./",
  display: "standalone",
  background_color: "#fbfaf8",
  theme_color: "#fbfaf8",
  icons: [
    { src: `./assets/${icon192Name}`, sizes: "192x192", type: "image/png", purpose: "any maskable" },
    { src: `./assets/${icon512Name}`, sizes: "512x512", type: "image/png", purpose: "any maskable" }
  ]
};
await writeFile(path.join(output, "manifest.webmanifest"), `${JSON.stringify(manifest, null, 2)}\n`);

// Atomic service-worker cache —-

const precache = [
  "./",
  "./index.html",
  "./404.html",
  "./manifest.webmanifest",
  `./assets/${appName}`,
  `./assets/${workerName}`,
  `./assets/${stylesName}`,
  `./assets/${releaseName}`,
  `./assets/${githubName}`,
  `./assets/${icon192Name}`,
  `./assets/${icon512Name}`,
  `./models/${modelName}`,
  "./release.json"
];
const releaseId = shortHash(JSON.stringify({ appName, workerName, stylesName, modelHash }));
const serviceWorker = `const CACHE_PREFIX="spot-sepsis-release-";
const CACHE_NAME=CACHE_PREFIX+${JSON.stringify(releaseId)};
const PRECACHE=${JSON.stringify(precache)};
self.addEventListener("install",event=>{
  event.waitUntil(caches.open(CACHE_NAME).then(cache=>cache.addAll(PRECACHE)));
});
self.addEventListener("activate",event=>{
  event.waitUntil(caches.keys().then(keys=>Promise.all(keys.filter(key=>key.startsWith(CACHE_PREFIX)&&key!==CACHE_NAME).map(key=>caches.delete(key)))));
});
self.addEventListener("fetch",event=>{
  if(event.request.method!=="GET")return;
  const requestUrl=new URL(event.request.url);
  if(requestUrl.origin!==self.location.origin)return;
  if(event.request.mode==="navigate"){
    event.respondWith(caches.open(CACHE_NAME).then(cache=>cache.match("./index.html")).then(response=>response||Response.error()));
    return;
  }
  event.respondWith(caches.open(CACHE_NAME).then(cache=>cache.match(event.request)).then(response=>response||fetch(event.request)));
});
`;
await writeFile(path.join(output, "service-worker.js"), serviceWorker);

const release = {
  app_version: appVersion,
  model_version: modelVersion,
  model_id: modelHash.slice(0, 8),
  model_bundle_hash: modelHash,
  release_id: releaseId,
  assets: precache
};
await writeFile(path.join(output, "release.json"), `${JSON.stringify(release, null, 2)}\n`);
await writeFile(path.join(output, ".nojekyll"), "");

process.stdout.write(`Built validated static PWA release ${releaseId} in dist/\n`);
