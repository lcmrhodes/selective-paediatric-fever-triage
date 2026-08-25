import assert from "node:assert/strict";
import { createHash } from "node:crypto";
import { readFile } from "node:fs/promises";
import path from "node:path";
import test from "node:test";
import { root } from "./helpers";

// Static-release integrity and subpath safety —-

const dist = path.join(root, "dist");
const release = JSON.parse(await readFile(path.join(dist, "release.json"), "utf8")) as {
  app_version: string;
  model_version: string;
  model_id: string;
  model_bundle_hash: string;
  release_id: string;
  assets: string[];
};
const html = await readFile(path.join(dist, "index.html"), "utf8");
const manifest = JSON.parse(await readFile(path.join(dist, "manifest.webmanifest"), "utf8"));
const serviceWorker = await readFile(path.join(dist, "service-worker.js"), "utf8");

test("the deployed document uses relative assets and an in-scope manifest", () => {
  assert.doesNotMatch(html, /(?:src|href)="\/(?!\/)/);
  assert.equal(manifest.start_url, "./");
  assert.equal(manifest.scope, "./");
  assert.equal(manifest.display, "standalone");
});

test("the application requires the approved research-use acknowledgement", () => {
  assert.match(html, /id="safety-acknowledgement"/);
  assert.match(html, /not for live clinical prediction/);
  assert.match(html, /id="safety-confirmation"/);
  assert.match(html, /id="safety-continue"[^>]*disabled/);
  assert.match(html, /Available offline/);
});

test("the install manifest includes the supplied Spot Sepsis icon sizes", () => {
  const sizes = manifest.icons.map((icon: { sizes: string }) => icon.sizes);
  assert.deepEqual(sizes, ["192x192", "512x512"]);
});

test("the release model hash matches the precached model bytes", async () => {
  const modelAsset = release.assets.find((asset) => asset.startsWith("./models/model-"));
  assert.ok(modelAsset);
  const bytes = await readFile(path.join(dist, modelAsset.slice(2)));
  const observed = createHash("sha256").update(bytes).digest("hex");
  assert.equal(observed, release.model_bundle_hash);
  assert.equal(release.model_id, observed.slice(0, 8));
});

test("the service worker precaches one complete release and does not activate mid-case", () => {
  for (const asset of release.assets) assert.match(serviceWorker, new RegExp(asset.replace(/[.*+?^${}()|[\]\\]/g, "\\$&")));
  assert.match(serviceWorker, /cache\.addAll\(PRECACHE\)/);
  assert.doesNotMatch(serviceWorker, /skipWaiting/);
  assert.match(serviceWorker, /CACHE_PREFIX/);
});
