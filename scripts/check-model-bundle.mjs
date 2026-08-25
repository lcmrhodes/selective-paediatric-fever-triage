import { createHash } from "node:crypto";
import { readFile, writeFile } from "node:fs/promises";
import path from "node:path";
import process from "node:process";

// Fixed prediction-artifact inventory —-

const root = process.cwd();
const manifestPath = path.join(root, "models", "model_bundle_manifest.json");
const artifactNames = [
  "stage1_preprocessor.rds",
  "stage1_booster.ubj",
  "stage1_calibration.json",
  "stage2_models.json",
  "browser_model_bundle.json"
];

async function inspectArtifact(name) {
  const bytes = await readFile(path.join(root, "models", name));
  return {
    file: name,
    bytes: bytes.byteLength,
    hash: createHash("sha256").update(bytes).digest("hex")
  };
}

const files = await Promise.all(artifactNames.map(inspectArtifact));
const bundle = JSON.parse(await readFile(path.join(root, "models", "browser_model_bundle.json"), "utf8"));
const expected = {
  publication_model_date: "2026-08-24",
  model_version: bundle.model_version,
  participant_rows_included: false,
  hash_algorithm: "SHA-256",
  files,
  verification: {
    stage1_max_absolute_probability_difference: 0,
    stage2_max_absolute_probability_difference: 5.551115123125783e-16,
    verification_rows: 824
  }
};

if (process.argv.includes("--write")) {
  await writeFile(manifestPath, `${JSON.stringify(expected, null, 2)}\n`);
  process.stdout.write(`Wrote ${path.relative(root, manifestPath)}\n`);
} else {
  const observed = JSON.parse(await readFile(manifestPath, "utf8"));
  if (JSON.stringify(observed) !== JSON.stringify(expected)) {
    throw new Error("The prediction-artifact manifest does not match the fixed files.");
  }
  process.stdout.write("Prediction-artifact manifest: PASS\n");
}
