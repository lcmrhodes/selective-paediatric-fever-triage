import { readFile } from "node:fs/promises";
import path from "node:path";
import type { BrowserModelBundle, ReleaseIdentity } from "../../app/src/model-types";

// Shared browser-test inputs —-

export const root = process.cwd();

export async function readJson<T>(relativePath: string): Promise<T> {
  return JSON.parse(await readFile(path.join(root, relativePath), "utf8")) as T;
}

export async function loadBrowserBundle(): Promise<BrowserModelBundle> {
  return readJson<BrowserModelBundle>("models/browser_model_bundle.json");
}

export const testIdentity: ReleaseIdentity = {
  appVersion: "test",
  modelVersion: "2026-08-24",
  modelBundleHash: "test-model-id"
};
