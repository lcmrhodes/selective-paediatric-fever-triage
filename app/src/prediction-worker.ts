/// <reference lib="webworker" />

import {
  PredictionError,
  calculateCompatibleStage2,
  calculateStage1,
  calculateStage2,
  calculateWeightForAgeZScore
} from "./engine";
import type { BrowserModelBundle, ReleaseIdentity, Stage1Input } from "./model-types";

declare const __SPOT_APP_VERSION__: string;
declare const __SPOT_MODEL_VERSION__: string;
declare const __SPOT_MODEL_HASH__: string;
declare const __SPOT_MODEL_URL__: string;

const workerScope = self as unknown as DedicatedWorkerGlobalScope;

// Model-bundle integrity and release checks —-

function bytesToHex(bytes: ArrayBuffer): string {
  return [...new Uint8Array(bytes)].map((value) => value.toString(16).padStart(2, "0")).join("");
}

async function loadBundle(): Promise<{ bundle: BrowserModelBundle; identity: ReleaseIdentity }> {
  if (!globalThis.crypto?.subtle) throw new Error("This browser cannot verify the prediction model.");
  const response = await fetch(new URL(__SPOT_MODEL_URL__, workerScope.location.href), {
    cache: "no-store",
    credentials: "same-origin"
  });
  if (!response.ok) throw new Error("The prediction model is unavailable.");
  const bytes = await response.arrayBuffer();
  const observedHash = bytesToHex(await crypto.subtle.digest("SHA-256", bytes));
  if (observedHash !== __SPOT_MODEL_HASH__) {
    throw new Error("The prediction model did not pass its integrity check.");
  }

  const bundle = JSON.parse(new TextDecoder().decode(bytes)) as BrowserModelBundle;
  if (
    bundle.schema_version !== 1 ||
    bundle.model_version !== __SPOT_MODEL_VERSION__ ||
    bundle.participant_rows_included !== false
  ) {
    throw new Error("The prediction model is incompatible with this application release.");
  }
  if (bundle.stage1.booster.trees.length !== 5000 || Object.keys(bundle.stage2.strategies).length !== 7) {
    throw new Error("The prediction model is incomplete.");
  }

  return {
    bundle,
    identity: {
      appVersion: __SPOT_APP_VERSION__,
      modelVersion: __SPOT_MODEL_VERSION__,
      modelBundleHash: __SPOT_MODEL_HASH__
    }
  };
}

const model = loadBundle();

// Worker request dispatch —-

workerScope.addEventListener("message", async (event: MessageEvent) => {
  const request = event.data as { id?: number; method?: string; payload?: Record<string, unknown> };
  if (typeof request.id !== "number" || typeof request.method !== "string") return;

  try {
    const { bundle, identity } = await model;
    const payload = request.payload ?? {};
    let result: unknown;
    switch (request.method) {
      case "ready":
        result = identity;
        break;
      case "stage1":
        result = calculateStage1(payload as Stage1Input, bundle, identity);
        break;
      case "wfaz":
        result = calculateWeightForAgeZScore(payload, bundle);
        break;
      case "stage2":
        result = calculateStage2(
          payload.stage1_probability,
          payload.strategy,
          payload.measures,
          bundle,
          identity
        );
        break;
      case "stage2-compatible":
        result = calculateCompatibleStage2(
          payload.stage1_probability,
          payload.measures,
          bundle,
          identity
        );
        break;
      default:
        throw new PredictionError("Unknown prediction operation.");
    }
    workerScope.postMessage({ id: request.id, ok: true, result });
  } catch (error) {
    const failure = error instanceof Error ? error : new Error("Prediction failed.");
    workerScope.postMessage({
      id: request.id,
      ok: false,
      error: failure.message,
      field: failure instanceof PredictionError ? failure.field : undefined
    });
  }
});
