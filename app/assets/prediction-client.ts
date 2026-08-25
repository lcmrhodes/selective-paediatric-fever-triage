// Local worker transport —-

type WorkerFailure = Error & { field?: string };

interface PendingRequest {
  resolve(value: unknown): void;
  reject(reason: WorkerFailure): void;
}

interface SpotReleaseConfig {
  workerUrl: string;
  appVersion: string;
  modelVersion: string;
  modelId: string;
}

declare global {
  interface Window {
    SPOT_RELEASE?: SpotReleaseConfig;
  }
}

const release = window.SPOT_RELEASE;
if (!release?.workerUrl || typeof Worker === "undefined") {
  throw new Error("This browser cannot start the local prediction engine.");
}

const worker = new Worker(release.workerUrl, { type: "module", name: "spot-sepsis-prediction" });
const pending = new Map<number, PendingRequest>();
let nextRequestId = 1;

worker.addEventListener("message", (event) => {
  const response = event.data as {
    id?: number;
    ok?: boolean;
    result?: unknown;
    error?: string;
    field?: string;
  };
  if (typeof response.id !== "number") return;
  const request = pending.get(response.id);
  if (!request) return;
  pending.delete(response.id);
  if (response.ok) {
    request.resolve(response.result);
    return;
  }
  const failure = new Error(response.error || "The local prediction engine failed.") as WorkerFailure;
  failure.field = response.field;
  request.reject(failure);
});

worker.addEventListener("error", () => {
  const failure = new Error("The local prediction engine could not start.") as WorkerFailure;
  for (const request of pending.values()) request.reject(failure);
  pending.clear();
});

function requestPrediction(method: string, payload: Record<string, unknown> = {}): Promise<any> {
  const id = nextRequestId++;
  return new Promise((resolve, reject) => {
    pending.set(id, { resolve, reject });
    worker.postMessage({ id, method, payload });
  });
}

// Prediction operations —-

export const predictionClient = Object.freeze({
  ready() {
    return requestPrediction("ready");
  },

  calculateStage1(input: Record<string, unknown>) {
    return requestPrediction("stage1", input);
  },

  calculateWeightForAgeZScore(input: Record<string, unknown>) {
    return requestPrediction("wfaz", input);
  },

  calculateStage2(input: Record<string, unknown>) {
    return requestPrediction("stage2", input);
  },

  calculateCompatibleStage2(input: Record<string, unknown>) {
    return requestPrediction("stage2-compatible", input);
  }
});
