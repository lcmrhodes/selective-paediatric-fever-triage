import assert from "node:assert/strict";
import test from "node:test";
import {
  calculateCompatibleStage2,
  calculateStage1,
  calculateStage2,
  calculateWeightForAgeZScore,
  classify,
  PredictionError
} from "../../app/src/engine";
import type { Classification, Stage1Input } from "../../app/src/model-types";
import { loadBrowserBundle, readJson, testIdentity } from "./helpers";

interface PredictionFixtures {
  stage1: Array<{
    id: string;
    input: Stage1Input;
    raw_probability: number;
    probability: number;
    classification: Classification;
  }>;
  stage2: {
    stage1_probability: number;
    measures: Record<string, number>;
    expected: Array<{ strategy: string; probability: number; classification: Classification }>;
  };
  stage2_missing: {
    stage1_probability: number;
    strategy: string;
    measures: Record<string, null>;
    probability: number;
    classification: Classification;
    imputed: string[];
  };
  thresholds: Array<{ probability: number; classification: Classification }>;
}

// Fixed scientific fixtures —-

const bundle = await loadBrowserBundle();
const fixtures = await readJson<PredictionFixtures>("tests/fixtures/prediction_fixtures.json");

test("Stage 1 fixed Green, Amber, Red, missing, and range fixtures match", () => {
  for (const fixture of fixtures.stage1) {
    const result = calculateStage1(fixture.input, bundle, testIdentity);
    assert.ok(Math.abs(result.raw_probability - fixture.raw_probability) <= 1e-12, fixture.id);
    assert.ok(Math.abs(result.probability - fixture.probability) <= 1e-12, fixture.id);
    assert.equal(result.classification, fixture.classification, fixture.id);
    assert.equal(result.stage2_available, fixture.classification === "AMBER", fixture.id);
  }
});

test("traffic-light thresholds preserve the exact boundary rules", () => {
  for (const fixture of fixtures.thresholds) {
    assert.equal(classify(fixture.probability, bundle), fixture.classification);
  }
});

test("all seven fixed Stage 2 strategies and missing-value behavior match", () => {
  for (const fixture of fixtures.stage2.expected) {
    const result = calculateStage2(
      fixtures.stage2.stage1_probability,
      fixture.strategy,
      fixtures.stage2.measures,
      bundle,
      testIdentity
    );
    assert.ok(Math.abs(result.probability - fixture.probability) <= 1e-12, fixture.strategy);
    assert.equal(result.classification, fixture.classification, fixture.strategy);
  }

  const missing = fixtures.stage2_missing;
  const result = calculateStage2(
    missing.stage1_probability,
    missing.strategy,
    missing.measures,
    bundle,
    testIdentity
  );
  assert.ok(Math.abs(result.probability - missing.probability) <= 1e-12);
  assert.equal(result.classification, missing.classification);
  assert.deepEqual(result.imputed, missing.imputed);
});

test("compatible strategy comparison includes only strategies supported by supplied measures", () => {
  const result = calculateCompatibleStage2(
    fixtures.stage2.stage1_probability,
    { spo2: 96, strem1: null, crp: null, glucose: null },
    bundle,
    testIdentity
  );
  assert.deepEqual(result.results.map((item) => item.strategy), ["spo2"]);
});

// Fail-closed validation —-

function expectPredictionError(action: () => unknown, field?: string): void {
  assert.throws(action, (error: unknown) => {
    assert.ok(error instanceof PredictionError);
    if (field) assert.equal(error.field, field);
    return true;
  });
}

test("Stage 1 rejects malformed, categorical, and out-of-range input", () => {
  expectPredictionError(() => calculateStage1(null as unknown as Stage1Input, bundle, testIdentity));
  expectPredictionError(
    () => calculateStage1({ ...fixtures.stage1[0].input, sex: 2 }, bundle, testIdentity),
    "sex"
  );
  expectPredictionError(
    () => calculateStage1({ ...fixtures.stage1[0].input, "age.months": 217 }, bundle, testIdentity),
    "age.months"
  );
  expectPredictionError(
    () => calculateStage1({ ...fixtures.stage1[0].input, "hr.all": Number.POSITIVE_INFINITY }, bundle, testIdentity),
    "hr.all"
  );
});

test("Stage 2 is gated to Amber and rejects unknown strategies or invalid measures", () => {
  expectPredictionError(() => calculateStage2(0.004, "spo2", { spo2: 96 }, bundle, testIdentity));
  expectPredictionError(() => calculateStage2(0.03, "spo2", { spo2: 96 }, bundle, testIdentity));
  expectPredictionError(() => calculateStage2(0.01, "not-a-strategy", {}, bundle, testIdentity), "strategy");
  expectPredictionError(() => calculateStage2(0.01, "spo2", { spo2: 101 }, bundle, testIdentity), "spo2");
});

test("weight helper rejects unsupported age, sex, units, and weight", () => {
  expectPredictionError(
    () => calculateWeightForAgeZScore({ "age.months": 60, sex: 0, weight: 12, unit: "kg" }, bundle),
    "age.months"
  );
  expectPredictionError(
    () => calculateWeightForAgeZScore({ "age.months": 24, sex: 2, weight: 12, unit: "kg" }, bundle),
    "sex"
  );
  expectPredictionError(
    () => calculateWeightForAgeZScore({ "age.months": 24, sex: 0, weight: 12, unit: "stone" }, bundle),
    "unit"
  );
  expectPredictionError(
    () => calculateWeightForAgeZScore({ "age.months": 24, sex: 0, weight: 0.4, unit: "kg" }, bundle),
    "weight"
  );
});
