import assert from "node:assert/strict";
import test from "node:test";
import {
  calculateStage1,
  calculateStage2,
  calculateWeightForAgeZScore,
  preprocessStage1
} from "../../app/src/engine";
import type { Classification, Stage1Input } from "../../app/src/model-types";
import { loadBrowserBundle, readJson, testIdentity } from "./helpers";

interface Stage1Reference {
  id: string;
  input: Stage1Input;
  features: number[];
  raw_probability: number;
  probability: number;
  classification: Classification;
}

interface Stage2Reference {
  id: string;
  stage1_probability: number;
  strategy: string;
  measures: Record<string, number | null>;
  probability: number;
  classification: Classification;
  imputed: string[];
}

interface WeightReference {
  id: string;
  input: Record<string, unknown>;
  wfaz: number;
  weight_kg: number;
}

interface ParityReference {
  participant_rows_included: boolean;
  tolerances: {
    stage1_features: number;
    probability: number;
    weight_for_age: number;
  };
  stage1: Stage1Reference[];
  stage2: Stage2Reference[];
  weight_for_age: WeightReference[];
}

// Independent R-to-browser parity —-

const bundle = await loadBrowserBundle();
const reference = await readJson<ParityReference>("tests/fixtures/browser_parity_reference.json");

test("the parity reference contains only manual and deterministic synthetic fixtures", () => {
  assert.equal(reference.participant_rows_included, false);
  assert.equal(reference.stage1.length, 102);
  assert.equal(reference.stage2.length, 84);
});

test("Stage 1 preprocessing and probabilities match the R reference", () => {
  let maximumRawDifference = 0;
  let maximumCalibratedDifference = 0;
  for (const fixture of reference.stage1) {
    const preprocessing = preprocessStage1(fixture.input, bundle);
    assert.equal(preprocessing.features.length, fixture.features.length, fixture.id);
    for (let index = 0; index < fixture.features.length; index += 1) {
      assert.ok(
        Math.abs(preprocessing.features[index] - fixture.features[index]) <= reference.tolerances.stage1_features,
        `${fixture.id}: feature ${index}`
      );
    }

    const result = calculateStage1(fixture.input, bundle, testIdentity);
    const rawDifference = Math.abs(result.raw_probability - fixture.raw_probability);
    const calibratedDifference = Math.abs(result.probability - fixture.probability);
    maximumRawDifference = Math.max(maximumRawDifference, rawDifference);
    maximumCalibratedDifference = Math.max(maximumCalibratedDifference, calibratedDifference);
    assert.ok(rawDifference <= reference.tolerances.probability, `${fixture.id}: raw probability`);
    assert.ok(calibratedDifference <= reference.tolerances.probability, `${fixture.id}: calibrated probability`);
    assert.equal(result.classification, fixture.classification, fixture.id);
  }
  assert.ok(maximumRawDifference <= reference.tolerances.probability);
  assert.ok(maximumCalibratedDifference <= reference.tolerances.probability);
});

test("all Stage 2 strategies match the R reference", () => {
  let maximumDifference = 0;
  for (const fixture of reference.stage2) {
    const result = calculateStage2(
      fixture.stage1_probability,
      fixture.strategy,
      fixture.measures,
      bundle,
      testIdentity
    );
    const difference = Math.abs(result.probability - fixture.probability);
    maximumDifference = Math.max(maximumDifference, difference);
    assert.ok(difference <= reference.tolerances.probability, fixture.id);
    assert.equal(result.classification, fixture.classification, fixture.id);
    assert.deepEqual(result.imputed, fixture.imputed, fixture.id);
  }
  assert.ok(maximumDifference <= reference.tolerances.probability);
});

test("the local weight helper matches the R reference in kilograms and pounds", () => {
  for (const fixture of reference.weight_for_age) {
    const result = calculateWeightForAgeZScore(fixture.input, bundle);
    assert.ok(Math.abs(result.wfaz - fixture.wfaz) <= reference.tolerances.weight_for_age, fixture.id);
    assert.ok(Math.abs(result.weight_kg - fixture.weight_kg) <= reference.tolerances.weight_for_age, fixture.id);
  }
});
