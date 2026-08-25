import type {
  BrowserModelBundle,
  Classification,
  GrowthReference,
  Imputer,
  ReleaseIdentity,
  RPartSplit,
  RPartTree,
  Stage1Input,
  Stage1Result,
  Stage2Result,
  Stage2Strategy,
  XGBoostTree
} from "./model-types";

// Fixed clinical input contract —-

const STAGE1_RANGES: Record<string, readonly [number, number]> = {
  "age.months": [0, 216],
  wfaz: [-10, 10],
  cidysymp: [0, 60],
  "hr.all": [20, 300],
  "rr.all": [5, 150],
  envhtemp: [25, 45]
};

const STAGE2_RANGES: Record<string, readonly [number, number]> = {
  spo2: [50, 100],
  strem1: [0, 10000],
  crp: [0, 1000],
  glucose: [0, 50]
};

const BINARY_STAGE1_FIELDS = ["sex", "adm.recent", "not.alert", "crt.long"];

export class PredictionError extends Error {
  readonly field?: string;

  constructor(message: string, field?: string) {
    super(message);
    this.name = "PredictionError";
    this.field = field;
  }
}

function nullableNumber(value: unknown, field: string): number {
  if (value === null || value === undefined || value === "") return Number.NaN;
  if (typeof value !== "number" || !Number.isFinite(value)) {
    throw new PredictionError(`${field} must be numeric or missing.`, field);
  }
  return value;
}

function checkRange(value: number, range: readonly [number, number], field: string): void {
  if (!Number.isNaN(value) && (value < range[0] || value > range[1])) {
    throw new PredictionError(`${field} must be between ${range[0]} and ${range[1]}.`, field);
  }
}

export function validateStage1Input(input: unknown, bundle: BrowserModelBundle): Record<string, number> {
  if (input === null || typeof input !== "object" || Array.isArray(input)) {
    throw new PredictionError("Stage 1 input must be an object.");
  }
  const source = input as Record<string, unknown>;
  const values: Record<string, number> = {};
  for (const name of bundle.stage1.predictors) {
    values[name] = nullableNumber(source[name], name);
    if (STAGE1_RANGES[name]) checkRange(values[name], STAGE1_RANGES[name], name);
  }
  for (const name of BINARY_STAGE1_FIELDS) {
    const value = values[name];
    if (!Number.isNaN(value) && value !== 0 && value !== 1) {
      throw new PredictionError("Categorical inputs must use one of the displayed choices.", name);
    }
  }
  return values;
}

function validateStage2Measures(measures: unknown): Record<string, number> {
  const source = measures !== null && typeof measures === "object" && !Array.isArray(measures)
    ? measures as Record<string, unknown>
    : {};
  const values: Record<string, number> = {};
  for (const [name, range] of Object.entries(STAGE2_RANGES)) {
    values[name] = nullableNumber(source[name], name);
    checkRange(values[name], range, name);
  }
  return values;
}

// Fixed preprocessing evaluation —-

function splitDirection(split: RPartSplit, value: number): "left" | "right" {
  const lessThan = value < split.threshold;
  return lessThan === split.less_than_goes_left ? "left" : "right";
}

function evaluateRPartTree(tree: RPartTree, row: Record<string, number>): number {
  const nodes = new Map(tree.nodes.map((node) => [node.id, node]));
  let node = nodes.get(1);
  if (!node) throw new PredictionError("The model bundle contains an invalid imputation tree.");

  while (node.primary) {
    let direction: "left" | "right" | undefined;
    const primaryValue = row[node.primary.feature];
    if (Number.isFinite(primaryValue)) direction = splitDirection(node.primary, primaryValue);

    if (!direction && tree.use_surrogates > 0) {
      for (const surrogate of node.surrogates ?? []) {
        const value = row[surrogate.feature];
        if (Number.isFinite(value)) {
          direction = splitDirection(surrogate, value);
          break;
        }
      }
    }

    if (!direction && tree.use_surrogates === 2) direction = node.majority;
    if (!direction) return node.value;
    const childId = direction === "left" ? node.left : node.right;
    if (childId === undefined) throw new PredictionError("The model bundle contains an invalid tree path.");
    node = nodes.get(childId);
    if (!node) throw new PredictionError("The model bundle contains an invalid tree node.");
  }
  return node.value;
}

function evaluateImputer(imputer: Imputer, row: Record<string, number>): number {
  let sum = 0;
  for (const tree of imputer.trees) sum += evaluateRPartTree(tree, row);
  const value = sum / imputer.trees.length;
  return imputer.cast === "rounded_integer" ? roundTiesToEven(value) : value;
}

export function preprocessStage1(
  input: unknown,
  bundle: BrowserModelBundle
): { features: number[]; featureMap: Record<string, number>; imputed: string[] } {
  const values = validateStage1Input(input, bundle);
  const original: Record<string, number> = { ...values };
  for (const name of bundle.stage1.predictors) {
    original[`${bundle.stage1.missing_indicator_prefix}${name}`] = Number(Number.isNaN(values[name]));
  }

  const processed = { ...original };
  const imputed: string[] = [];
  for (const imputer of Object.values(bundle.stage1.preprocessing.imputers)) {
    if (!Number.isNaN(processed[imputer.target])) continue;
    const predictionRow = Object.fromEntries(imputer.predictors.map((name) => [name, original[name]]));
    processed[imputer.target] = evaluateImputer(imputer, predictionRow);
    imputed.push(imputer.target);
  }

  const features = bundle.stage1.features.map((name) => processed[name]);
  if (features.some((value) => !Number.isFinite(value))) {
    throw new PredictionError("Stage 1 preprocessing could not resolve all required features.");
  }
  return {
    features,
    featureMap: Object.fromEntries(bundle.stage1.features.map((name, index) => [name, features[index]])),
    imputed: imputed.filter((name) => bundle.stage1.predictors.includes(name))
  };
}

// Fixed XGBoost evaluation and calibration —-

function logit(probability: number): number {
  return Math.log(probability / (1 - probability));
}

function logistic(value: number): number {
  if (value >= 0) return 1 / (1 + Math.exp(-value));
  const exponential = Math.exp(value);
  return exponential / (1 + exponential);
}

function xgboostFloatLogit(probability: number): number {
  const float = Math.fround;
  const ratio = float(float(1) / float(probability));
  return float(-float(Math.log(float(ratio - float(1)))));
}

function xgboostFloatLogistic(margin: number): number {
  const float = Math.fround;
  const exponent = float(Math.exp(float(Math.min(float(-margin), float(88.7)))));
  const denominator = float(float(exponent + float(1)) + float(1e-16));
  return float(float(1) / denominator);
}

function evaluateXGBoostTree(tree: XGBoostTree, features: number[]): number {
  let node = 0;
  while (tree.left[node] !== -1) {
    const value = features[tree.split_index[node]];
    // The R reference constructs a sparse matrix, so exact zero values follow the missing branch.
    const missing = !Number.isFinite(value) || value === 0;
    const goLeft = missing ? tree.default_left[node] === 1 : value < tree.split_condition[node];
    node = goLeft ? tree.left[node] : tree.right[node];
    if (node < 0 || node >= tree.left.length) {
      throw new PredictionError("The model bundle contains an invalid XGBoost tree path.");
    }
  }
  return tree.split_condition[node];
}

export function predictStage1Raw(features: number[], bundle: BrowserModelBundle): number {
  const booster = bundle.stage1.booster;
  if (booster.objective !== "binary:logistic" || booster.output !== "complement") {
    throw new PredictionError("The model bundle uses an unsupported Stage 1 objective.");
  }
  let margin = xgboostFloatLogit(booster.base_score);
  for (const tree of booster.trees) {
    margin = Math.fround(margin + Math.fround(evaluateXGBoostTree(tree, features)));
  }
  return 1 - xgboostFloatLogistic(margin);
}

export function classify(probability: number, bundle: BrowserModelBundle): Classification {
  if (probability < bundle.stage1.thresholds.green_upper_exclusive) return "GREEN";
  if (probability <= bundle.stage1.thresholds.amber_upper_inclusive) return "AMBER";
  return "RED";
}

function classificationExplanation(classification: Classification, stage: 1 | 2): string {
  if (classification === "GREEN") {
    return stage === 1
      ? "Calibrated probability of death or organ support within two days. The result is Green because it is below 0.5%. Stage 2 is not applied because it is reserved for Amber results."
      : "The selected Stage 2 strategy places this research-model result below 0.5%.";
  }
  if (classification === "AMBER") {
    return stage === 1
      ? "Calibrated probability of death or organ support within two days. The result is Amber because it is between 0.5% and 2%. Stage 2 may be applied because additional information can help resolve this remaining uncertainty."
      : "The selected Stage 2 strategy retains this research-model result in the 0.5% through 2% range.";
  }
  return stage === 1
    ? "Calibrated probability of death or organ support within two days. The result is Red because it is above 2%. Stage 2 is not applied because it is reserved for Amber results."
    : "";
}

export function calculateStage1(
  input: Stage1Input,
  bundle: BrowserModelBundle,
  identity: ReleaseIdentity
): Stage1Result {
  const preprocessed = preprocessStage1(input, bundle);
  const rawProbability = predictStage1Raw(preprocessed.features, bundle);
  const calibration = bundle.stage1.calibration;
  const clipped = Math.min(Math.max(rawProbability, calibration.clip_epsilon), 1 - calibration.clip_epsilon);
  const probability = logistic(calibration.intercept + calibration.slope * logit(clipped));
  const classification = classify(probability, bundle);
  return {
    ...identity,
    raw_probability: rawProbability,
    probability,
    classification,
    stage2_available: classification === "AMBER",
    explanation: classificationExplanation(classification, 1),
    imputed: preprocessed.imputed
  };
}

// Fixed Stage 2 evaluation —-

function strategyPredictors(strategy: Stage2Strategy): string[] {
  return Array.isArray(strategy.predictors) ? strategy.predictors : [strategy.predictors];
}

function requireAmberStage1(probability: unknown, bundle: BrowserModelBundle): number {
  if (typeof probability !== "number" || !Number.isFinite(probability) || probability < 0 || probability > 1) {
    throw new PredictionError("Stage 1 probability must be one finite value from 0 to 1.", "stage1_probability");
  }
  if (classify(probability, bundle) !== "AMBER") {
    throw new PredictionError("Stage 2 is available only after an Amber Stage 1 result.", "stage1_probability");
  }
  return probability;
}

export function calculateStage2(
  stage1Probability: unknown,
  strategyId: unknown,
  measures: unknown,
  bundle: BrowserModelBundle,
  identity: ReleaseIdentity
): Stage2Result {
  const probability = requireAmberStage1(stage1Probability, bundle);
  if (typeof strategyId !== "string" || !bundle.stage2.strategies[strategyId]) {
    throw new PredictionError("Select a Stage 2 strategy.", "strategy");
  }
  const strategy = bundle.stage2.strategies[strategyId];
  const supplied = validateStage2Measures(measures);
  const values: Record<string, number> = {};
  const imputed: string[] = [];
  for (const name of strategyPredictors(strategy)) {
    let value = supplied[name];
    if (Number.isNaN(value)) {
      value = bundle.stage2.medians[name];
      imputed.push(name);
    }
    values[name] = value;
  }

  const epsilon = bundle.stage1.calibration.clip_epsilon;
  const clipped = Math.min(Math.max(probability, epsilon), 1 - epsilon);
  let linear = logit(clipped) + strategy.intercept;
  for (const name of strategyPredictors(strategy)) linear += strategy.coefficients[name] * values[name];
  const stage2Probability = logistic(linear);
  const classification = classify(stage2Probability, bundle);
  return {
    ...identity,
    strategy: strategyId,
    label: strategy.label,
    probability: stage2Probability,
    classification,
    imputed,
    measures: values,
    explanation: classificationExplanation(classification, 2)
  };
}

export function calculateCompatibleStage2(
  stage1Probability: unknown,
  measures: unknown,
  bundle: BrowserModelBundle,
  identity: ReleaseIdentity
): { results: Stage2Result[] } {
  const supplied = validateStage2Measures(measures);
  const results = Object.entries(bundle.stage2.strategies)
    .filter(([, strategy]) => strategyPredictors(strategy).every((name) => Number.isFinite(supplied[name])))
    .map(([id]) => calculateStage2(stage1Probability, id, measures, bundle, identity));
  if (!results.length) {
    throw new PredictionError("Enter enough Stage 2 measures to support at least one strategy.");
  }
  return { results };
}

// Weight-for-age helper —-

function roundTiesToEven(value: number): number {
  const floor = Math.floor(value);
  const fraction = value - floor;
  if (fraction < 0.5) return floor;
  if (fraction > 0.5) return floor + 1;
  return floor % 2 === 0 ? floor : floor + 1;
}

function interpolate(reference: GrowthReference, ageDays: number, values: number[]): number {
  const ages = reference.age_days;
  let low = 0;
  let high = ages.length - 1;
  while (low <= high) {
    const middle = Math.floor((low + high) / 2);
    if (ages[middle] === ageDays) return values[middle];
    if (ages[middle] < ageDays) low = middle + 1;
    else high = middle - 1;
  }
  if (high < 0 || low >= ages.length) throw new PredictionError("Age is outside the weight reference.");
  const fraction = (ageDays - ages[high]) / (ages[low] - ages[high]);
  return values[high] + fraction * (values[low] - values[high]);
}

export function calculateWeightForAgeZScore(
  payload: Record<string, unknown>,
  bundle: BrowserModelBundle
): { wfaz: number; weight_kg: number; reference: string; implementation: string } {
  const ageMonths = nullableNumber(payload["age.months"], "age.months");
  const sex = nullableNumber(payload.sex, "sex");
  const weight = nullableNumber(payload.weight, "weight");
  const unit = payload.unit;
  const reference = bundle.weight_for_age;

  if (Number.isNaN(ageMonths)) throw new PredictionError("Enter age before calculating the z score.", "age.months");
  if (ageMonths < reference.age_months_minimum || ageMonths > reference.age_months_maximum) {
    throw new PredictionError("Weight-based calculation is available for ages 1 to 59 months.", "age.months");
  }
  if (Number.isNaN(sex)) throw new PredictionError("Select sex before calculating the z score.", "sex");
  if (sex !== 0 && sex !== 1) throw new PredictionError("Select one of the displayed sex choices.", "sex");
  if (Number.isNaN(weight)) throw new PredictionError("Enter weight before calculating the z score.", "weight");
  if (unit !== "kg" && unit !== "lb") throw new PredictionError("Select kilograms or pounds.", "unit");

  const weightKg = unit === "lb" ? weight / 2.2046226218487757 : weight;
  if (weightKg < 0.5 || weightKg > 50) {
    throw new PredictionError("Weight must be between 0.5 and 50 kg (1.1 and 110.2 lb).", "weight");
  }

  const ageDays = roundTiesToEven(ageMonths * (365.25 / 12));
  const curve = sex === 1 ? reference.reference_by_sex.male : reference.reference_by_sex.female;
  const l = interpolate(curve, ageDays, curve.l);
  const m = interpolate(curve, ageDays, curve.m);
  const s = interpolate(curve, ageDays, curve.s);
  let z = ((weightKg / m) ** l - 1) / (l * s);
  const sd3Positive = m * (1 + l * s * 3) ** (1 / l);
  const sd2Positive = m * (1 + l * s * 2) ** (1 / l);
  const sd3Negative = m * (1 + l * s * -3) ** (1 / l);
  const sd2Negative = m * (1 + l * s * -2) ** (1 / l);
  if (z > 3) z = 3 + (weightKg - sd3Positive) / (sd3Positive - sd2Positive);
  if (z < -3) z = -3 + (weightKg - sd3Negative) / (sd2Negative - sd3Negative);

  const rounded = Math.round((z + Number.EPSILON) * 100) / 100;
  checkRange(rounded, STAGE1_RANGES.wfaz, "Calculated z score");
  return {
    wfaz: rounded,
    weight_kg: weightKg,
    reference: reference.reference,
    implementation: reference.source_implementation
  };
}
