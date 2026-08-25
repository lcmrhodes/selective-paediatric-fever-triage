// Browser-neutral model contract —-

export type Classification = "GREEN" | "AMBER" | "RED";

export type Stage1Input = Record<string, number | null | undefined>;

export interface RPartSplit {
  feature: string;
  threshold: number;
  less_than_goes_left: boolean;
}

export interface RPartNode {
  id: number;
  value: number;
  primary?: RPartSplit;
  surrogates?: RPartSplit[];
  left?: number;
  right?: number;
  majority?: "left" | "right";
}

export interface RPartTree {
  use_surrogates: number;
  nodes: RPartNode[];
}

export interface Imputer {
  target: string;
  predictors: string[];
  aggregation: "average";
  cast: "numeric" | "rounded_integer";
  trees: RPartTree[];
}

export interface XGBoostTree {
  left: number[];
  right: number[];
  default_left: number[];
  split_index: number[];
  split_condition: number[];
}

export interface Stage2Strategy {
  label: string;
  predictors: string | string[];
  intercept: number;
  coefficients: Record<string, number>;
}

export interface GrowthReference {
  age_days: number[];
  l: number[];
  m: number[];
  s: number[];
}

export interface BrowserModelBundle {
  schema_version: number;
  model_version: string;
  participant_rows_included: boolean;
  stage1: {
    predictors: string[];
    features: string[];
    missing_indicator_prefix: string;
    preprocessing: { imputers: Record<string, Imputer> };
    booster: {
      objective: "binary:logistic";
      base_score: number;
      output: "complement";
      trees: XGBoostTree[];
    };
    calibration: {
      method: string;
      intercept: number;
      slope: number;
      clip_epsilon: number;
    };
    thresholds: {
      green_upper_exclusive: number;
      amber_upper_inclusive: number;
    };
  };
  stage2: {
    offset_coefficient: number;
    medians: Record<string, number>;
    strategies: Record<string, Stage2Strategy>;
  };
  weight_for_age: {
    reference: string;
    source_implementation: string;
    age_months_minimum: number;
    age_months_maximum: number;
    reference_by_sex: {
      male: GrowthReference;
      female: GrowthReference;
    };
  };
}

export interface ReleaseIdentity {
  appVersion: string;
  modelVersion: string;
  modelBundleHash: string;
}

export interface Stage1Result extends ReleaseIdentity {
  raw_probability: number;
  probability: number;
  classification: Classification;
  stage2_available: boolean;
  explanation: string;
  imputed: string[];
}

export interface Stage2Result extends ReleaseIdentity {
  strategy: string;
  label: string;
  probability: number;
  classification: Classification;
  imputed: string[];
  measures: Record<string, number>;
  explanation: string;
}
