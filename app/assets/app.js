import { predictionClient } from "./prediction-client.ts";

// Fixed Stage 2 strategy definitions —-

const strategies = {
  spo2: { label: "SpO₂", measures: ["spo2"] },
  strem1: { label: "sTREM-1", measures: ["strem1"] },
  crp_strem1: { label: "CRP + sTREM-1", measures: ["crp", "strem1"] },
  crp_strem1_glucose: { label: "CRP + sTREM-1 + glucose", measures: ["crp", "strem1", "glucose"] },
  spo2_strem1: { label: "SpO₂ + sTREM-1", measures: ["spo2", "strem1"] },
  spo2_strem1_crp: { label: "SpO₂ + sTREM-1 + CRP", measures: ["spo2", "strem1", "crp"] },
  spo2_strem1_crp_glucose: { label: "SpO₂ + sTREM-1 + CRP + glucose", measures: ["spo2", "strem1", "crp", "glucose"] }
};

const measureLabels = {
  spo2: "oxygen saturation",
  strem1: "sTREM-1",
  crp: "CRP",
  glucose: "glucose"
};

const validationFields = {
  "age.months": { label: "Age", units: "months" },
  sex: { label: "Sex" },
  "adm.recent": { label: "Hospital admission in the preceding 6 months" },
  wfaz: { label: "Weight-for-age z score" },
  cidysymp: { label: "Illness duration", units: "days" },
  "not.alert": { label: "Alertness" },
  "hr.all": { label: "Heart rate", units: "beats/min" },
  "rr.all": { label: "Respiratory rate", units: "breaths/min" },
  envhtemp: { label: "Axillary temperature", units: "°C" },
  "crt.long": { label: "Capillary refill" },
  strategy: { label: "Stage 2 strategy" },
  spo2: { label: "Oxygen saturation", units: "%" },
  strem1: { label: "sTREM-1", units: "pg/mL" },
  crp: { label: "C-reactive protein", units: "mg/L" },
  glucose: { label: "Glucose", units: "mmol/L" }
};

// Cached interface elements —-

const stage1Form = document.querySelector("#stage1-form");
const stage2Form = document.querySelector("#stage2-form");
const stage1Result = document.querySelector("#stage1-result");
const stage2Result = document.querySelector("#stage2-result");
const stage1Step = document.querySelector("#stage1-step");
const stage2Step = document.querySelector("#stage2-step");
const comparison = document.querySelector("#comparison");
const ageInput = stage1Form.querySelector('[name="age.months"]');
const sexInput = stage1Form.querySelector('[name="sex"]');
const wfazInput = stage1Form.querySelector('[name="wfaz"]');
const wfazHelperToggle = document.querySelector("#wfaz-helper-toggle");
const wfazHelper = document.querySelector("#wfaz-helper");
const wfazWeight = document.querySelector("#wfaz-weight");
const wfazWeightUnit = document.querySelector("#wfaz-weight-unit");
const wfazWeightControl = document.querySelector("#wfaz-weight-control");
const wfazHelperError = document.querySelector("#wfaz-helper-error");
const wfazHelperApply = document.querySelector("#wfaz-helper-apply");
const stage1Submit = document.querySelector("#stage1-submit");
const stage2Submit = document.querySelector("#stage2-submit");
const compareButton = document.querySelector("#compare-button");
const startupError = document.querySelector("#startup-error");
const releaseInfo = document.querySelector("#release-info");
const safetyDialog = document.querySelector("#safety-acknowledgement");
const safetyConfirmation = document.querySelector("#safety-confirmation");
const safetyContinue = document.querySelector("#safety-continue");

// Research-use acknowledgement gate —-

function setApplicationInert(inert) {
  document.querySelectorAll(".site-header, main, footer").forEach((surface) => {
    surface.toggleAttribute("inert", inert);
  });
}

function openSafetyAcknowledgement() {
  setApplicationInert(true);
  document.body.classList.add("safety-gate-open");

  if (typeof safetyDialog.showModal === "function") {
    safetyDialog.removeAttribute("open");
    safetyDialog.showModal();
  } else {
    safetyDialog.setAttribute("open", "");
  }

  safetyConfirmation.focus({ preventScroll: true });
}

function acceptSafetyAcknowledgement() {
  if (!safetyConfirmation.checked) return;
  if (typeof safetyDialog.close === "function") safetyDialog.close();
  else safetyDialog.removeAttribute("open");
  document.body.classList.remove("safety-gate-open");
  setApplicationInert(false);
  stage1Step.focus({ preventScroll: true });
}

safetyConfirmation.addEventListener("change", () => {
  safetyContinue.disabled = !safetyConfirmation.checked;
});
safetyContinue.addEventListener("click", acceptSafetyAcknowledgement);
safetyDialog.addEventListener("cancel", (event) => event.preventDefault());

// In-memory assessment state —-

const state = {
  stage1Probability: null,
  stage1Classification: null,
  activeStage: 1,
  wfazCalculated: false
};

// Field-level validation interface —-

function prepareFieldErrorUI() {
  document.querySelectorAll(".field > input, .field > select").forEach((control, index) => {
    const shell = document.createElement("span");
    shell.className = "control-shell";
    shell.classList.toggle("has-select", control instanceof HTMLSelectElement);
    control.before(shell);
    shell.append(control);

    const errorUI = document.createElement("span");
    errorUI.className = "field-error-ui";
    errorUI.hidden = true;

    const indicator = document.createElement("button");
    indicator.className = "field-error-indicator";
    indicator.type = "button";
    indicator.textContent = "i";
    indicator.addEventListener("click", (event) => event.preventDefault());

    const tooltip = document.createElement("span");
    tooltip.className = "field-error-tooltip";
    tooltip.id = `field-error-${index + 1}`;
    tooltip.setAttribute("role", "tooltip");

    errorUI.append(indicator, tooltip);
    shell.append(errorUI);
  });
}

function clearFieldError(control) {
  if (!(control instanceof HTMLInputElement || control instanceof HTMLSelectElement)) return;
  if (!control.closest(".control-shell")) return;
  const field = control.closest(".field");
  const errorUI = field?.querySelector(".field-error-ui");
  field?.classList.remove("is-invalid");
  control.removeAttribute("aria-invalid");
  control.removeAttribute("aria-describedby");
  if (errorUI) errorUI.hidden = true;
}

function clearFieldErrors(form) {
  form.querySelectorAll("input, select").forEach(clearFieldError);
}

function markFieldError(control, message) {
  const field = control.closest(".field");
  const errorUI = field?.querySelector(".field-error-ui");
  const indicator = errorUI?.querySelector(".field-error-indicator");
  const tooltip = errorUI?.querySelector(".field-error-tooltip");
  if (!field || !errorUI || !indicator || !tooltip) return;

  field.classList.add("is-invalid");
  control.setAttribute("aria-invalid", "true");
  control.setAttribute("aria-describedby", tooltip.id);
  indicator.setAttribute("aria-label", message);
  tooltip.textContent = message;
  errorUI.hidden = false;
}

function rangeMessage(control) {
  const details = validationFields[control.name] || { label: "This value" };
  const units = details.units ? ` ${details.units}` : "";
  return `${details.label} must be between ${control.min} and ${control.max}${units}.`;
}

function validateRanges(form) {
  const invalid = [];
  form.querySelectorAll('input[type="number"]:not([data-auxiliary])').forEach((control) => {
    if (control.value === "") return;
    const value = Number(control.value);
    const below = control.min !== "" && value < Number(control.min);
    const above = control.max !== "" && value > Number(control.max);
    if (!Number.isFinite(value) || below || above) {
      markFieldError(control, rangeMessage(control));
      invalid.push(control);
    }
  });
  invalid[0]?.focus({ preventScroll: true });
  return invalid.length === 0;
}

function fieldFromApiError(form, message) {
  const names = Object.keys(validationFields).sort((left, right) => right.length - left.length);
  const name = names.find((candidate) => message.includes(candidate));
  return name ? form.querySelector(`[name="${name}"]`) : null;
}

function showPredictionFailure(form, errorElement, failure) {
  const message = failure.message || "The prediction request failed.";
  const control = failure.field
    ? form.querySelector(`[name="${failure.field}"]`)
    : fieldFromApiError(form, message);
  if (!control) {
    showError(errorElement, message);
    return;
  }

  const details = validationFields[control.name];
  const hasRange = control instanceof HTMLInputElement && control.min !== "" && control.max !== "";
  const plainMessage = hasRange
    ? rangeMessage(control)
    : `${details?.label || "This field"} contains an invalid value.`;
  markFieldError(control, plainMessage);
  control.focus({ preventScroll: true });
  showError(errorElement, "");
}

// Form and display helpers —-

function valueOrNull(value) {
  return value === "" ? null : Number(value);
}

function formValues(form) {
  const data = new FormData(form);
  return Object.fromEntries(
    [...data.entries()].map(([key, value]) => [key, valueOrNull(value)])
  );
}

function probabilityLabel(value) {
  const percent = value * 100;
  if (percent < 0.01) return `${percent.toFixed(3)}%`;
  return `${percent.toFixed(2)}%`;
}

function setZone(element, classification) {
  const label = classification[0] + classification.slice(1).toLowerCase();
  element.textContent = label;
  element.setAttribute("aria-label", `Classification: ${label}`);
  element.className = `zone-badge zone-${classification.toLowerCase()}`;
}

function setProbabilityZone(element, classification) {
  element.className = `probability probability-${classification.toLowerCase()}`;
}

function showError(element, message) {
  element.textContent = message;
  element.hidden = !message;
}

function setBusy(button, busy, idleLabel) {
  button.disabled = busy;
  button.textContent = busy ? "Calculating…" : idleLabel;
  button.setAttribute("aria-busy", String(busy));
}

// Weight-for-age helper —-

function showWfazHelperError(message) {
  wfazHelperError.textContent = message;
  wfazHelperError.hidden = !message;
  wfazWeightControl.classList.toggle("is-invalid", Boolean(message));
  wfazWeight.toggleAttribute("aria-invalid", Boolean(message));
  if (message) wfazWeight.setAttribute("aria-describedby", "wfaz-helper-error");
  else wfazWeight.removeAttribute("aria-describedby");
}

function setWfazHelperOpen(open) {
  wfazHelper.hidden = !open;
  wfazHelperToggle.setAttribute("aria-expanded", String(open));
  if (open) {
    showWfazHelperError("");
    wfazWeight.focus({ preventScroll: true });
  }
}

function updateWfazWeightUnit() {
  const pounds = wfazWeightUnit.value === "lb";
  wfazWeight.min = pounds ? "1.1" : "0.5";
  wfazWeight.max = pounds ? "110.2" : "50";
  wfazWeight.placeholder = pounds ? "For example, 27.3" : "For example, 12.4";
  showWfazHelperError("");
}

function clearCalculatedWfaz() {
  if (!state.wfazCalculated) return;
  wfazInput.value = "";
  wfazInput.removeAttribute("data-source");
  state.wfazCalculated = false;
}

async function applyCalculatedWfaz() {
  showWfazHelperError("");
  clearFieldError(ageInput);
  clearFieldError(sexInput);

  const age = valueOrNull(ageInput.value);
  const sex = valueOrNull(sexInput.value);
  const weight = valueOrNull(wfazWeight.value);

  if (age === null) {
    markFieldError(ageInput, "Enter age before calculating the z score.");
    ageInput.focus({ preventScroll: true });
    return;
  }
  if (age < 1 || age > 59) {
    markFieldError(ageInput, "Weight-based calculation is available for ages 1 to 59 months.");
    ageInput.focus({ preventScroll: true });
    return;
  }
  if (sex === null) {
    markFieldError(sexInput, "Select sex before calculating the z score.");
    sexInput.focus({ preventScroll: true });
    return;
  }
  if (weight === null) {
    showWfazHelperError("Enter weight before calculating the z score.");
    wfazWeight.focus({ preventScroll: true });
    return;
  }

  const minimum = Number(wfazWeight.min);
  const maximum = Number(wfazWeight.max);
  if (!Number.isFinite(weight) || weight < minimum || weight > maximum) {
    const units = wfazWeightUnit.value === "lb" ? "lb" : "kg";
    showWfazHelperError(`Weight must be between ${minimum} and ${maximum} ${units}.`);
    wfazWeight.focus({ preventScroll: true });
    return;
  }

  setBusy(wfazHelperApply, true, "Use calculated z score");
  try {
    const result = await predictionClient.calculateWeightForAgeZScore({
      "age.months": age,
      sex,
      weight,
      unit: wfazWeightUnit.value
    });
    wfazInput.value = Number(result.wfaz).toFixed(2);
    wfazInput.setAttribute("data-source", "weight");
    state.wfazCalculated = true;
    clearFieldError(wfazInput);
    invalidateStage1();
    setWfazHelperOpen(false);
    wfazInput.focus({ preventScroll: true });
  } catch (failure) {
    showWfazHelperError(failure.message || "The z score could not be calculated.");
  } finally {
    setBusy(wfazHelperApply, false, "Use calculated z score");
  }
}

// Stage navigation and reset behavior —-

function setStage(stage) {
  const stage1View = document.querySelector("#stage1-view");
  const stage2View = document.querySelector("#stage2-view");
  state.activeStage = stage;

  stage1View.hidden = stage !== 1;
  stage2View.hidden = stage !== 2;
  stage1View.classList.toggle("is-active", stage === 1);
  stage2View.classList.toggle("is-active", stage === 2);

  stage1Step.classList.toggle("is-active", stage === 1);
  stage2Step.classList.toggle("is-active", stage === 2);
  stage1Step.classList.toggle("is-complete", stage === 2);
  stage1Step.toggleAttribute("aria-current", stage === 1);
  stage2Step.toggleAttribute("aria-current", stage === 2);
  stage1Step.setAttribute("aria-label", stage === 1 ? "Stage 1, current stage" : "Stage 1");
  stage2Step.setAttribute(
    "aria-label",
    stage === 2
      ? "Stage 2, current stage"
      : stage2Step.disabled
        ? "Stage 2, unavailable until an Amber result"
        : "Stage 2"
  );

  if (stage === 1) {
    document.querySelector("#stage1-heading").focus?.({ preventScroll: true });
  } else {
    updateMeasureVisibility();
    document.querySelector("#stage2-strategy").focus({ preventScroll: true });
  }
}

function enableStage2(enabled) {
  stage2Step.disabled = !enabled;
  stage2Step.setAttribute(
    "aria-label",
    enabled ? "Stage 2" : "Stage 2, unavailable until an Amber result"
  );
}

function invalidateStage1() {
  if (state.stage1Probability === null) return;
  state.stage1Probability = null;
  state.stage1Classification = null;
  enableStage2(false);
  stage2Form.reset();
  stage2Result.hidden = true;
  updateMeasureVisibility();
}

function editStage1() {
  stage1Result.hidden = true;
  stage1Form.hidden = false;
  stage1Form.querySelector("input, select").focus({ preventScroll: true });
}

function resetAssessment() {
  state.stage1Probability = null;
  state.stage1Classification = null;
  state.wfazCalculated = false;
  stage1Form.reset();
  stage2Form.reset();
  clearFieldErrors(stage1Form);
  clearFieldErrors(stage2Form);
  stage1Form.hidden = false;
  stage2Form.hidden = false;
  stage1Result.hidden = true;
  stage2Result.hidden = true;
  comparison.hidden = true;
  wfazInput.removeAttribute("data-source");
  setWfazHelperOpen(false);
  enableStage2(false);
  showError(document.querySelector("#stage1-error"), "");
  showError(document.querySelector("#stage2-error"), "");
  updateMeasureVisibility();
  setStage(1);
}

// Stage 2 measure visibility —-

function updateMeasureVisibility() {
  const selected = document.querySelector("#stage2-strategy").value;
  document.querySelectorAll("[data-measure]").forEach((field) => {
    field.hidden = !strategies[selected].measures.includes(field.dataset.measure);
  });
}

function stage2Measures() {
  const values = formValues(stage2Form);
  return {
    spo2: values.spo2,
    strem1: values.strem1,
    crp: values.crp,
    glucose: values.glucose
  };
}

// Stage 1 interactions —-

stage1Form.addEventListener("input", (event) => {
  if (event.target.closest("#wfaz-helper")) return;
  if (event.target === wfazInput) {
    state.wfazCalculated = false;
    wfazInput.removeAttribute("data-source");
  } else if (event.target === ageInput || event.target === sexInput) {
    clearCalculatedWfaz();
  }
  clearFieldError(event.target);
  invalidateStage1();
});

wfazHelperToggle.addEventListener("click", () => {
  setWfazHelperOpen(wfazHelper.hidden);
});
wfazHelperApply.addEventListener("click", applyCalculatedWfaz);
wfazWeight.addEventListener("input", () => showWfazHelperError(""));
wfazWeightUnit.addEventListener("change", updateWfazWeightUnit);

stage1Form.addEventListener("submit", async (event) => {
  event.preventDefault();
  const error = document.querySelector("#stage1-error");
  const submit = stage1Submit;
  showError(error, "");
  clearFieldErrors(stage1Form);
  if (!validateRanges(stage1Form)) return;
  setBusy(submit, true, "Calculate Stage 1");

  try {
    const result = await predictionClient.calculateStage1(formValues(stage1Form));
    state.stage1Probability = result.probability;
    state.stage1Classification = result.classification;

    const stage1Probability = document.querySelector("#stage1-probability");
    stage1Probability.textContent = probabilityLabel(result.probability);
    setProbabilityZone(stage1Probability, result.classification);
    setZone(document.querySelector("#stage1-classification"), result.classification);
    document.querySelector("#stage1-explanation").textContent = result.explanation;
    document.querySelector("#continue-stage2").hidden = !result.stage2_available;

    enableStage2(result.stage2_available);
    stage1Form.hidden = true;
    stage1Result.hidden = false;
  } catch (failure) {
    showPredictionFailure(stage1Form, error, failure);
  } finally {
    setBusy(submit, false, "Calculate Stage 1");
  }
});

document.querySelector("#restart-stage1").addEventListener("click", editStage1);
document.querySelector("#continue-stage2").addEventListener("click", () => setStage(2));
document.querySelector(".brand").addEventListener("click", (event) => {
  event.preventDefault();
  setStage(1);
});
stage1Step.addEventListener("click", resetAssessment);
stage2Step.addEventListener("click", () => {
  if (!stage2Step.disabled) setStage(2);
});

// Stage 2 interactions —-

document.querySelector("#stage2-strategy").addEventListener("change", updateMeasureVisibility);
stage2Form.addEventListener("input", (event) => clearFieldError(event.target));

stage2Form.addEventListener("submit", async (event) => {
  event.preventDefault();
  const error = document.querySelector("#stage2-error");
  const submit = stage2Submit;
  showError(error, "");
  clearFieldErrors(stage2Form);
  if (!validateRanges(stage2Form)) return;
  setBusy(submit, true, "Calculate Stage 2");

  try {
    const strategy = document.querySelector("#stage2-strategy").value;
    const result = await predictionClient.calculateStage2({
      stage1_probability: state.stage1Probability,
      strategy,
      measures: stage2Measures()
    });

    const stage2Probability = document.querySelector("#stage2-probability");
    stage2Probability.textContent = probabilityLabel(result.probability);
    setProbabilityZone(stage2Probability, result.classification);
    setZone(document.querySelector("#stage2-classification"), result.classification);
    document.querySelector("#stage2-strategy-result").textContent = `Strategy: ${strategies[result.strategy].label}`;
    const stage2Explanation = document.querySelector("#stage2-explanation");
    stage2Explanation.textContent = result.explanation;
    stage2Explanation.hidden = !result.explanation;

    const imputation = document.querySelector("#stage2-imputation");
    const imputed = Array.isArray(result.imputed)
      ? result.imputed
      : result.imputed
        ? [result.imputed]
        : [];
    imputation.textContent = imputed.length
      ? `Fixed derivation-cohort median used for: ${imputed.map((name) => measureLabels[name]).join(", ")}.`
      : "";
    imputation.hidden = imputed.length === 0;

    stage2Form.hidden = true;
    stage2Result.hidden = false;
  } catch (failure) {
    showPredictionFailure(stage2Form, error, failure);
  } finally {
    setBusy(submit, false, "Calculate Stage 2");
  }
});

document.querySelector("#recalculate-stage2").addEventListener("click", () => {
  stage2Result.hidden = true;
  stage2Form.hidden = false;
  document.querySelector("#stage2-strategy").focus({ preventScroll: true });
});

document.querySelector("#new-assessment").addEventListener("click", resetAssessment);
document.querySelector("#back-stage1").addEventListener("click", () => setStage(1));

// Compatible-strategy comparison —-

document.querySelector("#compare-button").addEventListener("click", async () => {
  const error = document.querySelector("#stage2-error");
  showError(error, "");
  clearFieldErrors(stage2Form);
  if (!validateRanges(stage2Form)) return;
  compareButton.disabled = true;
  compareButton.textContent = "Comparing…";

  try {
    const result = await predictionClient.calculateCompatibleStage2({
      stage1_probability: state.stage1Probability,
      measures: stage2Measures()
    });
    const grid = document.querySelector("#comparison-grid");
    grid.replaceChildren();

    result.results.forEach((item) => {
      const card = document.createElement("article");
      card.className = "comparison-item";
      const heading = document.createElement("h4");
      heading.textContent = strategies[item.strategy].label;
      const value = document.createElement("p");
      value.textContent = `${probabilityLabel(item.probability)} · ${item.classification[0] + item.classification.slice(1).toLowerCase()}`;
      card.append(heading, value);
      grid.append(card);
    });

    comparison.hidden = false;
    document.querySelector("#comparison-close").focus({ preventScroll: true });
  } catch (failure) {
    showPredictionFailure(stage2Form, error, failure);
  } finally {
    compareButton.disabled = false;
    compareButton.textContent = "Compare Stage 2 Strategies";
  }
});

function closeComparison() {
  comparison.hidden = true;
  document.querySelector("#compare-button").focus({ preventScroll: true });
}

// Dialog and helper dismissal —-

document.querySelector("#comparison-close").addEventListener("click", closeComparison);
comparison.addEventListener("click", (event) => {
  if (event.target === comparison) closeComparison();
});
document.addEventListener("keydown", (event) => {
  if (event.key === "Escape" && !comparison.hidden) closeComparison();
  if (event.key === "Escape" && !wfazHelper.hidden) {
    setWfazHelperOpen(false);
    wfazHelperToggle.focus({ preventScroll: true });
  }
});

document.addEventListener("click", (event) => {
  if (!wfazHelper.hidden && !event.target.closest(".wfaz-field")) {
    setWfazHelperOpen(false);
  }
});

// Initial interface setup —-

openSafetyAcknowledgement();
prepareFieldErrorUI();
updateWfazWeightUnit();
updateMeasureVisibility();

function setPredictionControlsEnabled(enabled) {
  stage1Submit.disabled = !enabled;
  stage2Submit.disabled = !enabled;
  compareButton.disabled = !enabled;
  wfazHelperApply.disabled = !enabled;
}

async function startLocalPredictionEngine() {
  setPredictionControlsEnabled(false);
  try {
    const identity = await predictionClient.ready();
    releaseInfo.textContent = `v${identity.appVersion} · Model ${identity.modelVersion} · ${identity.modelBundleHash.slice(0, 8)}`;
    releaseInfo.title = `Application ${identity.appVersion}; model ${identity.modelVersion}; Model ID ${identity.modelBundleHash}`;
    releaseInfo.hidden = false;
    setPredictionControlsEnabled(true);
  } catch (failure) {
    startupError.textContent = `${failure.message || "The local prediction engine could not start."} No prediction has been calculated. Reload the complete application before trying again.`;
    startupError.hidden = false;
  }
}

async function registerServiceWorker() {
  if (!("serviceWorker" in navigator)) return;
  try {
    await navigator.serviceWorker.register(new URL("service-worker.js", document.baseURI), { scope: "./" });
  } catch {
    // The validated local scorer remains available in the current page; installation is unavailable.
  }
}

startLocalPredictionEngine();
window.addEventListener("load", registerServiceWorker, { once: true });
