# colorectal_eo.json specification dictionary

This document comments and explains the parameters in:

- `inst/configs/colorectal_eo.json`

The actual JSON config must remain valid JSON and therefore cannot contain
inline comments. This file acts as the human-readable annotation layer for the
EOCRC configuration.

---

## Top-level structure

The config file has these main blocks:

| Block | Purpose |
| :------           | :---------------- |
| `$schema` | Relative path to the JSON schema used to validate the config. |
| `meta` | Metadata describing the model. |
| `study` | Study calendar window and follow-up settings. |
| `simulation` | General simulation controls. |
| `rule_resolution` | How target rules are matched and resolved. |
| `population` | Baseline population generation. |
| `latent_traits` | Baseline latent continuous traits. |
| `exposures` | Exposure models: alcohol, adiposity, insulin. |
| `diseases` | Disease-specific incidence and stage models. |
| `mortality` | Competing background mortality model. |
| `rules` | Time-varying and subgroup-specific target rules. |

---

# 1. `$schema`

```json
"$schema": "../schemas/sim_spec.schema.json"
```

| Parameter	        | Meaning           |
| :------           | :---------------- |
| `$schema`	        | Path to the JSON schema used for validation. |

---

# 2. `meta`

The `meta` block describes the Metadata for the model.

---

```json
"meta": {
  "model_name": "colorectal_early_onset",
  "version": "0.1.0",
  "tumour_type": "crc",
  "description": "..."
}
```

| Parameter	        | Meaning           |
| :------           | :---------------- |
| `model_name`      |	Human-readable identifier for this simulation configuration. |
| `version`	        | Version label for the config. Useful for tracking calibration/spec changes. |
| `tumour_type`	    | Default tumour/disease type. Here "crc" is used to select colorectal cancer. |
| `description`     |	Free-text description of the model purpose and scope. |

---


# 3. study

The `study` block sets the calendar window and follow-up time.

---

```json
"study": {
  "calendar_start": 1995,
  "calendar_end": 2025,
  "max_age": 50,
  "sample_entry_cal_time": true,
  "min_followup_time": 0.5
}
```

| Parameter	        | Meaning           |
| :------           | :---------------- |
| `calendar_start`	| Earliest calendar time for simulated study entry. |
| `calendar_end`	| Administrative end of follow-up. |
| `max_age`	| Maximum age at which follow-up stops. |
| `sample_entry_cal_time`	| If true, each individual’s entry calendar time is sampled uniformly within the study window subject to minimum follow-up. If false, everyone enters at calendar_start. |
| `min_followup_time`	| Minimum available follow-up time required when sampling entry calendar time. |

---

# 4. simulation

The `simulation` block describes the general simulation controls

---

```json
"simulation": {
  "default_memory_model": "long",
  "supported_memory_models": ["short", "long"]
}
```

| Parameter	        | Meaning           |
| :------           | :---------------- |
| `default_memory_model`	| Default exposure memory strategy if not otherwise supplied. |
| `supported_memory_models` |	Memory strategies supported by this config. "short" uses primarily current/previous exposure. "long" uses recent/cumulative history features. |

---


# 5. rule_resolution

The `rule_resultion` block describes how target rules are matched and resolved

---

```json
"rule_resolution": {
  "match_strategy": "most_specific",
  "tie_breaker": "priority",
  "default_extrapolation": "clamp"
}
```

| Parameter	        | Meaning           |
| :------           | :---------------- |
| `match_strategy` |	Determines how matching rules are selected. "most_specific" keeps the highest-specificity matching selector set.|
| `tie_breaker` |	How to break ties between otherwise comparable base rules. "priority" uses rule priority.|
| `default_extrapolation` |	Default period extrapolation behaviour when a rule does not specify one. "clamp" evaluates outside a period at the nearest boundary.|

---


# 6. population

The `population` block defines baseline demographic and inherited variables.

---

## 6.1 entry_age

```json
"entry_age": {
  "distribution": "truncated_exponential",
  "min": 20,
  "max": 49,
  "rate": 0.12
}
```

| Parameter	        | Meaning           |
| :------           | :---------------- |
|`distribution` |	Distribution used to sample baseline entry age. Currently "truncated_exponential".|
|`min`	| Minimum possible entry age.|
|`max`	| Maximum possible entry age.|
|`rate`	| Exponential rate parameter controlling how entry ages are distributed between min and max. In the simulator’s current implementation, this creates an increasing density over the interval.|


---


## 6.2 sex_prob


```json
"sex_probs": {
  "Female": 0.5,
  "Male": 0.5
}
```

| Parameter	        | Meaning           |
| :------           | :---------------- |
|`Female` |	Probability of assigning sex "Female".|
|`Male`	| Probability of assigning sex "Male".|


---


## 6.3 education_prob

```json
"education_probs": {
  "Low": 0.3,
  "Medium": 0.45,
  "High": 0.25
}
```

| Parameter	        | Meaning           |
| :------           | :---------------- |
|`Low`	| Probability of low education category.|
|`Medium`	| Probability of medium education category.|
|`High`	| Probability of high education category.|


Education is currently generated as a baseline time-invariant attribute. 
It may later be reinterpreted or refactored as parental education, educational attainment, or a time-dependent variable.

---


## 6.4 ses_given_education

```json
"ses_given_education": {
  "Low": {
    "Low": 0.55,
    "Medium": 0.3,
    "High": 0.15
  },
  ...
}
```

This block defines conditional SES probabilities given education level.

| Education 	  | SES     | Meaning |
| :------   | :------  | :------ |
|`Low`	| Low, Medium, High	| SES  distribution among people with low education.|
|`Medium`|	Low, Medium, High |	SES  distribution among people with medium education.|
|`High`	| Low, Medium, High	| SES  distribution among people with high education.|


---


## 6.5 ethnicity

```json
"ethnicity": {
  "enabled": true,
  "classification_system": "placeholder",
  "levels": ["Unspecified"],
  "assignment": {
    "method": "fixed",
    "value": "Unspecified"
  }
}
```

| Parameter	        | Meaning           |
| :------           | :---------------- |
|`enabled` |	Whether ethnicity is included in generated outputs and contexts.|
|`classification_system` |	Label for the classification system. Currently placeholder.|
|`levels`	| Allowed ethnicity levels.|
|`assignment.method` |	How ethnicity is assigned. "fixed" assigns the same value to all individuals.|
|`assignment.value`	| Fixed ethnicity value assigned when method is "fixed".|

---


## 6.6 geography

```json
"geography": {
  "enabled": true,
  "level": "region",
  "classification_system": "placeholder",
  "levels": ["Unspecified"],
  "assignment": {
    "method": "fixed",
    "value": "Unspecified"
  }
}
```

| Parameter	        | Meaning           |
| :------           | :---------------- |
|`enabled`|	Whether geography is included in generated outputs and contexts.|
|`level` |	Geographic granularity, e.g. region.|
|`classification_system` |	Label for the geographic classification system. Currently placeholder.|
|`levels`	| Allowed geography levels.|
|`assignment.method` |	How geography is assigned.|
|`assignment.value` |	Fixed geography value assigned when method is "fixed".|


---

## 6.7 family_history_models

Family-history variables are sampled using Bernoulli logit models.

---

### 6.7.1 fh_crc

```json
"fh_crc": {
  "model": "bernoulli_logit",
  "intercept": -2.08,
  "coefficients": {
    "sex": {
      "Male": 0.2
    }
  }
}
```

| Parameter	        | Meaning           |
| :------           | :---------------- |
|`model`	|Specifies Bernoulli outcome with logit link.|
|`intercept` |	Baseline log-odds of family history of CRC.|
|`coefficients.sex.Male`|	Log-odds increment if sex is Male. Female is reference because no Female coefficient is supplied.|

---


### 6.7.2 fh_diabetes

```json
"fh_diabetes": {
  "model": "bernoulli_logit",
  "intercept": -1.42,
  "coefficients": {
    "ses": {
      "Low": 0.15
    }
  }
}
```

| Parameter	        | Meaning           |
| :------           | :---------------- |
|`intercept`|	Baseline log-odds of family history of diabetes.|
|`coefficients.ses.Low`	|Log-odds increment for low SES. Medium/High default to zero increment unless specified.|


---


## 6.8 genetic_models

### 6.8.1 genetic_crc_predisposition

```json
"genetic_crc_predisposition": {
  "model": "bernoulli_logit",
  "intercept": -5.35,
  "coefficients": {
    "fh_crc": 0.75
  }
}
```

| Parameter	        | Meaning           |
| :------           | :---------------- |
|`model` |	Bernoulli logit model.|
|`intercept` |	Baseline log-odds of rare inherited CRC predisposition.|
|`coefficients.fh_crc` |	Log-odds increment for family history of CRC.|


---


# 7. latent_traits


The `latent_traits` block modelings latent traits for each individual-level, sampling these from normal linear models.

---

## 7.1 latent_metabolic

```json
"latent_metabolic": {
  "model": "normal_linear",
  "intercept": 0.0,
  "coefficients": {
    "sex": {
      "Male": 0.35
    },
    "fh_diabetes": 0.5,
    "ses": {
      "Low": 0.25
    },
    "education": {
      "High": -0.2
    }
  },
  "sd": 0.6
}
```

| Parameter	        | Meaning           |
| :------           | :---------------- |
|`model`	|Normal linear model.|
|`intercept`	|Baseline mean latent metabolic score.|
|`coefficients.sex.Male`	|Mean increment for males.|
|`coefficients.fh_diabetes`	|Mean increment if family history of diabetes is present.|
|`coefficients.ses.Low`	|Mean increment for low SES.|
|`coefficients.education.High`	|Mean decrement for high education.|
|`sd`	|Standard deviation of residual normal noise.|


---


## 7.2 latent_crc


```json
"latent_crc": {
  "model": "normal_linear",
  "intercept": 0.0,
  "coefficients": {
    "fh_crc": 0.7,
    "fh_diabetes": 0.15,
    "genetic_crc_predisposition": 1.0
  },
  "sd": 0.5
}
```


| Parameter	        | Meaning           |
| :------           | :---------------- |
|`intercept` |	Baseline mean latent CRC susceptibility score.|
|`coefficients.fh_crc` |	Mean increment for family history of CRC.|
|`coefficients.fh_diabetes`	|Mean increment for family history of diabetes.|
|`coefficients.genetic_crc_predisposition`|	Mean increment for rare inherited CRC predisposition.|
|`sd`	|Standard deviation of residual normal noise|


---


# 8. exposures


The `exposures` block defines alcohol, adiposity, and insulin processes.

---


## 8.1 Alcohol exposure

```json
"alcohol": {
  "enabled": true,
  "kind": "categorical",
  ...
}
```


| Parameter	        | Meaning           |
| :------           | :---------------- |
|`enabled`	|Whether alcohol exposure is active.|
|`kind`	|Exposure type. "categorical" means state-based exposure.|
|`states`	|Allowed alcohol states.|
|`score_map`	|Numeric score assigned to each alcohol state for use in models|


---

Alcohol states

```json
"states": [
  "non_drinker",
  "moderate_drinker",
  "hazardous_drinker"
]
```

| State	        | Meaning           |
| :------           | :---------------- |
|`non_drinker`|	No alcohol consumption state.|
|`moderate_drinker`	|Moderate alcohol consumption state.|
|`hazardous_drinker`|	Hazardous alcohol consumption state.|

---

Alcohol score_map

```json
"score_map": {
  "non_drinker": 0,
  "moderate_drinker": 1,
  "hazardous_drinker": 2
}
```

| Parameter	        | Meaning           |
| :------           | :---------------- |
|`non_drinker`	|Numeric score for non-drinker.|
|`moderate_drinker`	|Numeric score for moderate drinker.|
|`hazardous_drinker`	|Numeric score for hazardous drinker.|


---

Alcohol short-memory model


```json
"short": {
  "persistence_logits": {
    "non_drinker": 0.35,
    "moderate_drinker": 0.25,
    "hazardous_drinker": 0.45
  },
  "latent_metabolic_hazardous_coef": 0.08
}
```

| Parameter	        | Meaning           |
| :------           | :---------------- |
|`persistence_logits`	|State-specific logit modifiers encouraging persistence in the previous alcohol score/state.|
|`persistence_logits.non_drinker`	|Persistence modifier for non-drinker state.|
|`persistence_logits.moderate_drinker`|	Persistence modifier for moderate-drinker state.|
|`persistence_logits.hazardous_drinker`	|Persistence modifier for hazardous-drinker state.|
|`latent_metabolic_hazardous_coef`	|Modifier increasing hazardous-drinker log score as latent metabolic score increases.|

---

Alcohol long-memory model

```json
"long": {
  "non_if_recent_mean_lt_0_25": 0.25,
  "moderate_if_recent_mean_between_0_75_and_1_5": 0.15,
  "hazardous_recent_mean_coef": 0.35,
  "hazardous_recent_prop_coef": 0.7,
  "latent_metabolic_hazardous_coef": 0.08
}
```

| Parameter	        | Meaning           |
| :------           | :---------------- |
|`non_if_recent_mean_lt_0_25`	|Log-score increment for non-drinker if recent mean alcohol score is below 0.25.|
|`moderate_if_recent_mean_between_0_75_and_1_5`	|Log-score increment for moderate drinker if recent mean score lies in [0.75, 1.5).|
|`hazardous_recent_mean_coef`	|Effect of recent mean alcohol score on hazardous-drinker log score.|
|`hazardous_recent_prop_coef`	|Effect of recent proportion of hazardous drinking on hazardous-drinker log score.|
|`latent_metabolic_hazardous_coef`	|Effect of latent metabolic score on hazardous-drinker log score.|

---

Alcohol units model


non_drinker


```json
"non_drinker": {
  "distribution": "point_mass",
  "value": 0
}
```

| Parameter	        | Meaning           |
| :------           | :---------------- |
|`distribution`	|"point_mass" always returns the same value.|
|`value`	|Weekly alcohol units assigned to non-drinkers.|


moderate_drinker

```json
"moderate_drinker": {
  "distribution": "truncated_normal",
  "mean": 8.0,
  "sd": 3.5,
  "min": 1.0,
  "max": 14.0,
  "long_recent_mean_coef": 1.0
}
```


| Parameter	        | Meaning           |
| :------           | :---------------- |
|`distribution`	|Truncated normal distribution for alcohol units.|
|`mean`	|Mean weekly units before long-memory adjustment.|
|`sd`	|Standard deviation of weekly units.|
|`min`|	Lower truncation bound.|
|`max`|	Upper truncation bound.|
|`long_recent_mean_coef`|	In long-memory mode, mean units increase with recent mean alcohol score.|


hazardous_drinker


```json
"hazardous_drinker": {
  "distribution": "truncated_normal",
  "mean": 24.0,
  "sd": 7.0,
  "min": 15.0,
  "max": 60.0,
  "long_recent_mean_coef": 2.0,
  "long_hazard_recent_prop_coef": 3.0
}
```

| Parameter	        | Meaning           |
| :------           | :---------------- |
|`mean`	|Mean weekly units before long-memory adjustment.|
|`sd`	|Standard deviation of weekly units.|
|`min`|	Lower truncation bound.|
|`max`|	Upper truncation bound.|
|`long_recent_mean_coef`|	Long-memory adjustment from recent mean alcohol score.|
|`long_hazard_recent_prop_coef`	|Long-memory adjustment from recent hazardous-drinker proportion.|

## 8.2 Adiposity exposure

```json
"adiposity": {
  "enabled": true,
  "kind": "continuous",
  "variable": "visceral_adipose",
  ...
}
```

| Parameter	        | Meaning           |
| :------           | :---------------- |
|`enabled`|	Whether adiposity exposure is active.|
|`kind`	|Exposure type. "continuous" means numeric continuous state.|
|`variable`	|Output variable name for observed visceral adiposity.|
|`reference_value`|	Centring value for visceral adiposity effects in downstream models.|
|`latent_age_ref`|	Age reference used in latent metabolic updates.|
|`obesity_threshold`|	Visceral adiposity threshold used to define obesity indicator.|
|`visceral_sd`	|Standard deviation used when sampling true visceral adiposity.|


---


Adiposity target_probability_bounds

```json
"target_probability_bounds": {
  "min": 0.01,
  "max": 0.95
}
```

| Parameter	        | Meaning           |
| :------           | :---------------- |
|`min`	|Lower bound applied to resolved obesity probability targets.|
|`max`	|Upper bound applied to resolved obesity probability targets.|

---

Adiposity target_mapping


```json
"target_mapping": {
  "method": "normal_threshold",
  "threshold": 30.0,
  "sd": 4.0
}
```

| Parameter	        | Meaning           |
| :------           | :---------------- |
|`method`	|Method used to map obesity target probability to mean visceral adiposity.|
|`threshold`	|Obesity threshold used in the normal-threshold mapping.|
|`sd`	|Standard deviation assumed in the mapping.|

---

Adiposity age_effect

```json
"age_effect": {
  "age_ref": 35.0,
  "logit_slope_per_10y": 0.22
}
```

| Parameter	        | Meaning           |
| :------           | :---------------- |
|`age_ref`	|Reference age for age adjustment to obesity target.|
|`logit_slope_per_10y`	|Logit-scale change in obesity probability per 10-year age difference.|

---

Adiposity baseline_distribution


```json
"baseline_distribution": {
  "distribution": "truncated_normal",
  "min": 8.0,
  "max": 70.0
}
```

| Parameter	        | Meaning           |
| :------           | :---------------- |
|`distribution`	|Distribution family used for sampled visceral adiposity bounds.|
|`min`	|Lower truncation bound for true visceral adiposity.|
|`max`	|Upper truncation bound for true visceral adiposity.|

---

Adiposity initial_state

```json
"initial_state": {
  "latent_metabolic_coef": 1.5
}
```

| Parameter	        | Meaning           |
| :------           | :---------------- |
|`latent_metabolic_coef`|	Effect of baseline latent metabolic state on initial visceral adiposity mean.|


---

Adiposity short-memory dynamics

```json
"short": {
  "latent_ar": 0.8,
  "latent_age_coef": 0.03,
  "latent_alcohol_score_coef": 0.08,
  "latent_fh_diabetes_coef": 0.18,
  "latent_noise_sd": 0.22,
  "mean_prev_visceral_coef": 0.6,
  "mean_alcohol_score_coef": 0.8,
  "mean_latent_metabolic_coef": 2.0
}
```

| Parameter	        | Meaning           |
| :------           | :---------------- |
|`latent_ar`	|Autoregressive persistence of latent metabolic state.|
|`latent_age_coef`	|Effect of age, centred at latent_age_ref, on latent metabolic state.|
|`latent_alcohol_score_coef`	|Effect of current alcohol score on latent metabolic state.|
|`latent_fh_diabetes_coef`	|Effect of family history of diabetes on latent metabolic state.|
|`latent_noise_sd`	|Standard deviation of Gaussian innovation noise in latent metabolic state.|
|`mean_prev_visceral_coef`	|Persistence term pulling current visceral adiposity towards previous value relative to target.|
|`mean_alcohol_score_coef`	|Effect of current alcohol score on current visceral adiposity mean.|
|`mean_latent_metabolic_coef`	|Effect of updated latent metabolic state on visceral adiposity mean.|


---

Adiposity long-memory dynamics


```json
"long": {
  "latent_ar": 0.8,
  "latent_age_coef": 0.03,
  "latent_recent_mean_score_coef": 0.1,
  "latent_cum_mean_score_coef": 0.05,
  "latent_fh_diabetes_coef": 0.18,
  "latent_noise_sd": 0.22,
  "mean_prev_visceral_coef": 0.5,
  "mean_recent_visceral_coef": 0.2,
  "mean_alcohol_score_coef": 0.6,
  "mean_recent_mean_score_coef": 0.4,
  "mean_latent_metabolic_coef": 1.8
}
```

| Parameter	        | Meaning           |
| :------           | :---------------- |
|`latent_ar`	|Autoregressive persistence of latent metabolic state.|
|`latent_age_coef`|	Effect of age on latent metabolic state.|
|`latent_recent_mean_score_coef`	|Effect of recent mean alcohol score on latent metabolic state.|
|`latent_cum_mean_score_coef`|	Effect of cumulative mean alcohol score on latent metabolic state.|
|`latent_fh_diabetes_coef`	|Effect of family history of diabetes on latent metabolic state.|
|`latent_noise_sd`|Standard deviation of Gaussian innovation noise.|
|`mean_prev_visceral_coef`	|Current visceral mean dependence on previous visceral value.|
|`mean_recent_visceral_coef`	|Current visceral mean dependence on recent visceral history.|
|`mean_alcohol_score_coef`|	Current alcohol score effect on visceral mean.|
|`mean_recent_mean_score_coef`|	Recent alcohol history effect on visceral mean.|
|`mean_latent_metabolic_coef`	|Latent metabolic effect on visceral mean.|

---

Adiposity observation_model

```json
"observation_model": {
  "model": "bernoulli_logit",
  "intercept": 1.0,
  "coefficients": {
    "ses": {
      "Low": -0.4
    }
  }
}
```

| Parameter	        | Meaning           |
| :------           | :---------------- |
|`model`	|Bernoulli logit observation model for whether adiposity is measured/recorded.|
|`intercept`	|Baseline log-odds of observing adiposity measurement.|
|`coefficients.ses.Low`|	Log-odds decrement for low SES.|

---

# 8.3 Insulin exposure

```json
"insulin": {
  "enabled": true,
  "kind": "continuous",
  "variable": "fasting_insulin",
  "log_scale": true,
  "high_log_threshold": 2.7080502011
}
```

| Parameter	        | Meaning           |
| :------           | :---------------- |
|`enabled`	|Whether insulin exposure is active.|
|`kind`	|Exposure type.|
|`variable`	|Output variable name for observed insulin.|
|`log_scale`	|Whether insulin dynamics are modelled on the log scale.|
|`high_log_threshold`	|Log-insulin threshold defining high insulin history indicator.|

Note: 2.7080502011 is approximately log(15).

---

Insulin initial_state

```json
"initial_state": {
  "base_mean_log": 1.9459101491,
  "visceral_coef": 0.03,
  "latent_metabolic_coef": 0.1,
  "sd": 0.25,
  "min_log": 0.6931471806,
  "max_log": 4.3820266347
}
```

| Parameter	        | Meaning           |
| :------           | :---------------- |
|`base_mean_log`	|Baseline mean log insulin. Approximately log(7).|
|`visceral_coef`	|Effect of centred visceral adiposity on baseline log insulin.|
|`latent_metabolic_coef`	|Effect of latent metabolic score on baseline log insulin.|
|`sd`	|Standard deviation for truncated-normal log insulin sampling.|
|`min_log`	|Lower truncation bound. Approximately log(2).|
|`max_log`	|Upper truncation bound. Approximately log(80).|

---

Insulin short-memory dynamics

```json
"short": {
  "base_mean_log": 1.9459101491,
  "prev_log_coef": 0.65,
  "visceral_coef": 0.035,
  "alcohol_score_coef": 0.04,
  "latent_metabolic_coef": 0.12,
  "fh_diabetes_coef": 0.06,
  "sd": 0.22,
  "min_log": 0.6931471806,
  "max_log": 4.3820266347
}
```

| Parameter	        | Meaning           |
| :------           | :---------------- |
|`base_mean_log`	|Reference log insulin mean.|
|`prev_log_coef`	|Autoregressive persistence of previous log insulin.|
|`visceral_coef`	|Effect of current centred visceral adiposity.|
|`alcohol_score_coef`	|Effect of current alcohol score.|
|`latent_metabolic_coef`	|Effect of latent metabolic state.|
|`fh_diabetes_coef`|	Effect of family history of diabetes.|
|`sd`	|Standard deviation of sampling noise.|
|`min_log` |	Lower truncation bound.|
|`max_log`	|Upper truncation bound.|

---

Insulin long-memory dynamics

```json
"long": {
  "base_mean_log": 1.9459101491,
  "prev_log_coef": 0.55,
  "visceral_coef": 0.03,
  "recent_visceral_coef": 0.015,
  "cum_visceral_coef": 0.01,
  "alcohol_score_coef": 0.03,
  "recent_mean_score_coef": 0.02,
  "latent_metabolic_coef": 0.1,
  "fh_diabetes_coef": 0.06,
  "sd": 0.22,
  "min_log": 0.6931471806,
  "max_log": 4.3820266347
}
```

| Parameter	        | Meaning           |
| :------           | :---------------- |
|`prev_log_coef`	|Autoregressive persistence of previous log insulin.|
|`visceral_coef`	|Effect of current centred visceral adiposity.|
|`recent_visceral_coef`	|Effect of recent visceral adiposity history.|
|`cum_visceral_coef`	|Effect of cumulative visceral adiposity history.|
|`alcohol_score_coef`|	Effect of current alcohol score.|
|`recent_mean_score_coef`	|Effect of recent mean alcohol score.|
|`latent_metabolic_coef`	|Effect of latent metabolic state.|
|`fh_diabetes_coef`	|Effect of family history of diabetes.|
|`sd`	|Standard deviation of sampling noise.|
|`min_log`	|Lower truncation bound.|
|`max_log`	|Upper truncation bound.|


---

Insulin observation_model

```json
"observation_model": {
  "model": "bernoulli_logit",
  "intercept": 0.8,
  "coefficients": {
    "fh_diabetes": 0.2,
    "ses": {
      "Low": -0.25
    }
  }
}
```

| Parameter	        | Meaning           |
| :------           | :---------------- |
|`model`	|Bernoulli logit observation model for whether insulin is measured.|
|`intercept`|	Baseline log-odds of insulin observation.|
|`coefficients.fh_diabetes`|	Log-odds increment if family history of diabetes is present.|
|`coefficients.ses.Low`	|Log-odds decrement for low SES.|

---

# 9. diseases

The `diseases` block controls disease-specific incidence and stage in models

---

## 9.1 CRC disease block

```json
"crc": {
  "enabled": true,
  "label": "Early-onset colorectal cancer",
  "event_name": "eo_crc"
}
```

| Parameter	        | Meaning           |
| :------           | :---------------- |
|`enabled`	|Whether this disease block is active.|
|`label`	|Human-readable disease label.|
|`event_name`	|Patient-table event indicator column name.|

---

CRC incidence_model

```json
"incidence_model": {
  "type": "piecewise_exponential_log_linear",
  "intercept": -13.66,
  "age_ref": 20.0,
  "coefficients": { ... },
  "time_trend_target": "disease.crc.apc"
}
```

| Parameter	        | Meaning           |
| :------           | :---------------- |
|`type`	|Incidence model type. Current simulator treats this as a log-linear hazard model.|
|`intercept`	|Baseline log hazard before covariates and time trend.|
|`age_ref`	|Reference age for the age_per_year covariate.|
|`coefficients`	|Log-hazard coefficients for incidence covariates.|
|`time_trend_target`	|Rule target used to resolve calendar-time trend contribution.|

---

CRC incidence coefficients


| Coefficient	        | Meaning           |
| :------           | :---------------- |
|`age_per_year`	|Log-hazard increase per year of age above age_ref.|
|`fh_crc`	|Log-hazard effect of family history of CRC.|
|`fh_diabetes`	|Log-hazard effect of family history of diabetes.|
|`genetic_crc_predisposition`	|Log-hazard effect of rare inherited CRC predisposition.|
|`sex.Male`	|Log-hazard increment for males.|
|`ses.Low`	|Log-hazard increment for low SES.|
|`education.High`	|Log-hazard decrement for high education.|
|`alcohol_score_short`	|Alcohol score effect under short-memory model.|
|`alcohol_score_long`	|Current alcohol score effect under long-memory model.|
|`recent_mean_score`	|Recent mean alcohol score effect under long-memory model.|
|`haz_recent_prop`	|Recent hazardous-drinking proportion effect under long-memory model.|
|`visceral_current_per_unit`	|Current centred visceral adiposity effect.|
|`visceral_recent_per_unit`	|Recent centred visceral adiposity effect under long-memory model.|
|`visceral_cum_per_unit`	|Cumulative centred visceral adiposity effect under long-memory model.|
|`log_insulin_current`	|Current log insulin effect.|
|`recent_log_insulin`	|Recent log insulin effect under long-memory model.|
|`high_insulin_recent_prop`	|Recent proportion of high insulin under long-memory model.|
|`latent_crc`	|Latent CRC susceptibility score effect.|

---

CRC stage_model

```json
"stage_model": {
  "type": "two_stage_logit_multinomial",
  "advanced_logit_intercept": -0.3,
  "advanced_logit_coefficients": { ... },
  "early_stage_probs": {
    "I": 0.55,
    "II": 0.45
  },
  "advanced_stage_probs": {
    "III": 0.65,
    "IV": 0.35
  }
}
```

| Parameter	        | Meaning           |
| :------           | :---------------- |
|`type`|	Stage model type. First samples advanced vs early, then samples specific stage.|
|`advanced_logit_intercept`	|Baseline log-odds of advanced stage.|
|`advanced_logit_coefficients`	|Covariate effects on advanced-stage log-odds.|
|`early_stage_probs`	|Conditional probabilities for stages I/II among non-advanced cases.|
|`advanced_stage_probs`	|Conditional probabilities for stages III/IV among advanced cases.|

---

CRC stage coefficients


| Coefficient	        | Meaning           |
| :------           | :---------------- |
|`ses.Low	`|Low SES effect on advanced-stage log-odds.|
|`latent_crc`	|Latent CRC score effect on advanced-stage log-odds.|
|`visceral_current_per_unit`	|Current centred visceral adiposity effect on advanced-stage log-odds.|
|`log_insulin_current`	|Current log insulin effect on advanced-stage log-odds.|
|`cum_visceral_per_unit`	|Cumulative centred visceral effect under long-memory model.|
|`recent_log_insulin`	|Recent log insulin effect under long-memory model.|
|`interval_gap`	|Visit interval length effect on advanced-stage log-odds.|

---

# 10. mortality

The `mortality` block handles xompeting background mortality in the model

---

```json
"death_other": {
  "type": "piecewise_exponential_log_linear",
  "intercept": -9.5,
  "age_ref": 20.0,
  "coefficients": {
    "age_per_year": 0.07,
    "sex": {
      "Male": 0.25
    },
    "ses": {
      "Low": 0.2
    },
    "fh_diabetes": 0.15,
    "latent_metabolic": 0.1
  }
}
```


| Parameter	        | Meaning           |
| :------           | :---------------- |
|`death_other`|	Name of the competing background mortality model.|
|`type`	|Mortality model type. Current model uses log-linear hazard.|
|`ntercept`	|Baseline log hazard of other-cause death.|
|`age_ref`|Reference age for mortality age effect.|
|`coefficients.age_per_year`	|Log-hazard increase per year above age_ref.|
|`coefficients.sex.Male`	|Male mortality log-hazard increment.|
|`coefficients.ses.Low`	|Low SES mortality log-hazard increment.|
|`coefficients.fh_diabetes`	|Family history of diabetes mortality log-hazard increment.|
|`coefficients.latent_metabolic`	|Latent metabolic effect on mortality.|


---

# 11. rules

The `rules` block defines time-varying and subgroup-specific target values.

---

Each rule has common fields:


| Field	        | Meaning           |
| :------           | :---------------- |
|`target`	|Name of the model target being resolved.|
|`selectors`|Subgroup selectors such as age range, sex, SES, or disease.|
|`period`	|Calendar-time period over which the rule applies.|
|`rule_type` |Type of rule, e.g. anchor points or annual percent change.|
|`scale`	|Scale of rule value: identity, log, or logit.|
|`operation`	|How the rule contributes: set, add, multiply, override.|
|`priority`	|Tie-breaking priority among otherwise comparable rules.|
|`source`	|Label for the evidence/source/calibration origin.|

---

# 11.1 Alcohol state probability rules

Target

```json
"target": "alcohol.state_probs"
```

These rules resolve the baseline alcohol state probability vector by year and subgroup.

| Selector pattern	        | Meaning           |
| :------           | :---------------- |
|`age_range: [18, 50]`	|Applies to people aged 18 to 50.|
|`ses: High`	|SES-specific alcohol probabilities for high SES.|
|`ses: Medium`|SES-specific alcohol probabilities for medium SES.|
|`ses: Low`	|SES-specific alcohol probabilities for low SES.|


Alcohol rule anchors

Each anchor has:

| Field	        | Meaning           |
| :------           | :---------------- |
|`year`	|Calendar year of anchor point.|
|`value.non_drinker`	|Probability of non-drinker state.|
|`value.moderate_drinker`	|Probability of moderate-drinker state.|
|`value.hazardous_drinker`|Probability of hazardous-drinker state.|

Between anchor years, values are linearly interpolated element-wise.

---

# 11.2 Obesity probability rules

Target:

```json
"target": "adiposity.obesity_probability.base"
```


These rules resolve baseline obesity probability before the age effect is applied.

Selectors are sex- and SES-specific:


| Selector pattern	        | Meaning           |
| :------           | :---------------- |
|`sex: Female, ses: High`	|Female high-SES obesity target.|
|`sex: Male, ses: High`	|Male high-SES obesity target.|
|`sex: Female, ses: Medium`	|Female medium-SES obesity target.|
|`sex: Male, ses: Medium`	|Male medium-SES obesity target.|
|`sex: Female, ses: Low`	|Female low-SES obesity target.|
|`sex: Male, ses: Low`	|Male low-SES obesity target.|
|`Obesity` |rule anchors|


| Field	        | Meaning           |
| :------           | :---------------- |
|`year`	|Calendar year of anchor point.|
|`value`	|Baseline obesity probability at that year for the selected subgroup.|


The configured adiposity age effect is applied after resolving this baseline target.

---


# 11.3 CRC APC rules


Target:


```json
"target": "disease.crc.apc"
```

These rules define calendar-time annual percent change in CRC incidence.


| Field	        | Meaning           |
| :------           | :---------------- |
|`selectors.disease`	|Restricts the APC rule to CRC.|
|`period.start`	|Start of APC period.|
|`period.end`	|End of APC period.|
|`rule_type`	|annual_percent_change.|
|`scale`	|log, meaning the APC is returned as additive log-hazard offset.|
|`annual_rate`	|Annual percent change expressed as a decimal. For example, 0.01 means 1% annual increase.|


Current periods:

| Period	        | Annual rate	Interpretation           |
| :------           | :---------------- |
|`1995–2010`	|0.005	Approx. 0.5% annual increase.|
|`2010–2025`	|0.01	Approx. 1.0% annual increase.|

---


# Notes

This document is descriptive and should be kept in sync with:

- `inst/configs/colorectal_eo.json`
- `inst/schemas/sim_spec.schema.json`
- `simulator modules in R/`

The JSON config remains the executable source of parameters; this file is a human-readable annotation and modelling dictionary.



