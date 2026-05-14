# synthetic-eocrc

A modular framework to generate synthetic early-onset colorectal cancer exposure data in R. This work was inspired by two papers.
The first was a large genome-wide association study of EO-CRC patients `(Laskar et al 2024)` and the second the data simulation approach adopted in `(Rein et al 2024)`  

- (Laskar et al 2024) https://doi.org/10.1016/j.annonc.2024.02.008
- (Rien et al 2024) https://arxiv.org/pdf/2410.21531

## Aim

The aim of this project is to develop **short-memory** and **long-memory** exposure models for **early-onset colorectal cancer (EOCRC)** in order to:

- generate synthetic cohort datasets, and
- model and investigate cancer historisis, including effect of intervention and prevention strategies 
- benchmark causal observational data models.

<!--- The framework is designed to be driven by structured configuration (JSON spec + JSON schema) and to support modular extension to additional exposures, observational processes, and tumour/disease modules.-->

The simulator is designed to:

- read a structured JSON simulation specification
- validate the specification against a JSON schema
- resolve time- and subgroup-specific parameter rules
- simulate individuals over repeated visits
- support both short-memory and long-memory exposure models
- keep tumour-specific disease logic modular
- make future extensions more explicit and maintainable

## Objectives

- **To use observational / empirical public health data to mimic early-onset trends in Scotland.**
- **To develop a flexible and modularised framework** to enable investigation into additional exposures, observational data, longitudinal patterns, and data missingness patterns.
- **To explore how best to utilise large language models (LLMs)** to co-create this framework.

## Co-creation note

This work has been **co-created with ELM**, the University of Edinburgh's secure gateway to generative AI based on the **GPT architecture**.

The resulting code and documentation remain a **research prototype** and **still require assessment and validation**, including (but not limited to):

- review of modelling assumptions
- regression and calibration testing
- epidemiological/statistical validation of generated outputs
- scrutiny of spec semantics (schema/config) and rule interpretation


## Project structure

```text
synthetic-eocrc/
├── R/
│   ├── utils_math.R
│   ├── utils_sampling.R
│   ├── config_read.R
│   ├── config_validate.R
│   ├── config_accessors.R
│   ├── dimensions.R
│   ├── interpolation.R
│   ├── rule_resolver.R
│   ├── history_features.R
│   ├── state_init.R
│   ├── state_update.R
│   ├── exposure_alcohol.R
│   ├── exposure_adiposity.R
│   ├── exposure_insulin.R
│   ├── disease_crc.R
│   ├── disease_breast.R
│   ├── disease_liver.R
│   ├── disease_pancreas.R
│   ├── observation_models.R
│   ├── simulate_person.R
│   ├── simulate_cohort.R
│   ├── output_builders.R
│   ├── calibration_tables.R
│   └── ... (other modules as added)
├── inst/
│   ├── schemas/
│   │   └── sim_spec.schema.json
│   └── configs/
│       ├── colorectal_eo.json
│       ├── breast.json
│       ├── liver.json
│       └── pancreas.json
├── scripts/
│   └── run_crc_example.R
├── tests/
│   └── testthat/
│       ├── helper-load-modules.R
│       ├── test-utils-accessors-read-validate-resolve.R
│       ├── test-state-and-exposure-modules.R
│       └── test-simulate-person-and-cohort.R
└── README.md
```
---

## Module overview

### Utility modules

- **`utils_math.R`**
  - small mathematical helpers
  - coercion and validation helpers
  - linear predictor helpers
  - inverse logit and related utilities

- **`utils_sampling.R`**
  - shared sampling utilities
  - categorical draws
  - truncated normal sampling
  - waiting-time sampling

---

### Configuration modules

- **`config_read.R`**
  - reads the JSON simulation specification into R

- **`config_validate.R`**
  - validates the loaded specification against the JSON schema
  - performs any additional R-side checks

- **`config_accessors.R`**
  - central accessors for common spec blocks
  - avoids repeated `spec$...$...` navigation
  - includes helpers such as:
    - `get_adiposity_spec()`
    - `get_adiposity_reference_value()`
    - `get_adiposity_latent_age_ref()`
    - `get_adiposity_target_probability_bounds()`
    - `get_insulin_spec()`
    - `get_disease_spec()`
    - `get_mortality_spec()`

---

### Rule system modules

- **`dimensions.R`**
  - selector and subgroup matching logic
  - age-range handling
  - selector specificity scoring
  - selector-key formatting for diagnostics

- **`interpolation.R`**
  - period evaluability and extrapolation
  - anchor-point interpolation
  - annual percent change evaluation

- **`rule_resolver.R`**
  - main rule resolution engine
  - candidate selection
  - specificity filtering
  - base/add/multiply/APC composition
  - public entry points:
    - `resolve_rule()`
    - `resolve_rule_candidates()`

---

### State and exposure modules

- **`state_init.R`**
  - baseline person initialisation
  - sex, education, SES, ethnicity, geography
  - family history and genetic variables
  - latent traits
  - initial adiposity and insulin state

- **`history_features.R`**
  - recent and cumulative summaries of exposure history
  - rolling-window means and proportions

- **`exposure_alcohol.R`**
  - alcohol-state probabilities
  - short- and long-memory alcohol dynamics
  - alcohol-unit generation

- **`exposure_adiposity.R`**
  - obesity target resolution from rules
  - mapping to visceral adiposity
  - short- and long-memory adiposity updates
  - latent metabolic updates

- **`exposure_insulin.R`**
  - insulin initialisation
  - short- and long-memory insulin updates
  - high-insulin threshold helpers

- **`observation_models.R`**
  - observation and measurement logic for recorded exposure values

- **`state_update.R`**
  - state transition helpers used inside the visit loop

---

### Disease modules

- **`disease_crc.R`**
  - active CRC disease selection
  - CRC time-trend resolution
  - CRC incidence context construction
  - CRC hazard and event-time calculation
  - CRC stage context and stage sampling

- **`disease_breast.R`**
- **`disease_liver.R`**
- **`disease_pancreas.R`**

These non-CRC disease modules are currently placeholders or templates for future
extension.

---

### Simulation modules

- **`simulate_person.R`**
  - person-level visit loop
  - competing-risk event and death logic

- **`simulate_cohort.R`**
  - cohort-level wrappers
  - repeated person simulation
  - sorting and output combination

- **`output_builders.R`**
  - patient-level and longitudinal output assembly helpers

---

## Core workflow

The simulator currently follows this broad sequence:

1. **Read config**
   - `read_sim_spec()`

2. **Validate config**
   - `validate_sim_spec()`

3. **Resolve target values from rules**
   - `resolve_rule()`

4. **Initialise a person**
   - baseline demographics
   - baseline latent traits
   - initial exposures

5. **Run the visit loop**
   - update history summaries
   - update alcohol
   - update adiposity and latent metabolic state
   - update insulin
   - apply observation models
   - evaluate disease and mortality hazards
   - sample events or advance to next visit

6. **Build outputs**
   - patient-level summary
   - long-format visit-level data

---

## Configuration-driven design

The simulator is controlled through a JSON configuration file, for example:

- `inst/configs/colorectal_eo.json`

This specification includes:

- study window settings
- population generation parameters
- latent trait models
- exposure models
- disease models
- mortality models
- rule definitions for time- and subgroup-specific targets

The schema for these files is:

- `inst/schemas/sim_spec.schema.json`

---

## Current CRC example

The colorectal example configuration currently includes:

- early-onset CRC disease model
- alcohol exposure
- adiposity exposure
- insulin exposure
- background mortality
- time-varying rules for:
  - alcohol state probabilities
  - baseline obesity probabilities
  - CRC calendar-time APC trend

The adiposity configuration now also contains:
- `reference_value`
- `latent_age_ref`
- `target_probability_bounds`

so that those quantities are no longer hard-coded in the relevant modules.

---

## Quick start

### 1. Run the example driver

A minimum runnable script is provided in:

- `scripts/run_crc_example.R`

This script uses `here::here()` to locate files inside the project.

Run from R with:

```r
source(here::here("scripts", "run_crc_example.R"))
```

The script will:

- source all simulator modules
- load and validate the CRC example spec
- simulate one person
- simulate one fixed-size cohort
- simulate a larger source cohort until a target number of CRC cases is reached
- print calibration summaries including incidence rate per 100,000 person-years
- write outputs to `outputs/`

---

### 2. Basic interactive use

```r
source(here::here("R", "utils_math.R"))
source(here::here("R", "utils_sampling.R"))
source(here::here("R", "config_read.R"))
source(here::here("R", "config_validate.R"))
source(here::here("R", "config_accessors.R"))
source(here::here("R", "dimensions.R"))
source(here::here("R", "interpolation.R"))
source(here::here("R", "rule_resolver.R"))
source(here::here("R", "history_features.R"))
source(here::here("R", "exposure_alcohol.R"))
source(here::here("R", "exposure_adiposity.R"))
source(here::here("R", "exposure_insulin.R"))
source(here::here("R", "disease_crc.R"))
source(here::here("R", "observation_models.R"))
source(here::here("R", "state_init.R"))
source(here::here("R", "state_update.R"))
source(here::here("R", "output_builders.R"))
source(here::here("R", "simulate_person.R"))
source(here::here("R", "simulate_cohort.R"))

spec <- read_sim_spec(here::here("inst", "configs", "colorectal_eo.json"))
validate_sim_spec(spec)

out <- simulate_cohort_from_spec(
  n = 1000,
  spec = spec,
  disease = "crc",
  memory_model = "long",
  mortality_name = "death_other",
  seed = 123
)

head(out$patient)
head(out$long)
```

---

## Calibration summaries

The example driver includes a helper that computes source-cohort calibration
summaries from the patient-level output.

Current reported measures include:

- number of people simulated
- number of CRC cases observed
- total person-years
- cumulative risk per 100,000 people
- incidence rate per 100,000 person-years
- approximate normal-theory confidence interval for the rate

The person-time rate is calculated as:

```text
rate_per_100k_py = 100000 * n_cases_total / total_py
```

where:

- `n_cases_total` is the number of diagnosed cases in the simulated source
  cohort
- `total_py` is the total follow-up person-time measured as:

```text
sum(censor_cal_time_raw - entry_cal_time_raw)
```

---

## Example public functions

### Configuration and rules
- `read_sim_spec()`
- `validate_sim_spec()`
- `resolve_rule()`
- `resolve_rule_candidates()`

### Disease helpers
- `get_crc_disease_name()`
- `get_crc_disease_spec()`
- `get_crc_event_name()`

### Person and cohort simulation
- `simulate_one_patient_from_spec()`
- `simulate_cohort_from_spec()`

Depending on wrappers retained in the codebase, you may also have:
- `simulate_one_patient_short_from_spec()`
- `simulate_one_patient_long_from_spec()`
- `simulate_cohort_short_from_spec()`
- `simulate_cohort_long_from_spec()`

---

## Testing

Tests are located in:

- `tests/testthat/test-utils-accessors-read-validate-resolve.R`
- `tests/testthat/test-state-and-exposure-modules.R`
- `tests/testthat/test-simulate-person-and-cohort.R`

Example usage:

```r
testthat::test_file("tests/testthat/test-utils-accessors-read-validate-resolve.R")
testthat::test_file("tests/testthat/test-state-and-exposure-modules.R")
testthat::test_file("tests/testthat/test-simulate-person-and-cohort.R")
```

The current reported status from your test runs is:

- utilities / accessors / read / validate / resolve: passing
- state and exposure modules: passing
- simulate person and cohort: passing

---

## Current status

### Implemented
- modular config reader and validator
- central config accessors
- selector and interpolation subsystems
- rule resolution engine
- alcohol, adiposity, and insulin exposure modules
- CRC disease module
- person- and cohort-level simulation
- unit tests for utilities, exposures, and simulation flows

### Future work
- calibration and diagnostic summaries beyond the current driver script
- nested case-control sampling utilities as modules
- redesign time-independent education to being parental education
- redeign adding education attainment as a time-dependent exposure
- future modules which capture interventions to reduce incidence

---

## Design principles

This refactor aims to keep the simulator:

- **specification-driven**
  - important model settings should live in JSON, not in hidden code defaults

- **modular**
  - exposures, diseases, rule logic, and simulation logic should be separated

- **traceable**
  - rule resolution should be inspectable and debuggable

- **extensible**
  - new diseases should fit into an explicit module interface

- **testable**
  - core numerical and simulation logic should have direct unit coverage

---

## Next steps

1. add calibration and diagnostic summary functions as reusable modules
2. refactor nested case-control sampling into modular scripts
3. redesign framwork to include modules for education attainment exposure
4. add intervention modules into framework to model it effect

---

## Notes

This repository is currently organised as a sourced-module project rather than a
fully packaged R package. The intended use pattern is:

- source modules
- read and validate config
- run person or cohort simulation
- inspect or save outputs

If the project later becomes an installable package, the same module structure
should translate naturally into package internals.
