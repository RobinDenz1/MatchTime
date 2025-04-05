# print.match_time, defaults

    A match_time object
     - method: balanced risk set matching
     - match-method: 1:1 (exact) matching only on time
     - controls: Not replacing controls
     - cases: Using all cases
     - number of obs.: 1000 (original), 458 (matched)
     - target estimand: ATT
     - covariates: mac

# print.match_time, fast_exact method

    A match_time object
     - method: balanced risk set matching
     - match-method: 1:1 exact matching
     - controls: Not replacing controls
     - cases: Using all cases
     - number of obs.: 1000 (original), 458 (matched)
     - target estimand: ATT
     - covariates: mac

# print.match_time, matchit based method

    A match_time object
     - method: balanced risk set matching
     - match-method: 1:1 nearest neighbor matching (using matchit())
     - controls: Not replacing controls
     - cases: Using all cases
     - number of obs.: 1000 (original), 458 (matched)
     - target estimand: ATT
     - covariates: mac

# print.match_time, replace_over_t

    A match_time object
     - method: balanced risk set matching
     - match-method: 1:1 (exact) matching only on time
     - controls: Replacing controls only over time
     - cases: Using all cases
     - number of obs.: 1000 (original), 458 (matched)
     - target estimand: ATT
     - covariates: mac

# print.match_time, replace_at_t

    A match_time object
     - method: balanced risk set matching
     - match-method: 1:1 (exact) matching only on time
     - controls: Replacing controls only at each point in time
     - cases: Using all cases
     - number of obs.: 1000 (original), 458 (matched)
     - target estimand: ATT
     - covariates: mac

# print.match_time, replace_over_t & replace_at_t

    A match_time object
     - method: balanced risk set matching
     - match-method: 1:1 (exact) matching only on time
     - controls: Replacing controls at each point in time and over time
     - cases: Using all cases
     - number of obs.: 1000 (original), 458 (matched)
     - target estimand: ATT
     - covariates: mac

# print.match_time, replace_cases=FALSE

    A match_time object
     - method: balanced risk set matching
     - match-method: 1:1 (exact) matching only on time
     - controls: Not replacing controls
     - cases: Using only cases that were not used as controls
     - number of obs.: 1000 (original), 458 (matched)
     - target estimand: ATT
     - covariates: mac

# print.match_time, one covariate

    A match_time object
     - method: balanced risk set matching
     - match-method: 1:1 (exact) matching only on time
     - controls: Not replacing controls
     - cases: Using all cases
     - number of obs.: 1000 (original), 458 (matched)
     - target estimand: ATT
     - covariates: mac

# print.match_time, multiple covariates

    A match_time object
     - method: balanced risk set matching
     - match-method: 1:1 (exact) matching only on time
     - controls: Not replacing controls
     - cases: Using all cases
     - number of obs.: 1000 (original), 458 (matched)
     - target estimand: ATT
     - covariates: mac, A, B, C

# print.match_time, method='psm'

    A match_time object
     - method: time-dependent propensity score matching
     - match-method: 1:1 (exact) matching only on time
     - controls: Not replacing controls
     - cases: Using all cases
     - number of obs.: 1000 (original), 458 (matched)
     - target estimand: ATT
     - covariates: mac

# print.match_time, method='pgm'

    A match_time object
     - method: time-dependent prognostic score matching
     - match-method: 1:1 (exact) matching only on time
     - controls: Not replacing controls
     - cases: Using all cases
     - number of obs.: 1000 (original), 458 (matched)
     - target estimand: ATT
     - covariates: mac

# print.match_time, method='dsm'

    A match_time object
     - method: time-dependent double score matching
     - match-method: 1:1 (exact) matching only on time
     - controls: Not replacing controls
     - cases: Using all cases
     - number of obs.: 1000 (original), 458 (matched)
     - target estimand: ATT
     - covariates: mac

# print.match_time, method='greedy'

    A match_time object
     - method: always selecting all possible controls
     - match-method: 1:1 (exact) matching only on time
     - controls: Not replacing controls
     - cases: Using all cases
     - number of obs.: 1000 (original), 458 (matched)
     - target estimand: ATT
     - covariates: mac

# checking all matchit() methods

    A match_time object
     - method: balanced risk set matching
     - match-method: 1:1 exact matching (using matchit())
     - controls: Not replacing controls
     - cases: Using all cases
     - number of obs.: 1000 (original), 458 (matched)
     - target estimand: ATT
     - covariates: mac

---

    A match_time object
     - method: balanced risk set matching
     - match-method: 1:1 coarsened exact matching (using matchit())
     - controls: Not replacing controls
     - cases: Using all cases
     - number of obs.: 1000 (original), 458 (matched)
     - target estimand: ATT
     - covariates: mac

---

    A match_time object
     - method: balanced risk set matching
     - match-method: 1:1 optimal pair matching (using matchit())
     - controls: Not replacing controls
     - cases: Using all cases
     - number of obs.: 1000 (original), 458 (matched)
     - target estimand: ATT
     - covariates: mac

---

    A match_time object
     - method: balanced risk set matching
     - match-method: 1:1 optimal full matching (using matchit())
     - controls: Not replacing controls
     - cases: Using all cases
     - number of obs.: 1000 (original), 458 (matched)
     - target estimand: ATT
     - covariates: mac

---

    A match_time object
     - method: balanced risk set matching
     - match-method: 1:1 generalized full matching (using matchit())
     - controls: Not replacing controls
     - cases: Using all cases
     - number of obs.: 1000 (original), 458 (matched)
     - target estimand: ATT
     - covariates: mac

---

    A match_time object
     - method: balanced risk set matching
     - match-method: 1:1 genetic matching (using matchit())
     - controls: Not replacing controls
     - cases: Using all cases
     - number of obs.: 1000 (original), 458 (matched)
     - target estimand: ATT
     - covariates: mac

---

    A match_time object
     - method: balanced risk set matching
     - match-method: 1:1 cardinality matching (using matchit())
     - controls: Not replacing controls
     - cases: Using all cases
     - number of obs.: 1000 (original), 458 (matched)
     - target estimand: ATT
     - covariates: mac

# not matched on any covariates

    A match_time object
     - method: balanced risk set matching
     - match-method: 1:1 (exact) matching only on time
     - controls: Not replacing controls
     - cases: Using all cases
     - number of obs.: 1000 (original), 458 (matched)
     - target estimand: ATT
     - covariates: Only matched on time

