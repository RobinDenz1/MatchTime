# print.MatchTD, defaults

    A MatchTD object
     - method: 1:1 (exact) matching only on time
     - controls: Not replacing controls
     - cases: Using all cases
     - number of obs.: 1000 (original), 802 (matched)
     - target estimand: ATT
     - covariates: Only matched on time

# print.MatchTD, fast_exact method

    A MatchTD object
     - method: 1:1 exact matching
     - controls: Not replacing controls
     - cases: Using all cases
     - number of obs.: 1000 (original), 802 (matched)
     - target estimand: ATT
     - covariates: Only matched on time

# print.MatchTD, matchit based method

    A MatchTD object
     - method: 1:1 nearest neighbor matching (using matchit())
     - controls: Not replacing controls
     - cases: Using all cases
     - number of obs.: 1000 (original), 802 (matched)
     - target estimand: ATT
     - covariates: Only matched on time

# print.MatchTD, replace_over_t

    A MatchTD object
     - method: 1:1 (exact) matching only on time
     - controls: Replacing controls only over time
     - cases: Using all cases
     - number of obs.: 1000 (original), 802 (matched)
     - target estimand: ATT
     - covariates: Only matched on time

# print.MatchTD, replace_at_t

    A MatchTD object
     - method: 1:1 (exact) matching only on time
     - controls: Replacing controls only at each point in time
     - cases: Using all cases
     - number of obs.: 1000 (original), 802 (matched)
     - target estimand: ATT
     - covariates: Only matched on time

# print.MatchTD, replace_over_t & replace_at_t

    A MatchTD object
     - method: 1:1 (exact) matching only on time
     - controls: Replacing controls at each point in time and over time
     - cases: Using all cases
     - number of obs.: 1000 (original), 802 (matched)
     - target estimand: ATT
     - covariates: Only matched on time

# print.MatchTD, replace_cases=FALSE

    A MatchTD object
     - method: 1:1 (exact) matching only on time
     - controls: Not replacing controls
     - cases: Using only cases that were not used as controls
     - number of obs.: 1000 (original), 802 (matched)
     - target estimand: ATT
     - covariates: Only matched on time

# print.MatchTD, one covariate

    A MatchTD object
     - method: 1:1 (exact) matching only on time
     - controls: Not replacing controls
     - cases: Using all cases
     - number of obs.: 1000 (original), 802 (matched)
     - target estimand: ATT
     - covariates: mac

# print.MatchTD, multiple covariates

    A MatchTD object
     - method: 1:1 (exact) matching only on time
     - controls: Not replacing controls
     - cases: Using all cases
     - number of obs.: 1000 (original), 802 (matched)
     - target estimand: ATT
     - covariates: mac, A, B, C

