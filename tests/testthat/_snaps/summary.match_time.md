# general test case

    Call:
    match_time(formula = vacc ~ mac, data = d_single, id = ".id", 
        inclusion = "inclusion", match_method = "fast_exact")
    
    Summary of Balance for Matched Data at Baseline:
                  Means Treated Means Control Std. Mean Diff. Var. Ratio eCDF Mean
    medsTRUE          0.3886463     0.1441048       0.5016826         NA 0.2445415
    influenzaTRUE     0.0000000     0.0000000       0.0000000         NA 0.0000000
    macTRUE           0.2096070     0.2096070       0.0000000         NA 0.0000000
                   eCDF Max
    medsTRUE      0.2445415
    influenzaTRUE 0.0000000
    macTRUE       0.0000000
    
    Sample Sizes:
              Controls Treated  All
    Matched        229     229  458
    Unmatched      566       0  566
    Included      1000     229 1000
    Supplied      1000     236 1000
    
    Points in Time:
    Matching was performed at 168 unique points in time between 2 and 359.

