# Results Summary

Cycle: 2017-2018
Analytic sample (unweighted): N = 2,944 adults (18+), non-pregnant, complete-case for model.

Outcome: clinically relevant depressive symptoms (PHQ-9 >= 10).
Exposure: usual sleep duration on workdays/weekdays (hours) and categories (<6, 6-<7, 7-<9, >=9).

Weighted prevalence of PHQ-9 >= 10 by sleep duration category:
- <6: 14.9% (95% CI 11.7%, 18.0%)
- 6-<7: 8.7% (95% CI 4.0%, 13.4%)
- 7-<9: 6.1% (95% CI 4.7%, 7.5%)
- >=9: 12.8% (95% CI 9.8%, 15.7%)

Adjusted model (sleep categories; ref 7-<9):
- relevel(sleep_cat, ref = "7-<9")<6: OR 2.51 (95% CI 1.65, 3.80)
- relevel(sleep_cat, ref = "7-<9")6-<7: OR 1.41 (95% CI 0.78, 2.53)
- relevel(sleep_cat, ref = "7-<9")>=9: OR 1.87 (95% CI 1.37, 2.55)

Figure: figures/sleep_hours_vs_dep10_curve.png
