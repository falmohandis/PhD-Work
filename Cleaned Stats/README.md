# Experimental Questions, Tests, and Timeline

## A. At T30 (End of Hemorrhage)
- **Question:** Are there significant differences in average blood flow between hemorrhage levels?  
- **Test:** One-way ANOVA to test for mean differences, followed by Tukey post hoc for pairwise comparisons.

---

## B. At T60 (End of Occlusion)
- **Question:** Are there significant differences in average blood flow between hemorrhage levels, occlusion groups, and their interaction?  
- **Test:** Two-way ANOVA to examine main effects (hemorrhage, occlusion) and their interaction, followed by Tukey post hoc.

---

## C. Between T60 and T65 (Early Blood Transfusion)
- **Questions:**  
  - How does average flow change over this short interval?  
  - Do hemorrhage and occlusion levels significantly affect this short-term change?  
- **Test:** Linear Mixed Model (LMM) to account for repeated measures, followed by post hoc comparisons to identify group differences.

---

## D. Across All Time Points (T0–T240)
- **Questions:**  
  - How does average flow evolve over time?  
  - What are the overall effects and interactions of time, hemorrhage level, and occlusion group?  
- **Test:** Linear Mixed Model (LMM) across the full timeline with time, hemorrhage, and occlusion as factors. Post hoc comparisons for detailed group contrasts.

---

# ASCII Timeline of Phases

T0             T30              T60        T65                          T240
|--------------|----------------|----------|-----------------------------|
Baseline       End of           End of     Early Transfusion             End of
               Hemorrhage       Occlusion  (Short-term dynamics)         Experiment

   A: ANOVA     B: Two-way ANOVA   C: LMM (short-term)     D: LMM (full timeline)

## Rational behind using the mixed linear model instead of repeated measures ANOVA

- Missing or uneven time points (e.g., some animals drop out before T240).

- Unbalanced data (different sample sizes across groups).

- Complex covariance structures (time effects not independent, non-spherical).

- Continuous covariates (e.g., weight, baseline flow as covariates).

- Flexibility: LMMs allow modeling random slopes, random intercepts, and individual variability more robustly.