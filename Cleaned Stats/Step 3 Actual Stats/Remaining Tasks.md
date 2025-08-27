# PhD Thesis Task Tracker

This document outlines the remaining analysis, coding, and paper-writing tasks to complete for my dissertation.

---

## 1. R Code Development & Clinical Question Analyses

### A. Hemodynamics & Cardiac Function

**Dataset focus:** Renal artery flow, carotid artery flow, and left ventricular pressure-volume relationship.

#### T30 (End of Hemorrhage)
- **Question:** How do different hemorrhage levels affect physiological and hemodynamic outcomes?
- **Planned Analysis:** 
  - One-Way ANOVA (or rank-based One-Way ANCOVA)
  - Tukey post hoc

#### T60 (End of Occlusion)
- **Question:** What are the combined effects of hemorrhage level and occlusion type on outcomes?
- **Planned Analysis:** 
  - Two-Way ANOVA (or rank-based Two-Way ANCOVA)
  - Tukey post hoc

#### T60 → T65 (Early Blood Transfusion)
- **Question:** How do hemorrhage, occlusion, and time interact to acutely influence system dynamics?
- **Planned Analysis:** 
  - Linear Mixed Model (or rank-based LME model)
  - Post hoc comparisons

#### T0 → T240 (Full Experimental Timeline)
- **Question:** When and how does the subject return to homeostasis compared to baseline (T0)?  
- **Planned Analysis:** 
  - Linear Mixed Model (or rank-based LME model)
  - Post hoc comparisons
  - Focus on physiological recovery trajectories

---

### B. TEG (Thromboelastography) Data

#### Question A
- **Objective:** Do any factors significantly change over time due to hemorrhage or intervention group differences?  
- **Proposed Analysis:**  
  - Originally: ANOVA  
  - Likely better: **Mixed Linear Model** (since repeated measures ANOVA may not handle missing data well)  
  - Decision: **Confirm with advisor/statistician** whether to use LMM as primary approach.

#### Question B
- **Objective:** Are there significant group differences in factors at the end of the study (T240) vs. beginning (T0)?  
- **Planned Analysis:**  
  - ANOVA + Mixed Linear Model

---

## 2. Paper Writing & Publication Plan

### Paper 3
**Title:** *Capturing the hemodynamics and physiologic response to varying degrees of hemorrhagic shock and aortic occlusion – A first step towards developing a mathematical framework for acute transient cardiovascular modeling*  

- ✅ Introduction completed  
- ✅ Methods completed  
- ⚡ Results partially completed  
  - Waiting on R-based analyses above (esp. ANOVA & LMM results)  

**Next Steps:**  
- Finalize results after coding analyses  
- Write discussion & conclusion  
- Target journal?

---

### Paper 4
**Title (tentative):** *Development of an AI model to classify hemorrhage rates from arterial pressure waveforms*  

- **Data Focus:** Left subclavian pressure & distal aortic pressure  
- **Next Steps:**  
  - Define model architecture (start with Random Forest / CNN / LSTM?)  
  - Preprocess waveform data  
  - Train + evaluate model  

---

### Paper 5
**Title (tentative):** *Machine learning classification of kidney injury using oxidative stress markers*  

- **Marker focus:** 8-Isoprostane  
- **Next Steps:**  
  - Gather datasets  
  - Perform exploratory analysis  
  - Test classification algorithms (e.g., Random Forest, Logistic Regression, Neural Nets)  

---

## Notes & Priorities

- **High Priority:** Finish R scripts for clinical questions → directly tied to Paper 3 + Dissertation core.  
- **Medium Priority:** Drafting Papers 4 and 5 (can continue after graduation if needed).  
- **Decision Points:**  
  - Confirm with advisor whether to use **Mixed Linear Models** instead of repeated measures ANOVA for TEG data.  
  - Decide ranking approach (parametric vs. non-parametric) depending on normality & variance tests.  

---




## Further Notes for me to focus on

- Remake the R code so that it is focused on answering all of the research questions in much simpler and more elegant code
- *Make sure to keep all of the cofactors*
- I need to eventually talk to Magan or Williams to find out about the hetastarch
- Try to run all of the codes from one document...?




