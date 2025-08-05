# Artificial Intelligence and Advanced Modeling Techniques for Optimizing Resuscitation Strategies in Hemorrhagic Shock

*PhD Dissertation Repository – Fahim Mobin*

## 🧠 Overview

Hemorrhagic shock continues to pose a major challenge in trauma care—despite advancements like REBOA (Resuscitative Endovascular Balloon Occlusion of the Aorta). Individual variability in hemodynamic response, difficulty in accurately estimating blood loss in real time, and a limited understanding of REBOA's impact across **known levels of hemorrhage** complicate clinical management.

This repository contains the datasets, analysis code, and model development workflows used in my PhD research, which aims to develop data-driven tools to:

- **Enhance real-time assessment** of hemorrhagic shock severity  
- **Improve treatment decision-making** using AI and physiologic modeling  
- **Reduce mortality** by optimizing REBOA and resuscitation strategies  

---

## 🎯 Research Aims

This multifaceted study is structured into **three main aims**, each addressing a key limitation in current trauma management:

### **Aim 1: AI-based Hemorrhage Severity Classification**
> **Goal**: Develop a machine learning model that classifies the severity of hemorrhagic shock using only arterial pressure waveforms.  
> **Question**: *How bad is the hemorrhage?*

---

### **Aim 2: Cardiac Impact of Hemorrhage & REBOA**
> **Goal**: Build a Python-based tool to analyze changes in **left ventricular pressure-volume (P-V) loops** during hemorrhagic shock and REBOA.  
> **Question**: *How does hemorrhage and REBOA affect cardiac performance?*

---

### **Aim 3: REBOA Strategy Optimization**
> **Goal**: Evaluate the effectiveness of **different REBOA occlusion modalities** (none, partial, full) across varying hemorrhage levels, and quantify resuscitation needs.  
> **Question**: *How effective are different REBOA strategies in restoring hemodynamics at different hemorrhage levels?*

---

## 📊 Datasets

This repository uses **two distinct large-animal datasets**, both simulating trauma and REBOA use but differing in instrumentation and granularity:

### 1. **Heavily Instrumented Swine Model (Wake Forest Study)**
- **Subjects**: 54 pigs  
- **Scenario**: Simulated trauma to hemostasis, REBOA intervention, and resuscitation  
- **Measurements**:
  - **Pressure**: Left subclavian artery, distal aorta, left jugular vein  
  - **Flow**: Ascending aorta, brachiocephalic trunk, carotid artery, proximal descending aorta, renal artery, distal aorta  
  - **Cardiac**: Left ventricular pressure-volume loops  

This dataset provides rich vector data for real-time analysis of flow and pressure changes during REBOA intervention.

---

### 2. **Legacy REBOA Model (Simplified Instrumentation)**
- **Differences**:
  - Single grade of hemorrhage  
  - Longer REBOA occlusion time  
  - Caval occlusion used for **load-independent cardiac performance** analysis  
- **Included in**: Aim 2 for additional insights into cardiac function under REBOA

---

## 📂 Repository Structure

