---
name: Variable request
about: Suggest a variable for cchsflow
title: ''
labels: ''
assignees: ''

---

**Introduction**

We accept requests and PR for new variables. Providing information about your request helps discussion about whether and how to include the variable.

**Is the variable an existing CCHS variable, or a derived variable?**

Does this variable exist already in CCHS cycles, or is it a newly derived variable?

**What is the name of the variable?**

What is the most consistent name for this variable (usually the variable name used from 2007-2014). If this is an derived variable, what is the name of the newly derived variable?

**Description of variable**

Provide a brief description of the variable.

**Is it consistent across CCHS cycles? If not, explain changes between cycles**

For existing variables in the CCHS.

**Which cycles is this variable found?**

For existing variables in the CCHS.

**Derived variables only. What variables are used to create this variable?**

List the variables in cchsflow used to create this variable

**Additional context**

Add any other context or screenshots about the feature request here.

**Additional instructions for derived variables**

Derived variables use R code for more complex operations and multiple starting variables. Note: cchsflow currently uses only base R to derive variables. More complex derived variables that require dependancies are currently out-of-scope for cchsflow.

If possible, attach an .R file that includes documentation of the derived variable as per roxygen2 standards, along with the code to derive the variable. Include all starting variables.
