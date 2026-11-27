# STA304 sleep before the exam

Group NequalNp's interactive R Shiny survey analysis.

Public website URL: pending deployment and verification.

This repository contains 85 survey responses with direct identifiers removed. Row labels are local response labels, not student IDs.
Only analytical variables are included. Participation was voluntary; the results are exploratory and do not establish causation.

Run locally with `shiny::runApp()` after installing `shiny`, `plotly` and `DT`.
Publish on Posit Connect Cloud with primary file `app.R` and the included dependency manifest.

Exam recency estimates: 0-7 days -> 3.5; 8-14 -> 11; 15-30 -> 22.5; more than 30 -> 45.5 (assumed 31-60-day interval).
The source category is retained. Demographic variables were not collected; the app explicitly discloses the missing demographic-filter requirement.
