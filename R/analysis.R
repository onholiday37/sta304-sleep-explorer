# Shared data and calculation functions. No names or course-specific IDs enter
# the browser-facing analysis data, plots, or response table.
NUMERIC_MEASURES <- c(
  "Exam-week sleep" = "avg_sleep_exam_week",
  "Typical-week sleep" = "avg_sleep_typical_week",
  "Sleep the night before" = "sleep_night_before",
  "Sleep the night after" = "sleep_night_after",
  "Study time over seven days" = "study_hours_7d"
)
MEASURE_INFO <- list(
  avg_sleep_exam_week = list(label = "Sleep during the seven nights before the exam", unit = "hours per night", short = "exam-week sleep"),
  avg_sleep_typical_week = list(label = "Sleep during a typical week", unit = "hours per night", short = "typical-week sleep"),
  sleep_night_before = list(label = "Sleep the night before the exam", unit = "hours", short = "sleep the night before the exam"),
  sleep_night_after = list(label = "Sleep the night after the exam", unit = "hours", short = "sleep the night after the exam"),
  study_hours_7d = list(label = "Study time during the seven days before the exam", unit = "hours over seven days", short = "seven-day study time")
)
PAIRS <- list(
  typical_exam = list(x = "avg_sleep_typical_week", y = "avg_sleep_exam_week", x_label = "Typical week", y_label = "Week before the exam", unit = "hours per night"),
  before_after = list(x = "sleep_night_before", y = "sleep_night_after", x_label = "Night before the exam", y_label = "Night after the exam", unit = "hours")
)
RECENCY_LABELS <- c("Within the past 7 days", "8-14 days ago", "15-30 days ago", "More than 30 days ago")
RECENCY_DAYS <- c(3.5, 11, 22.5, 45.5)
MEASURE_INFO$other_exams_7d <- list(label = "Other assessments in the same seven days", unit = "coded count (5 = 5 or more)", short = "other-assessment count")
MEASURE_INFO$exam_recency_days <- list(label = "Estimated time since the exam", unit = "days (range midpoints)", short = "estimated exam recency")
NUMERIC_MEASURES <- c(NUMERIC_MEASURES, "Other assessments (top-coded)" = "other_exams_7d", "Exam recency (estimated days)" = "exam_recency_days")
PALETTES <- list(
  teal = c(primary = "#227e75", secondary = "#486fa5", accent = "#b45d22"),
  blue = c(primary = "#0072B2", secondary = "#009E73", accent = "#D55E00"),
  purple = c(primary = "#7555a4", secondary = "#377f78", accent = "#b36c16"),
  mono = c(primary = "#435362", secondary = "#677583", accent = "#111111")
)

read_analysis_data <- function(path = "data/analysis_data.csv") {
  if (!file.exists(path)) stop("Analysis data is missing. Run prepare_data.R first.")
  d <- read.csv(path, stringsAsFactors = FALSE, check.names = FALSE)
  required <- unique(c("response_id", unname(NUMERIC_MEASURES), "assessment_type", "exam_recency", "other_exams_7d"))
  if (!all(required %in% names(d))) stop("The analysis data has an unexpected schema.")
  if (any(grepl("student.?id|email|timestamp|name", names(d), ignore.case = TRUE))) {
    stop("Identifying columns were found. Rebuild the analysis data with prepare_data.R.")
  }
  if (anyDuplicated(d$response_id) || anyNA(d$response_id)) stop("Response labels must be present and unique.")
  for (key in unname(NUMERIC_MEASURES)) {
    if (!is.numeric(d[[key]])) stop(paste("A numeric measure has the wrong type:", key))
    upper <- switch(key, study_hours_7d = 168, other_exams_7d = 5, exam_recency_days = 60, 24)
    if (any(!is.finite(d[[key]]) & !is.na(d[[key]])) || any(d[[key]] < 0 | d[[key]] > upper, na.rm = TRUE)) {
      stop(paste("Out-of-range values require review:", key))
    }
  }
  expected <- RECENCY_DAYS[d$exam_recency]
  if (any(is.na(d$exam_recency_days) != is.na(expected)) ||
      any(abs(d$exam_recency_days - expected) > 1e-8, na.rm = TRUE)) stop("Recency midpoints do not match the documented coding.")
  for (key in c("assessment_type", "exam_recency", "other_exams_7d")) {
    permitted <- switch(key, assessment_type = 0:1, exam_recency = 1:4, other_exams_7d = 0:5)
    if (any(!is.na(d[[key]]) & !d[[key]] %in% permitted)) stop(paste("Unknown category:", key))
  }
  d[, required, drop = FALSE]
}

filter_analysis <- function(d, assessment = "all", recency = "all", other_exams = "all") {
  filters <- list(assessment_type = assessment, exam_recency = recency, other_exams_7d = other_exams)
  keep <- rep(TRUE, nrow(d))
  for (key in names(filters)) {
    value <- filters[[key]]
    if (length(value) == 1L && !is.na(value) && value != "all") {
      keep <- keep & !is.na(d[[key]]) & as.character(d[[key]]) == as.character(value)
    }
  }
  d[keep, , drop = FALSE]
}

measure_summary <- function(d, key) {
  stopifnot(key %in% unname(NUMERIC_MEASURES))
  x <- d[[key]][is.finite(d[[key]])]
  list(n = length(x), missing = nrow(d) - length(x),
       mean = if (length(x)) mean(x) else NA_real_,
       median = if (length(x)) median(x) else NA_real_,
       sd = if (length(x) > 1) sd(x) else NA_real_,
       iqr = if (length(x)) IQR(x, type = 7) else NA_real_,
       min = if (length(x)) min(x) else NA_real_,
       max = if (length(x)) max(x) else NA_real_)
}

pair_data <- function(d, pair = "typical_exam") {
  stopifnot(pair %in% names(PAIRS))
  p <- PAIRS[[pair]]
  good <- is.finite(d[[p$x]]) & is.finite(d[[p$y]])
  data.frame(response_id = d$response_id[good], x = d[[p$x]][good], y = d[[p$y]][good], stringsAsFactors = FALSE)
}

pair_summary <- function(d, pair = "typical_exam") {
  p <- pair_data(d, pair)
  difference <- p$y - p$x
  list(n = nrow(p), missing = nrow(d) - nrow(p),
       x_mean = if (nrow(p)) mean(p$x) else NA_real_,
       y_mean = if (nrow(p)) mean(p$y) else NA_real_,
       difference = if (nrow(p)) mean(difference) else NA_real_,
       lower = sum(difference < -1e-8), same = sum(abs(difference) <= 1e-8), higher = sum(difference > 1e-8))
}

fmt <- function(x, digits = 2) ifelse(is.finite(x), formatC(x, digits = digits, format = "f"), "--")

distribution_text <- function(d, key) {
  s <- measure_summary(d, key)
  if (!s$n) return("No usable values match these filters. Reset the filters or choose another measure.")
  m <- MEASURE_INFO[[key]]
  paste0("Across ", s$n, " responses with recorded values, ", m$short, " averages ", fmt(s$mean), " ", m$unit,
    ". The middle response (median) is ", fmt(s$median), "; reported values range from ", fmt(s$min), " to ", fmt(s$max),
    ". Each bar counts responses in a range of values; the dashed line marks the average.",
    if (s$missing) paste0(" ", s$missing, " subgroup responses have no usable value and are not plotted.") else "")
}

comparison_text <- function(d, pair) {
  s <- pair_summary(d, pair); p <- PAIRS[[pair]]
  if (!s$n) return("No complete pairs match these filters. Reset the filters to compare responses.")
  direction <- if (abs(s$difference) < 0.005) "essentially the same on average" else
    paste0(fmt(abs(s$difference)), " ", p$unit, if (s$difference < 0) " lower on average" else " higher on average")
  paste0("For the same ", s$n, " respondents, ", tolower(p$y_label), " is ", direction, " than ", tolower(p$x_label),
    " (", fmt(s$y_mean), " versus ", fmt(s$x_mean), " ", p$unit, "). ", s$lower, " report less, ", s$same,
    " report the same, and ", s$higher, " report more. Each dot is one response, and some dots overlap. Dots below the dashed line show less sleep in the second period.",
    if (s$missing) paste0(" ", s$missing, " incomplete pairs are excluded.") else "")
}

response_table <- function(d) {
  # Explicit column selection also protects the client if new fields are added later.
  data.frame(
    "Response" = d$response_id,
    "Assessment" = ifelse(is.na(d$assessment_type), "Not provided", ifelse(d$assessment_type == 0, "Test / midterm", "Final examination")),
    "Exam-week sleep (h/night)" = d$avg_sleep_exam_week,
    "Typical-week sleep (h/night)" = d$avg_sleep_typical_week,
    "Night before (h)" = d$sleep_night_before,
    "Night after (h)" = d$sleep_night_after,
    "Study time, 7 days (h)" = d$study_hours_7d,
    "Exam recency" = ifelse(is.na(d$exam_recency), "Not provided", RECENCY_LABELS[d$exam_recency]),
    "Estimated recency (days)" = d$exam_recency_days,
    "Other assessments" = ifelse(is.na(d$other_exams_7d), "Not provided", ifelse(d$other_exams_7d == 5, "5 or more", as.character(d$other_exams_7d))),
    check.names = FALSE, stringsAsFactors = FALSE
  )
}

# Degenerate and small subgroups return descriptive values without invalid tests.
mean_inference <- function(x, seed = 3042027) {
  x <- x[is.finite(x)]; n <- length(x)
  out <- list(n = n, mean = if (n) mean(x) else NA_real_, ci = c(NA_real_, NA_real_),
              t = NA_real_, df = n - 1L, p = NA_real_, bootstrap = c(NA_real_, NA_real_))
  if (n < 2 || sd(x) < 1e-10) return(out)
  test <- t.test(x)
  out$ci <- unname(test$conf.int); out$t <- unname(test$statistic); out$p <- test$p.value
  # Preserve the caller's random state. Each filtered sample is reproducible.
  had_seed <- exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE)
  if (had_seed) old_seed <- get(".Random.seed", envir = .GlobalEnv)
  on.exit(if (had_seed) assign(".Random.seed", old_seed, .GlobalEnv) else
    if (exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE)) rm(".Random.seed", envir = .GlobalEnv))
  set.seed(seed)
  boot <- replicate(10000, mean(sample(x, n, replace = TRUE)))
  out$bootstrap <- unname(quantile(boot, c(.025, .975), type = 7))
  out
}

regression_summary <- function(d) {
  good <- is.finite(d$study_hours_7d) & is.finite(d$avg_sleep_exam_week)
  v <- d[good, , drop = FALSE]
  out <- list(n = nrow(v), data = v, valid = FALSE)
  if (nrow(v) < 3 || sd(v$study_hours_7d) < 1e-10 || sd(v$avg_sleep_exam_week) < 1e-10) return(out)
  fit <- lm(avg_sleep_exam_week ~ study_hours_7d, data = v)
  s <- summary(fit)
  if (!is.finite(s$sigma) || s$sigma < 1e-10) return(out)
  out$valid <- TRUE; out$model <- fit; out$r <- cor(v$study_hours_7d, v$avg_sleep_exam_week)
  out$slope <- unname(coef(fit)[2]); out$r2 <- s$r.squared
  out$p <- s$coefficients[2, 4]; out$ci <- unname(confint(fit)[2, ])
  X <- model.matrix(fit); h <- hatvalues(fit)
  out$hc3_ci <- c(NA_real_, NA_real_); out$hc3_p <- NA_real_
  if (all(1 - h > 1e-8)) {
    bread <- solve(crossprod(X))
    cov <- bread %*% crossprod(X, X * (residuals(fit) / (1 - h))^2) %*% bread
    se <- sqrt(diag(cov))[2]
    out$hc3_ci <- out$slope + c(-1, 1) * qt(.975, df.residual(fit)) * se
    out$hc3_p <- 2 * pt(abs(out$slope / se), df.residual(fit), lower.tail = FALSE)
  }
  out
}

format_p <- function(x) if (!is.finite(x)) "unavailable" else if (x < .001) "< 0.001" else paste0("= ", fmt(x, 3))
ci_text <- function(x, digits = 2) paste0("[", fmt(x[1], digits), ", ", fmt(x[2], digits), "]")

regression_text <- function(s) {
  if (!s$valid) return(paste0("There are ", s$n, " complete responses. A regression needs at least three responses and variation in both measures; a meaningful fit is unavailable for this subgroup."))
  paste0("For ", s$n, " responses, Pearson r = ", fmt(s$r), ", R-squared = ", fmt(s$r2, 3),
    ". The estimated slope is ", fmt(s$slope, 4), " hours of nightly sleep per extra study hour (95% conventional CI ",
    ci_text(s$ci, 4), ", p ", format_p(s$p), "). The HC3 interval allowing unequal error variance is ",
    ci_text(s$hc3_ci, 4), " (p ", format_p(s$hc3_p), "). ",
    if (!is.finite(s$hc3_p)) "HC3 is unavailable for this subgroup. " else if (s$hc3_ci[1] <= 0 && s$hc3_ci[2] >= 0)
      "The HC3 interval includes zero, so the linear association is inconclusive. " else "The HC3 interval excludes zero in this exploratory subgroup. ",
    "Dots show original measurements without jitter; overlapping dots can hide responses. When shown, the shaded band is a conventional 95% interval for the fitted mean, not a prediction interval. This voluntary survey cannot establish causation.")
}

summary_table <- function(d) {
  rows <- lapply(unname(NUMERIC_MEASURES), function(key) {
    s <- measure_summary(d, key)
    data.frame(Measure = MEASURE_INFO[[key]]$short, Unit = MEASURE_INFO[[key]]$unit,
      n = s$n, Mean = s$mean, Median = s$median, SD = s$sd, IQR = s$iqr, Min = s$min, Max = s$max)
  })
  do.call(rbind, rows)
}

frequency_table <- function(d) {
  specs <- list(assessment_type = list(0:1, c("Test / midterm", "Final examination")),
    exam_recency = list(1:4, RECENCY_LABELS), other_exams_7d = list(0:5, c("0", "1", "2", "3", "4", "5 or more")))
  do.call(rbind, lapply(names(specs), function(key) {
    spec <- specs[[key]]; counts <- as.integer(table(factor(d[[key]], levels = spec[[1]])))
    data.frame(Variable = key, Category = spec[[2]], Count = counts,
      Percent = if (nrow(d)) 100 * counts / nrow(d) else NA_real_)
  }))
}
