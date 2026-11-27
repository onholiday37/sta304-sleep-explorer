finish_plot <- function(p, x_title, y_title, x_range = NULL, y_range = NULL) {
  p <- plotly::layout(p,
    font = list(family = "Arial, sans-serif", color = "#334155", size = 12),
    paper_bgcolor = "#ffffff", plot_bgcolor = "#ffffff", showlegend = FALSE,
    margin = list(l = 65, r = 24, t = 24, b = 78),
    xaxis = list(title = list(text = x_title, standoff = 16), range = x_range, gridcolor = "#edf0f3", zeroline = FALSE),
    yaxis = list(title = list(text = y_title, standoff = 14), range = y_range, gridcolor = "#edf0f3", zeroline = FALSE),
    hoverlabel = list(bgcolor = "#152e3a", font = list(color = "white")), hovermode = "closest")
  plotly::config(p, displaylogo = FALSE, responsive = TRUE,
    modeBarButtonsToRemove = c("select2d", "lasso2d", "sendDataToCloud"))
}

measure_range <- function(key, axis_view) {
  if (!identical(axis_view, "full")) return(NULL)
  c(0, switch(key, study_hours_7d = 168, other_exams_7d = 5.5, exam_recency_days = 60, 24))
}

distribution_plot <- function(d, key, palette = PALETTES$teal, axis_view = "auto", bins = 12) {
  x <- d[[key]][is.finite(d[[key]])]; m <- MEASURE_INFO[[key]]
  if (!length(x)) stop("No usable observations to plot.")
  width <- max(if (key == "study_hours_7d") 1 else if (key == "other_exams_7d") 1 else .5,
    diff(range(x)) / max(1, bins))
  p <- plotly::plot_ly(x = x, type = "histogram", name = "Responses",
    autobinx = FALSE, xbins = list(start = floor(min(x) / width) * width, end = max(x) + width, size = width),
    marker = list(color = palette[["primary"]], line = list(color = "white", width = 1)),
    hovertemplate = paste0(m$label, "<br>%{x:.2f} ", m$unit, "<br>Responses: %{y}<extra></extra>"))
  p <- plotly::layout(p, bargap = .08,
    shapes = list(list(type = "line", x0 = mean(x), x1 = mean(x), y0 = 0, y1 = 1,
      xref = "x", yref = "paper", line = list(color = palette[["accent"]], dash = "dash", width = 2))))
  finish_plot(p, paste0(m$label, " (", m$unit, ")"), "Number of responses", measure_range(key, axis_view))
}

comparison_plot <- function(d, pair, palette = PALETTES$teal, axis_view = "auto") {
  v <- pair_data(d, pair); p <- PAIRS[[pair]]
  if (!nrow(v)) stop("No complete pairs to plot.")
  limits <- range(c(v$x, v$y)); padding <- max(.5, diff(limits) * .1)
  limits <- if (axis_view == "full") c(0, 24) else c(max(0, limits[1] - padding), min(24, limits[2] + padding))
  g <- plotly::plot_ly(x = limits, y = limits, type = "scatter", mode = "lines", name = "Equal sleep",
    line = list(color = "#929da6", dash = "dash", width = 1.5), hoverinfo = "skip")
  g <- plotly::add_trace(g, data = v, x = ~x, y = ~y, text = ~response_id, inherit = FALSE,
    type = "scatter", mode = "markers", name = "Responses",
    marker = list(color = palette[["secondary"]], size = 9, opacity = .65),
    hovertemplate = paste0("%{text}<br>", p$x_label, ": %{x:.2f}<br>", p$y_label, ": %{y:.2f}<extra></extra>"))
  finish_plot(g, paste0(p$x_label, " (", p$unit, ")"), paste0(p$y_label, " (", p$unit, ")"), limits, limits)
}

study_plot <- function(d, palette = PALETTES$teal, axis_view = "auto", fit = TRUE) {
  s <- regression_summary(d); v <- s$data
  g <- plotly::plot_ly()
  if (s$valid && fit) {
    grid <- data.frame(study_hours_7d = seq(min(v$study_hours_7d), max(v$study_hours_7d), length.out = 100))
    pred <- predict(s$model, grid, interval = "confidence")
    g <- plotly::add_trace(g, x = grid$study_hours_7d, y = pred[, "lwr"], type = "scatter", mode = "lines",
      line = list(width = 0), hoverinfo = "skip", name = "Lower confidence limit")
    g <- plotly::add_trace(g, x = grid$study_hours_7d, y = pred[, "upr"], type = "scatter", mode = "lines",
      fill = "tonexty", fillcolor = grDevices::adjustcolor(palette[["accent"]], alpha.f = .15),
      line = list(width = 0), hoverinfo = "skip", name = "95% conventional mean interval")
    g <- plotly::add_trace(g, x = grid$study_hours_7d, y = pred[, "fit"], type = "scatter", mode = "lines",
      line = list(color = palette[["accent"]], width = 2), hoverinfo = "skip", name = "OLS fit")
  }
  g <- plotly::add_trace(g, x = v$study_hours_7d, y = v$avg_sleep_exam_week, text = v$response_id,
    type = "scatter", mode = "markers", name = "Original responses",
    marker = list(color = palette[["primary"]], size = 9, opacity = .6),
    hovertemplate = "%{text}<br>Study: %{x:.2f} hours / 7 days<br>Sleep: %{y:.2f} hours / night<extra></extra>")
  finish_plot(g, "Study time for the exam (hours over seven days)", "Exam-week sleep (hours per night)",
    measure_range("study_hours_7d", axis_view), measure_range("avg_sleep_exam_week", axis_view))
}

diagnostic_plot <- function(d, pair, kind = "paired_qq", palette = PALETTES$teal) {
  if (kind == "paired_qq") {
    v <- pair_data(d, pair); z <- v$y - v$x
    if (length(z) < 3 || sd(z) < 1e-10) return(NULL)
    qq <- qqnorm(z, plot.it = FALSE)
    g <- plotly::plot_ly(x = qq$x, y = qq$y, type = "scatter", mode = "markers", marker = list(color = palette[["primary"]]))
    q <- quantile(z, c(.25, .75)); t <- qnorm(c(.25, .75)); slope <- diff(q) / diff(t)
    line <- q[1] - slope * t[1] + slope * range(qq$x)
    g <- plotly::add_trace(g, x = range(qq$x), y = line, inherit = FALSE, type = "scatter", mode = "lines", line = list(color = palette[["accent"]]), hoverinfo = "skip")
    return(finish_plot(g, "Theoretical normal quantile", "Paired difference (second minus first)"))
  }
  s <- regression_summary(d)
  if (!s$valid) return(NULL)
  g <- plotly::plot_ly(x = fitted(s$model), y = residuals(s$model), type = "scatter", mode = "markers",
    text = s$data$response_id, marker = list(color = palette[["primary"]], opacity = .65),
    hovertemplate = "%{text}<br>Fitted: %{x:.2f}<br>Residual: %{y:.2f}<extra></extra>")
  g <- plotly::layout(g, shapes = list(list(type = "line", xref = "paper", x0 = 0, x1 = 1, y0 = 0, y1 = 0,
    line = list(color = palette[["accent"]], dash = "dash"))))
  finish_plot(g, "Fitted sleep (hours per night)", "Residual (hours per night)")
}
