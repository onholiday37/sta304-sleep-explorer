make_server <- function(data) {
  force(data)
  function(input, output, session) {
    filtered <- shiny::reactive(filter_analysis(data, input$assessment, input$recency, input$other_exams))
    selected_measure <- shiny::reactive(if (is.null(input$measure) || !input$measure %in% unname(NUMERIC_MEASURES)) "avg_sleep_exam_week" else input$measure)
    selected_pair <- shiny::reactive(if (is.null(input$comparison) || !input$comparison %in% names(PAIRS)) "typical_exam" else input$comparison)
    palette <- shiny::reactive(if (is.null(input$palette) || !input$palette %in% names(PALETTES)) PALETTES$teal else PALETTES[[input$palette]])
    axis_view <- shiny::reactive(if (identical(input$axis_view, "full")) "full" else "auto")
    table_rows <- shiny::reactive(response_table(filtered()))
    exam_inference <- shiny::reactive(mean_inference(filtered()$avg_sleep_exam_week))
    paired_inference <- shiny::reactive({
      p <- pair_data(filtered(), selected_pair())
      mean_inference(p$y - p$x, seed = 3042026)
    })
    regression <- shiny::reactive(regression_summary(filtered()))
    shiny::observeEvent(input$reset, {
      for (id in c("assessment", "recency", "other_exams")) shiny::updateSelectInput(session, id, selected = "all")
    })
    output$subset_count <- shiny::renderText(as.character(nrow(filtered())))
    output$filter_count <- shiny::renderText(paste(nrow(filtered()), "of", nrow(data), "responses selected"))
    output$exam_mean <- shiny::renderText(paste0(fmt(measure_summary(filtered(), "avg_sleep_exam_week")$mean), " h"))
    output$mean_change <- shiny::renderText({
      value <- pair_summary(filtered(), "typical_exam")$difference
      if (!is.finite(value)) "--" else paste0(if (value > 0) "+" else "", fmt(value), " h")
    })
    output$distribution_plot <- plotly::renderPlotly({
      shiny::validate(shiny::need(measure_summary(filtered(), selected_measure())$n > 0, "No usable responses match these filters. Try Reset filters."))
      bins <- if (is.null(input$bins)) 12 else input$bins
      distribution_plot(filtered(), selected_measure(), palette(), axis_view(), bins)
    })
    output$comparison_plot <- plotly::renderPlotly({
      shiny::validate(shiny::need(pair_summary(filtered(), selected_pair())$n > 0, "No complete pairs match these filters. Try Reset filters."))
      comparison_plot(filtered(), selected_pair(), palette(), axis_view())
    })
    output$study_plot <- plotly::renderPlotly({
      shiny::validate(shiny::need(regression()$n > 0, "No complete responses match these filters. Try Reset filters."))
      study_plot(filtered(), palette(), axis_view(), !identical(input$show_fit, FALSE))
    })
    output$diagnostic_plot <- plotly::renderPlotly({
      kind <- if (identical(input$diagnostic, "residuals")) "residuals" else "paired_qq"
      p <- diagnostic_plot(filtered(), selected_pair(), kind, palette())
      shiny::validate(shiny::need(!is.null(p), "Not enough variation or complete observations for this diagnostic."))
      p
    })
    output$distribution_interpretation <- shiny::renderText({
      paste0(distribution_text(filtered(), selected_measure()),
        if (selected_measure() == "exam_recency_days") " Days are estimated from ranges: 3.5, 11, 22.5 and 45.5. The final value assumes 31-60 days for the open-ended category; it is not a recorded exam date." else
        if (selected_measure() == "other_exams_7d") " A code of 5 means five or more, so its mean is a lower bound on the true count mean." else "")
    })
    output$comparison_interpretation <- shiny::renderText(comparison_text(filtered(), selected_pair()))
    output$study_interpretation <- shiny::renderText(regression_text(regression()))
    output$mean_inference <- shiny::renderText({
      s <- exam_inference()
      if (!is.finite(s$p)) return(paste0("n = ", s$n, "; mean = ", fmt(s$mean), " hours/night. A t interval requires at least two values and nonzero variance."))
      paste0("Exam-week mean: ", fmt(s$mean), " hours/night; 95% t interval ", ci_text(s$ci),
        "; percentile bootstrap interval ", ci_text(s$bootstrap), ". Based on ", s$n,
        " respondents and 10,000 bootstrap resamples (seed 3042027). These intervals estimate a mean; they do not correct voluntary response bias.")
    })
    output$paired_inference <- shiny::renderText({
      s <- paired_inference(); p <- PAIRS[[selected_pair()]]
      if (!is.finite(s$p)) return(paste0("n = ", s$n, "; mean difference = ", fmt(s$mean), ". A paired test requires at least two nonconstant complete differences."))
      paste0(p$y_label, " minus ", tolower(p$x_label), ": ", fmt(s$mean), " ", p$unit,
        "; 95% paired t interval ", ci_text(s$ci), "; t(", s$df, ") = ", fmt(s$t), ", two-sided p ", format_p(s$p),
        ". Bootstrap interval ", ci_text(s$bootstrap), " (10,000 whole-respondent resamples; seed 3042026). ",
        "Non-normal differences and outliers can affect the t calculation. The bootstrap is a sensitivity check, not a remedy for biased sampling.")
    })
    output$diagnostic_interpretation <- shiny::renderText({
      if (identical(input$diagnostic, "residuals")) {
        s <- regression()
        if (!s$valid) return("A residual diagnostic is unavailable for this subgroup.")
        z <- residuals(s$model)
        sw <- if (length(z) >= 3 && length(z) <= 5000) shapiro.test(z)$p.value else NA_real_
        paste0("Look for curvature, unequal vertical spread and isolated points. Residual Shapiro-Wilk p ", format_p(sw),
          ". The full-sample EDA found uncertain constant variance, so the association tab also reports HC3 intervals. No observations are removed. These plots cannot verify independence or rule out confounding.")
      } else {
        p <- pair_data(filtered(), selected_pair()); z <- p$y - p$x
        sw <- if (length(z) >= 3 && length(z) <= 5000 && sd(z) > 1e-10) shapiro.test(z)$p.value else NA_real_
        paste0("A straight Q-Q pattern is consistent with normal paired differences; tail departures suggest caution. Shapiro-Wilk p ",
          format_p(sw), ". For paired inference, normality concerns the differences, not equality of variance between the original periods.")
      }
    })
    output$quantitative <- DT::renderDT({
      t <- DT::datatable(summary_table(filtered()), rownames = FALSE, selection = "none", escape = TRUE,
        options = list(dom = "t", paging = FALSE, scrollX = TRUE))
      DT::formatRound(t, columns = c("Mean", "Median", "SD", "IQR", "Min", "Max"), digits = 2)
    }, server = FALSE)
    output$categorical <- DT::renderDT({
      t <- DT::datatable(frequency_table(filtered()), rownames = FALSE, selection = "none", escape = TRUE,
        options = list(dom = "t", paging = FALSE, scrollX = TRUE))
      DT::formatRound(t, "Percent", digits = 2)
    }, server = FALSE)
    output$responses <- DT::renderDT({
      t <- DT::datatable(table_rows(), rownames = FALSE, selection = "none", escape = TRUE,
        class = "stripe hover compact", options = list(pageLength = 10, lengthMenu = c(10, 25, 50, 100),
          scrollX = TRUE, order = list(list(0, "asc")), dom = "ftlip", autoWidth = TRUE,
          language = list(search = "Search this table:", searchPlaceholder = "e.g. Final examination",
            emptyTable = "No responses match the current filters", zeroRecords = "No matching responses")))
      DT::formatRound(t, columns = names(table_rows())[c(3:7, 9)], digits = 2)
    }, server = FALSE)
  }
}
