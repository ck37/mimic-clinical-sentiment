#' @title Plot a ROC curve from cross-validated AUC from SuperLearner
#' @description
#' Plots the ROC curve for a single learner from a SuperLearner object,
#' defaulting to the minimum estimated risk learner. Based on code by Alan
#' Hubbard.
#'
#' @param x SuperLearner object
#' @param y Outcome vector if not already included in the SL object.
#' @param learner Which learner to plot (numeric index or character string).
#'   Defaults to minimum risk learner. Can be a vector.
#' @param title Title to use in the plot.
#' @param subtitle TBD.
#' @param digits Digits to use when rounding AUC and CI for plot.
#' @param ... Any additional unused arguments, due to the auc_table generic.
#'
#' @return List with plotted AUC & CI, table of AUC results for all learners,
#'       and the name of the best learner.
#'
#' @examples
#'
#' library(SuperLearner)
#' library(ck37r)
#'
#' data(Boston, package = "MASS")
#'
#' set.seed(1)
#' sl = SuperLearner(Boston$chas, subset(Boston, select = -chas),
#'                   family = binomial(), SL.library = c("SL.mean", "SL.glm"),
#'                   cvControl = list(V = 2))
#'
#' sl
#'
#' plot_roc(sl, y = Boston$chas)
#'
#' @references
#' LeDell, E., Petersen, M., & van der Laan, M. (2015). Computationally
#' efficient confidence intervals for cross-validated area under the ROC curve
#' estimates. Electronic journal of statistics, 9(1), 1583.
#'
#' Polley EC, van der Laan MJ (2010) Super Learner in Prediction. U.C. Berkeley
#' Division of Biostatistics Working Paper Series. Paper 226.
#' http://biostats.bepress.com/ucbbiostat/paper266/
#'
#' Sing, T., Sander, O., Beerenwinkel, N., & Lengauer, T. (2005). ROCR:
#' visualizing classifier performance in R. Bioinformatics, 21(20), 3940-3941.
#'
#' van der Laan, M. J., Polley, E. C. and Hubbard, A. E. (2007) Super Learner.
#' Statistical Applications of Genetics and Molecular Biology, 6, article 25.
#' http://www.degruyter.com/view/j/sagmb.2007.6.issue-1/sagmb.2007.6.1.1309/sagmb.2007.6.1.1309.xml
#'
#' @importFrom methods slot
#' @importFrom ROCR prediction performance
#' @importFrom ggplot2 qplot labs theme_bw annotate
#'
#' @export
plot_roc2.SuperLearner =
  function(x,
           y = x$Y,
           learner = NULL,
           title = "SuperLearner cross-validated ROC",
           subtitle = NULL,
           labels = NULL,
           digits = 4,
           ...) {

  # Better object name.
  sl = x

  auc_table = ck37r::auc_table(sl, y)

  # Choose the learner with the highest AUC.
  if (is.null(learner)) {
    # Extract the original learner index based on learner name in the AUC table.
    # Take the first learner if there are ties.
    learner_index = which(names(sl$cvRisk) == auc_table$learner[which.max(auc_table$auc)])[1]
  } else {
    if (class(learner) == "character") {
      # Can select multipler learners if the names are duplicative or learner is a vector.
      learner_index = which(names(sl$cvRisk) %in% learner)
    } else {
      # Learner argument is already an index.
      learner_index = learner
    }
  }

  
  # We need to index using learner name because auc_table() has been sorted.
  learner_name = names(sl$cvRisk)[learner_index]

  # Set learner name as the subtitle if no subtitle was specified.
  if (is.null(subtitle)) {
    subtitle = paste("Learner(s):", paste(learner_name, collapse = ", "))
  }
  
  
  # Only print AUC and CI if we have a single learner.
  if (length(learner_index) == 1L) {
    # Take the first row in case the best learner is duplicated for some random reason.
    best_row = auc_table[rownames(auc_table) == learner_index, , drop = FALSE]
    auc =  best_row$auc
    ci_upper = best_row$ci_upper
    ci_lower = best_row$ci_lower
  
    txt = paste0("AUC = ",
                 sprintf(paste0("%0.", digits, "f"), round(auc, digits)),
                 "\n95% CI = ", sprintf("%0.3f", round(ci_lower, digits)),
                 " - ", sprintf("%0.3f", round(ci_upper, digits)))
  } else {
    # Multiple learner version.
    txt = ""
    auc = NA
    ci_upper = NA
    ci_lower = NA
  }
  
  
  plot_df = NULL
  # Loop over learners in case we have multiple.
  for (learner_i in learner_index) {
    name_i = names(sl$cvRisk)[learner_i]
    preds = sl$Z[, learner_i]
    pred = ROCR::prediction(preds, y)
    perf1 = ROCR::performance(pred, "sens", "spec")

    x_vals = 1 - methods::slot(perf1, "x.values")[[1]]
    y_vals = methods::slot(perf1, "y.values")[[1]]
    plot_df = rbind(plot_df, data.frame(x = x_vals, y = y_vals, learner = name_i))
    
  }
  
  # Specify the levels so that the ordering doesn't convert to alphabetic.
  plot_df$learner = factor(plot_df$learner, levels = unique(plot_df$learner))
  
  if (!is.null(labels)) {
    labels_aucs = labels
    for (i in seq(length(learner))) {
      learner_name = learner[i]
      auc = auc_table[auc_table$learner == learner_name, "auc", drop = FALSE]
      auc_string = paste0(sprintf(paste0("%0.", digits, "f"), round(auc, digits)))
      labels_aucs[i] = paste0(labels_aucs[i], " (AUC = ", auc_string, ")")
    }
    # Use prettier names for the learners for use in the plot legend.
    levels(plot_df$learner) = labels_aucs
  }

  # ggplot version.
  p = ggplot2::ggplot(data = plot_df,
                         aes(x = x, y = y, group = learner, color = learner)) +
          geom_line() + 
          ggtitle(title) + 
          ggplot2::labs(subtitle = subtitle,
                        x = "False positive % (1 - specificity)",
                        y = "True positive % (sensitivity)") +
          ggplot2::theme_bw() +
          ggplot2::annotate("segment", x = 0, xend = 1, y = 0, yend = 1)
 
  if (txt != "") {
    p = p + ggplot2::annotate("text", x = 0.63, y = 0.15, label = txt, size = 6) 
  }
  
  if (length(learner) > 1) {
    p = p + theme(legend.position = c(0.75, 0.25),
                  legend.title = element_blank(),
                  legend.background = element_rect(fill = "#f5f5f5", color = "#e0e0e0"))
    
  }
  
  print(p)

  # Return AUC, AUC CI, and full table of AUC results.
  results = list(auc = auc,
                 auc_ci = c(ci_lower, ci_upper),
                 auc_table = auc_table,
                 learner_best = learner_name, 
                 plot_df = plot_df,
                 plot = p)

  # Return results invisibly.
  invisible(results)
}