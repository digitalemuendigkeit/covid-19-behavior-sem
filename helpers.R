

get_mm_details <- function(model) {
  data.frame(Construct = model$mmMatrix[,1],
             Type = ifelse(str_starts(model$mmMatrix[,3], "HOC"),
                           "HOC", "Construct"),
             Mode = model$mmMatrix[,3],
             Indicator.or.LOC = model$mmMatrix[,2])
}



#' Checks whether a model has a first stage submodel
#'
#' @param model The model to check
#'
#' @return Returns a boolean (TRUE or FALSE)
#' @export
#'
#' @examples
#' \dontrun{
#' has_fs_model(model)
#' }
has_fs_model <- function(model){
  if(!is.null(model$first_stage_model)) {
    return (FALSE)
  }
  TRUE
}


#' Generates a measurement evaluation table for all mode A constructs
#'
#' @param model The model to analyze
#'
#' @return A dataframe with the required columns
#' @export
#'
#' @examples
#' measurement_evaluation_composite_A(model)
measurement_evaluation_composite_A <- function(model) {
  if(has_fs_model(model)){
    sumfs <- summary(model$first_stage_model)
  } else {
    sumfs <- summary(model)
  }

  evalmm <- get_mm_details(model)

  refevalmmbase <- evalmm %>%
    filter(Mode == "A") %>%
    rename(Indicator = Indicator.or.LOC) %>%
    select(-Type, -Mode) %>%
    rowwise() %>% # add the loadings from the summary table
    mutate(Loading = sumfs$loadings[Indicator, Construct])

  cicr <- get_reliabilty(model)

  refevalmmbase %>% left_join(cicr, by = "Construct")
}


get_reliabilty <- function(model) {
  # Use the first stage model for reliability if available
  if (!is.null(model$first_stage_model)) {
    sumfs <- summary(model$first_stage_model)
  } else {
    sumfs <- summary(model)
  }

  data.frame(Construct = row.names(sumfs$reliability),
             AVE = as.numeric(sumfs$reliability[,3]),
             Calpha = as.numeric(sumfs$reliability[,1]),
             rhoC = as.numeric(sumfs$reliability[,2]),
             rhoA = as.numeric(sumfs$reliability[,4]))
}


get_htmt <- function(bootmodel, bootfsmodel) {
  # generate necessary summaries
  sumbomo <- summary(bootmodel)
  sumbofsmo <- summary(bootfsmodel)

  # get extensive MM model
  evalmm <- get_mm_details(bootmodel)
  # identify reflective measures
  refevalmmbase <- measurement_evaluation_composite_A(model)
  # identify necessary htmt constructs
  htmtvec <- c(unique(refevalmmbase$Construct))

  data.frame(Construct.Rel. = row.names(sumbomo$bootstrapped_HTMT),
             Lower.CI = as.numeric(sumbomo$bootstrapped_HTMT[,5]),
             Upper.CI = as.numeric(sumbomo$bootstrapped_HTMT[,6])) %>%
    rbind(data.frame(Construct.Rel. = row.names(sumbofsmo$bootstrapped_HTMT),
                     Lower.CI = as.numeric(sumbofsmo$bootstrapped_HTMT[,5]),
                     Upper.CI = as.numeric(sumbofsmo$bootstrapped_HTMT[,6]))) %>%
    separate(Construct.Rel., into = c("Construct.1", "Construct.2"),
             sep = "  ->  ") %>%
    filter(Construct.1 %in% htmtvec & Construct.2 %in% htmtvec)
}


get_htmt_problems <- function(boothtmt){
  # find problems in HTMT Table
  htmt_problems <- boothtmt %>%
    mutate(problems = Upper.CI >= 1) %>%
    select(-Lower.CI, -Upper.CI) %>%
    pivot_longer(cols = c(Construct.1, Construct.2),
                 names_to = NULL,  # not needed
                 values_to = "Construct") %>%
    group_by(Construct) %>%
    summarize(problems = any(problems)) %>%
    rename('1.in.HTMT.CI' = problems)
}


print_composite_A_evaluation_DT <- function(eval_table) {

  color_ok <- "#00"
  color_warning <- "#ffe5e5"
  color_bad <- "#ff9999"

  icr_style <- function(table, colname){
   table %>%
    formatStyle(colname,
                backgroundColor =
                  styleInterval(cuts = c(0.6, 0.9,
                                         0.95, 0.999),
                                values = c(color_bad, color_ok, color_warning,
                                           color_bad, color_ok))
                )
  }

  eval_table %>%
  datatable(
    filter = 'top',
    options = list(pageLength = nrow(eval_table)),
    rownames = FALSE,
    colnames = c(
      "Cronbach\'s alpha" = 5,
      "Composite reliability" = 6,
      "Construct reliability" = 7,
      "1 in HTMT CI" = 8
    ),
    caption = 'Results of the reflective measurement model evaluation'
  ) %>%
    formatRound(3:7,
                digits = 3) %>%
    formatStyle('Loading', backgroundColor = styleInterval(
      cuts = c(0.4, 0.708),
      values = c(color_bad, color_warning, color_ok)
    )) %>%
    formatStyle('AVE', backgroundColor =
                  styleInterval(cuts = c(0.5),
                                values = c(color_bad, color_ok))) %>%
    icr_style('Cronbach\'s alpha') %>%
    icr_style('Composite reliability') %>%
    icr_style('Construct reliability') %>%
    formatStyle('1 in HTMT CI', backgroundColor =
                  styleEqual(c(1, 0),
                             c(color_bad, color_ok),
                             default = NULL))
}




redundancy_analysis <- function(model, construct_name) {
  # Copy this snippet for as many formative constructs as you want to evaluate and alter it to your needs
  print(plot(model, title = paste("Redundancy Analysis", construct_name)))
  modelsum <- summary(model)
  # For the final dataframe, you should set a custom name and put the the name of the construct
  cvpse <- data.frame(
      Construct = construct_name,
      'C.V.R^2' = modelsum$paths[1],
      'C.V.PC' = modelsum$paths[3]
    )
  cvpse
}

## Function by https://paulvanderlaken.com/2020/07/28/publication-ready-correlation-matrix-significance-r/
#' correlation_matrix
#' Creates a publication-ready / formatted correlation matrix, using `Hmisc::rcorr` in the backend.
#'
#' @param df dataframe; containing numeric and/or logical columns to calculate correlations for
#' @param type character; specifies the type of correlations to compute; gets passed to `Hmisc::rcorr`; options are `"pearson"` or `"spearman"`; defaults to `"pearson"`
#' @param digits integer/double; number of decimals to show in the correlation matrix; gets passed to `formatC`; defaults to `3`
#' @param decimal.mark character; which decimal.mark to use; gets passed to `formatC`; defaults to `.`
#' @param use character; which part of the correlation matrix to display; options are `"all"`, `"upper"`, `"lower"`; defaults to `"all"`
#' @param show_significance boolean; whether to add `*` to represent the significance levels for the correlations; defaults to `TRUE`
#' @param replace_diagonal boolean; whether to replace the correlations on the diagonal; defaults to `FALSE`
#' @param replacement character; what to replace the diagonal and/or upper/lower triangles with; defaults to `""` (empty string)
#'
#' @return a correlation matrix
#' @export
#'
#' @examples
#' `correlation_matrix(iris)`
#' `correlation_matrix(mtcars)`
correlation_matrix <- function(df,
                               type = "pearson",
                               digits = 3,
                               decimal.mark = ".",
                               use = "all",
                               show_significance = TRUE,
                               replace_diagonal = FALSE,
                               replacement = ""){

  # check arguments
  stopifnot({
    is.numeric(digits)
    digits >= 0
    use %in% c("all", "upper", "lower")
    is.logical(replace_diagonal)
    is.logical(show_significance)
    is.character(replacement)
  })
  # we need the Hmisc package for this
  require(Hmisc)

  # retain only numeric and boolean columns
  isNumericOrBoolean = vapply(df, function(x) is.numeric(x) | is.logical(x), logical(1))
  if (sum(!isNumericOrBoolean) > 0) {
    cat('Dropping non-numeric/-boolean column(s):', paste(names(isNumericOrBoolean)[!isNumericOrBoolean], collapse = ', '), '\n\n')
  }
  df = df[isNumericOrBoolean]

  # transform input data frame to matrix
  x <- as.matrix(df)

  # run correlation analysis using Hmisc package
  correlation_matrix <- Hmisc::rcorr(x, type = )
  R <- correlation_matrix$r # Matrix of correlation coeficients
  p <- correlation_matrix$P # Matrix of p-value

  # transform correlations to specific character format
  Rformatted = formatC(R, format = 'f', digits = digits, decimal.mark = decimal.mark)

  # if there are any negative numbers, we want to put a space before the positives to align all
  if (sum(R < 0) > 0) {
    Rformatted = ifelse(R > 0, paste0(' ', Rformatted), Rformatted)
  }

  # add significance levels if desired
  if (show_significance) {
    # define notions for significance levels; spacing is important.
    stars <- ifelse(is.na(p), "   ", ifelse(p < .001, "***", ifelse(p < .01, "** ", ifelse(p < .05, "*  ", "   "))))
    Rformatted = paste0(Rformatted, stars)
  }
  # build a new matrix that includes the formatted correlations and their significance stars
  Rnew <- matrix(Rformatted, ncol = ncol(x))
  rownames(Rnew) <- colnames(x)
  colnames(Rnew) <- paste(colnames(x), "", sep =" ")

  # replace undesired values
  if (use == 'upper') {
    Rnew[lower.tri(Rnew, diag = replace_diagonal)] <- replacement
  } else if (use == 'lower') {
    Rnew[upper.tri(Rnew, diag = replace_diagonal)] <- replacement
  } else if (replace_diagonal) {
    diag(Rnew) <- replacement
  }

  return(Rnew)
}

## Based on function by https://paulvanderlaken.com/2020/07/28/publication-ready-correlation-matrix-significance-r/
#' correlation_matrix
#' Creates a publication-ready / formatted correlation matrix with different variables for columns on rows, using `Hmisc::rcorr` in the backend.
#'
#' @param df1 dataframe 1; containing numeric and/or logical columns to calculate correlations for; passed on to the rows
#' @param df2 dataframe 2; containing numeric and/or logical columns to calculate correlations for; passed on to the cols
#' @param type character; specifies the type of correlations to compute; gets passed to `Hmisc::rcorr`; options are `"pearson"` or `"spearman"`; defaults to `"pearson"`
#' @param digits integer/double; number of decimals to show in the correlation matrix; gets passed to `formatC`; defaults to `3`
#' @param decimal.mark character; which decimal.mark to use; gets passed to `formatC`; defaults to `.`
#' @param show_significance boolean; whether to add `*` to represent the significance levels for the correlations; defaults to `TRUE`
#'
#' @return a correlation matrix
#' @export
#'
#' @examples
#' `correlation_matrix_2(iris %>% select(starts_with("Sepal")), iris %>% select(starts_with("Petal")))`
correlation_matrix_2 <- function(df1,
                                 df2,
                               type = "pearson",
                               digits = 3,
                               decimal.mark = ".",
                               show_significance = TRUE){

  # check arguments
  stopifnot({
    is.numeric(digits)
    digits >= 0
    is.logical(show_significance)
  })
  # we need the Hmisc package for this
  require(Hmisc)
  require(tidyverse)

  # retain only numeric and boolean columns
  isNumericOrBoolean1 = vapply(df1, function(x) is.numeric(x) | is.logical(x), logical(1))
  if (sum(!isNumericOrBoolean1) > 0) {
    cat('Dropping non-numeric/-boolean column(s):', paste(names(isNumericOrBoolean1)[!isNumericOrBoolean1], collapse = ', '), '\n\n')
  }
  isNumericOrBoolean2 = vapply(df2, function(x) is.numeric(x) | is.logical(x), logical(1))
  if (sum(!isNumericOrBoolean2) > 0) {
    cat('Dropping non-numeric/-boolean column(s):', paste(names(isNumericOrBoolean2)[!isNumericOrBoolean2], collapse = ', '), '\n\n')
  }
  df1 = df1[isNumericOrBoolean1]
  df2 = df2[isNumericOrBoolean2]

  # transform input data frame to matrix
  x1 <- as.matrix(df1)
  x2 <- as.matrix(df2)

  # run correlation analysis using Hmisc package
  correlation_matrix <- Hmisc::rcorr(x = x1, y = x2, type = )
  R <- correlation_matrix$r # Matrix of correlation coeficients
  p <- correlation_matrix$P # Matrix of p-value

  # transform correlations to specific character format
  Rformatted = formatC(R, format = 'f', digits = digits, decimal.mark = decimal.mark)

  # if there are any negative numbers, we want to put a space before the positives to align all
  if (sum(R < 0) > 0) {
    Rformatted = ifelse(R > 0, paste0(' ', Rformatted), Rformatted)
  }

  # add significance levels if desired
  if (show_significance) {
    # define notions for significance levels; spacing is important.
    stars <- ifelse(is.na(p), "   ", ifelse(p < .001, "***", ifelse(p < .01, "** ", ifelse(p < .05, "*  ", "   "))))
    Rformatted = paste0(Rformatted, stars)
  }

  # build a new matrix that includes the formatted correlations and their significance stars
  Rnew <- matrix(Rformatted, ncol = ncol(x1)+ncol(x2))
  rownames(Rnew) <- c(colnames(x1), colnames(x2))
  Rnew <- data.frame(Rnew)
  colnames(Rnew) <- c(colnames(x1), colnames(x2))
  Rnew <- Rnew %>%
    select(colnames(x2))  %>%
    filter(rownames(Rnew) %in% colnames(x1))

  return(Rnew)
}
