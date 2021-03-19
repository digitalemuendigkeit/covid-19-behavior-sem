

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



