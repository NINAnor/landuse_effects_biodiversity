#' Calculate Cohen's Kappa Agreement Score
#'
#' This function calculates Cohen's Kappa Agreement Score for decision data in a dataframe.
#'
#' @param dataframe A dataframe containing decision data.
#' @param decision_column The name of the column containing decision data in the dataframe (default is "rayyan_decisions").
#'
#' @return A numeric value representing Cohen's Kappa Agreement Score.
#'
#' @details The function uses regex to extract decision data from the specified column. It then determines the number of reviewers and calculates kappa using either `kappa2` or `kappam.light` based on the number of reviewers.
#'
#' @examples
#' \dontrun{
#' # Example usage:
#' kappa_score <- calculate_kappa(your_dataframe)
#' print(paste("Cohen's Kappa Agreement Score:", kappa_score))
#' }
#'
#' @importFrom stringr str_match_all
#' @importFrom irr kappa2 kappam.light
#'
#' @export

calculate_kappa <- function(dataframe, decision_column = "rayyan_decisions") {
  library(stringr)
  library(irr)
  
  # Extract decisions using regex
  decision_matches <- str_match_all(dataframe[[decision_column]], "\"[^\"]+\"=>\"([^\"]+)\"")
  
  # Determine the number of reviewers
  num_reviewers <- max(sapply(decision_matches, nrow))
  
  # Create vectors for decisions
  decision_columns <- paste0("Decision", 1:num_reviewers)
  
  # Populate decision columns with extracted information
  for (i in seq_along(decision_columns)) {
    dataframe[, decision_columns[i]] <- sapply(decision_matches, function(match) {
      ifelse(length(match) >= i, match[i, 2], NA)
    })
    
    # Pad with NA values to match the number of rows in the dataframe
    dataframe[, decision_columns[i]] <- c(dataframe[, decision_columns[i]], rep(NA, nrow(dataframe) - length(dataframe[, decision_columns[i]])))
  }
  
  # Check the number of reviewers and calculate kappa accordingly
  if (num_reviewers == 2) {
    kappa_score <- kappa2(dataframe[, decision_columns])$value
  } else {
    kappa_score <- kappam.light(dataframe[, decision_columns])$value
  }
  
  return(kappa_score)
}


