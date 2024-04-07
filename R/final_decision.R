# Function to determine final decision
#' final_decision
#'
#' @param row row of dataframe
#'
#' @return final decision for each row
#' @export

final_decision <- function(row) {
  counts <- table(unlist(strsplit(gsub('[{}\"]', '', row), ", ")))
  
  # Extract just the decision statuses
  decision_statuses <- sapply(strsplit(names(counts), "=>"), function(x) x[2])
  
  # Count the occurrences of each decision status
  included_count <- sum(decision_statuses == "Included")
  excluded_count <- sum(decision_statuses == "Excluded")
  maybe_count <- sum(decision_statuses == "Maybe")
  
  max_count <- max(included_count, excluded_count, maybe_count)
  
  if (max_count == 0 || sum(counts) == 0) {
    return("Tie/Conflict")
  } else {
    final_decision <- ifelse(included_count == max_count, "Included",
                             ifelse(excluded_count == max_count, "Excluded",
                                    ifelse(maybe_count == max_count, "Maybe", "Tie/Conflict")))
    if (sum(final_decision == "Tie/Conflict") > 1) {
      return("Tie/Conflict")
    } else {
      return(final_decision)
    }
  }
}