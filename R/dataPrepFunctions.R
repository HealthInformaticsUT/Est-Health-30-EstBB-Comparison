#' Function
#' @param inputData
cleanData <- function(inputData){
  inputData <- inputData %>%
    dplyr::filter(!(gender_EN %in% c(NA, "-"))) %>%
    dplyr::filter(!(year %in% c(NA, "-"))) %>%
    dplyr::filter(!(age_group %in% c(NA, "-"))) %>%
    dplyr::filter(!(denominator %in% c(NA))) %>%
    dplyr::filter(!(patient_count %in% c(NA)))
  return(inputData)
}
#' Function
#' @param inputData
topData <- function(inputData){
  data <- inputData %>%
    dplyr::filter(!(parent0_code %in% c(NA))) %>%
    dplyr::filter(parent2_code %in% c(NA)) %>%
    dplyr::filter(parent1_code %in% c(NA))
  return(data)
}
#' Function
#' @param inputData
par1Data <- function(inputData){
  data <- inputData %>%
    dplyr::filter((parent2_code %in% c(NA))) %>%
    dplyr::filter(!(parent1_code %in% c(NA)))
  return(data)
}
#' Function
#' @param inputData
par2Data <- function(inputData){
  data <- inputData %>%
    dplyr::filter(!(parent2_code %in% c(NA)))
  return(data)
}
chapterData <- function(inputData) {
  data <- inputData %>%
    dplyr::filter(!(is.na(parent0_code) | parent0_code == "NA")) %>%
    dplyr::filter(is.na(parent1_code) | parent1_code == "NA") %>%
    dplyr::filter(is.na(parent2_code) | parent2_code == "NA") %>%
    dplyr::select(
      parent0_code,
      parent0_name,
      prevalence,
      patient_count,
      denominator
    )
  return(data)
}
blockData <- function(inputData) {
  data <- inputData %>%
    dplyr::filter(!(is.na(parent0_code) | parent0_code == "NA")) %>%
    dplyr::filter(!(is.na(parent1_code) | parent1_code == "NA")) %>%
    dplyr::filter(is.na(parent2_code) | parent2_code == "NA") %>%
    dplyr::select(
      parent0_code,
      parent0_name,
      parent1_code,
      parent1_name,
      prevalence,
      patient_count,
      denominator
    )
  return(data)
}
categoryData <- function(inputData) {
  data <- inputData %>%
    dplyr::filter(!(is.na(parent0_code) | parent0_code == "NA")) %>%
    dplyr::filter(!(is.na(parent1_code) | parent1_code == "NA")) %>%
    dplyr::filter(!(is.na(parent2_code) | parent2_code == "NA")) %>%
    dplyr::select(
      parent0_code,
      parent0_name,
      parent1_code,
      parent1_name,
      parent2_code,
      parent2_name,
      prevalence,
      patient_count,
      denominator
    )
  return(data)
}
