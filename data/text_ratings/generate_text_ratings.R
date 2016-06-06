library(dplyr)

text_ratings_simple <- function(parody_data, seed=1) {
  parody_type_1_all <- dplyr::filter(parody_data, question=="Q1",
                                 text_type == "Type1")
  parody_type_2_all <- dplyr::filter(parody_data, question=="Q1",
                                 text_type == "Type2")
  parody_type_1 <- dplyr::filter(parody_type_1_all, language=="English")
  parody_type_2 <- dplyr::filter(parody_type_2_all, language=="English")
  mean_type_1 <- with(parody_type_1, mean(rating), na.rm=T)
  sd_type_1 <- with(parody_type_1, sd(rating), na.rm=T)
  n_type_1 <- 2*nrow(parody_type_1_all)
  mean_type_2 <- with(parody_type_2, mean(rating, na.rm=T))
  sd_type_2 <- with(parody_type_2, sd(rating, na.rm=T))
  n_type_2 <- 2*nrow(parody_type_2_all)
  if (!is.null(seed)) set.seed(seed)
  sample_1 <- rnorm(n_type_1, mean_type_1, sd_type_1)
  sample_2 <- rnorm(n_type_2, mean_type_2, sd_type_2)
  if (!is.null(seed)) set.seed(NULL)
  result <- rbind(data.frame(rating=sample_1, text_type="Type 1"),
                  data.frame(rating=sample_2, text_type="Type 2"))
  return(result)
}

text_ratings_small <- function(parody_data, seed=NULL) {
  new_ratings <- text_ratings(parody_data)
  return(result)
}

parody_2groups <- function(parody_data) {
  parody_type_1_all <- dplyr::filter(parody_data, question=="Q1",
                                 text_type == "Type1")
  parody_type_2_all <- dplyr::filter(parody_data, question=="Q1",
                                 text_type == "Type2")
  parody_type_1 <- dplyr::filter(parody_type_1_all, language=="English")
  parody_type_2 <- dplyr::filter(parody_type_2_all, language=="English")
  sample_1 <- parody_type_1$rating[!is.na(parody_type_1$rating)]
  sample_2 <- parody_type_2$rating[!is.na(parody_type_2$rating)]
  result <- rbind(data.frame(rating=sample_1, text_type="Type 1"),
                  data.frame(rating=sample_2, text_type="Type 2"))
  return(result)
}
