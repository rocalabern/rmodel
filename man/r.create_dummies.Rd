% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/rmodel_model_preprocess.R
\name{r.create_dummies}
\alias{r.create_dummies}
\title{r.create_dummies}
\usage{
r.create_dummies(df, list_var = colnames(df)[lapply(df, class) \%in\%
  c("factor", "character")], df_input = df, deleteOriginalVar = TRUE,
  levelsOnly = FALSE, fullRank = FALSE, sep = "_")
}

