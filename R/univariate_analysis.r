univariate_analysis <- function(df, dep_var_name, var_list) {
  df_classes <- sapply(df, class)
  # Make sure that df has all the variables in var_list
  if(!all(var_list %in% names(df_classes))) {
    cat("Variable list has variable names that are not in data frame\n")
    return
  }

  # Make sure that the dependent variable is in df
  if(!(dep_var_name %in% names(df_classes))) {
    cat("Dependent variable is not in the data frame\n")
    return
  }

  # Make sure that the dependent variable is a factor with 2 levels
  if(!is.factor(df[,dep_var_name]) | nlevels(df[,dep_var_name]) != 2) {
    cat("Dependent variable is invalid\n")
    return
  }

  # Get the two levels of the dependent variable
  l1 <- levels(df[,dep_var_name])[1]
  l2 <- levels(df[,dep_var_name])[2]

  for(n in var_list) {
    cat(n)
    cat("\n")
    if(df_classes[n]=="numeric") {
      # Get cohort 1
      df1_n <- df[,n][which(df[,dep_var_name]==l1)]
      df2_n <- df[,n][which(df[,dep_var_name]==l2)]

      cat("df1_n =", l1, "\n")
      cat("df2_n =", l2, "\n")
      # At this time will perform both Student t test and Wilcoxon Rank Sum test
      cat("Student's t Test:\n")
      print(t.test(df1_n, df2_n))

      cat("\n")

      cat("Wilcoxon Rank Sum Test:\n")
      print(wilcox.test(df1_n, df2_n))
    }
    if(df_classes[n]=="factor") {
      # Make a table
      t <- table(df[,dep_var_name], df[,n])

      # Seeing if any cell has <10 items
      less_than_ten <- FALSE
      for(r in 1:nrow(t)) {
        for (c in 1:ncol(t)) {
          if(t[r,c] < 10) {
            less_than_ten <- TRUE
            break
          }
        }
      }

      # Print the table
      print(t)

      if(less_than_ten) {
        cat("Fisher Exact Test:\n")
        print(fisher.test(t,workspace=2e8))
      } else {
        cat("Chi-Square Test:\n")
        print(chisq.test(t))
      }
    }
    cat("-------------------------------------------------------------------------------\n")
  }
}
