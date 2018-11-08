library(dplyr)

univariate_analysis <- function(df, dep_var_name, var_list, round_digits=2) {
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

  cat("-------------------------------------------------------------------------------\n")
  for(n in var_list) {
    cat(n)
    cat("\n")
    cat("\n")

    # Get cohort 1
    df1_n <- df[,n][which(df[,dep_var_name]==l1)]
    # Get cohort 2
    df2_n <- df[,n][which(df[,dep_var_name]==l2)]

    if(df_classes[n]=="numeric") {
      cat("df1_n =", l1, "\n")
      cat(paste0("Mean: ", round(mean(df1_n, na.rm=T), round_digits), "\n"))
      cat(paste0("Median: ", round(median(df1_n), round_digits), "\n"))
      cat(paste0("SD: ", round(sd(df1_n, na.rm=T), round_digits), "\n"))

      cat("\n")

      cat("df2_n =", l2, "\n")
      cat(paste0("Mean: ", round(mean(df2_n, na.rm=T), round_digits), "\n"))
      cat(paste0("Median: ", round(median(df2_n), round_digits), "\n"))
      cat(paste0("SD: ", round(sd(df2_n, na.rm=T), round_digits), "\n"))

      cat("\n")

      # At this time will perform both Student t test and Wilcoxon Rank Sum test
      t1 <- t.test(df1_n, df2_n)
      t2 <- wilcox.test(df1_n, df2_n)

      cat("---------------------------------------\n")
      cat("Student's t Test:\n")
      print(t1)

      if(t1$p.value < 0.0001) {
        cat("Significance: ***\n")
      } else if (t1$p.value < 0.001) {
        cat("Significance: **\n")
      } else if (t1$p.value < 0.05) {
        cat("Significance: *\n")
      } else {
        cat("Significance: None\n")
      }
      cat("---------------------------------------\n")
      cat("Wilcoxon Rank Sum Test:\n")
      print(t2)

      if(t2$p.value < 0.0001) {
        cat("Significance: ***\n")
      } else if (t2$p.value < 0.001) {
        cat("Significance: **\n")
      } else if (t2$p.value < 0.05) {
        cat("Significance: *\n")
      } else {
        cat("Significance: None\n")
      }

    }
    if(df_classes[n]=="factor") {
      # Make a table
      t <- table(df[,dep_var_name], df[,n])

      cat(paste0(l1, " (n=", length(df1_n), ")\n"))
      for(l in levels(df1_n)) {
        cat(paste0(l, ": ", length(df1_n[which(df1_n==l)]), "(", round(length(df1_n[which(df1_n==l)]) / length(df1_n) * 100, round_digits), ")\n"))
      }
      cat("\n")

      cat(paste0(l2, " (n=", length(df2_n), ")\n"))
      for(l in levels(df1_n)) {
        cat(paste0(l, ": ", length(df2_n[which(df2_n==l)]), "(", round(length(df2_n[which(df2_n==l)]) / length(df2_n) * 100, round_digits), ")\n"))
      }
      cat("\n")

      # Seeing if any cell has <10 items
      less_than_ten <- FALSE
      for(r in 1:nrow(t)) {
        for (c in 1:ncol(t)) {
          if(t[r,c] <= 10) {
            less_than_ten <- TRUE
            break
          }
        }
      }

      # Print the table
      print(t)

      cat("---------------------------------------\n")
      if(less_than_ten) {
        cat("Fisher Exact Test:\n")

        tt <- fisher.test(t,workspace=2e8)
        print(tt)

        if(tt$p.value < 0.0001) {
          cat("Significance: ***\n")
        } else if (tt$p.value < 0.001) {
          cat("Significance: **\n")
        } else if (tt$p.value < 0.05) {
          cat("Significance: *\n")
        } else {
          cat("Significance: None\n")
        }

      } else {
        cat("Chi-Square Test:\n")

        tt <- chisq.test(t)
        print(tt)

        if(tt$p.value < 0.0001) {
          cat("***\n")
        } else if (tt$p.value < 0.001) {
          cat("**\n")
        } else if (tt$p.value < 0.05) {
          cat("*\n")
        } else {
          cat("Significance: None\n")
        }
      }
    }
    cat("-------------------------------------------------------------------------------\n")
  }
}
