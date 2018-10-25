summarize_data <- function(df, var_list=names(df), round_digits=2) {
	df_classes <- sapply(df, class)
	for(n in var_list) {
		cat(n)
		cat("\n")
		if(df_classes[n]=="numeric") {
			cat(paste0("Mean: ", round(mean(df[,n], na.rm=T), round_digits), "\n"))
			cat(paste0("Median: ", round(median(df[,n]), round_digits), "\n"))
			cat(paste0("SD: ", round(sd(df[,n], na.rm=T), round_digits), "\n"))
		}
		if(df_classes[n]=="factor") {
			# Get the total size
			t = 0
			for(l in levels(df[,n])) {
				t <- t + nrow(df[which(df[,n]==l),])
			}
			for(l in levels(df[,n])) {
				cat(paste0(l, ": ", nrow(df[which(df[,n]==l),]), "(", round(nrow(df[which(df[,n]==l),]) / t * 100, round_digits), ")\n"))
			}
		}
		cat("\n")
	}
}
