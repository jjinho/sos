# Get the matched data and the n-sized matched groups information
# Requires the 'm.out' object from the 'MatchIt' library
# Requires the 'data.table' library
# Parameters:
# 	m.out 		= the m.out object from calling 'matchit'
# 		    	  (e.g. matchit(formula, data=prematch_df))
# 	df 	  		= the data.frame object that was used by MatchIt to create
# 				    the matched data
#	join_index  = the unique identifier used to left join items from the
#				      prematch data.frame to the postmatch data.frame
# Returns:
# 	data.frame that has the matched data (called by 'match.data(m.out)' with a
#	  new column called STRATA which has the n-sized matched group information
match_data_groups <- function(m.out, df, join_index) {
    m <- setDT(as.data.frame(m.out$match.matrix), keep.rownames=T)[]
    m <- as.matrix(m)
	df$STRATA <- NA
    # Starting the strata index
	strata <- 1

    for(i in 1:nrow(m)) {
        # Each column is the matched item
		for(j in 1:ncol(m)) {
            matched_index <- m[i,j]

			# MatchIt should only match indices where
			# the index is not an NA
            if(!is.na(matched_index)) {
                df[matched_index,]$STRATA <- strata
            }
        }
		# Each item in the row will have the same strata
        strata <- strata + 1
    }
    df$STRATA <- as.numeric(df$STRATA)

	# Select only the unique index which we will join on and STRATA
    df <- dplyr::select_(df, join_index, "STRATA")
	return(left_join(match.data(m.out), df))
}
