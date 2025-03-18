
# This file is a generated template, your changes will not be overwritten

onehotencodingClass <- if (requireNamespace('jmvcore', quietly=TRUE)) R6::R6Class(
    "onehotencodingClass",
    inherit = onehotencodingBase,
    private = list(
        .run = function() {

             if (is.null(self$options$ColumnToSplit))
                return()
            # Get the variable to split
            var_to_split <- self$data[[self$options$ColumnToSplit]]
            
            if (is.null(var_to_split) || length(var_to_split) == 0) {
                self$results$text$setContent("No data available in the selected column.")
                return()
            }
            
            # Get the separator from user input
            separator <- self$options$separator
            
            # If separator is empty or NULL, default to comma
            if (is.null(separator) || nchar(trimws(separator)) == 0) {
                separator <- ","
            }
            
            # Convert variable to character if it's not already
            var_to_split <- as.character(var_to_split)
            
            # Remove all whitespace from values
            var_to_split <- gsub("\\s+", "", var_to_split)
            
            # Split all values and get unique elements
            all_elements <- unique(unlist(strsplit(var_to_split, separator)))
            all_elements <- all_elements[!is.na(all_elements) & all_elements != ""]
            
            # Sort elements alphabetically for better readability
            all_elements <- sort(all_elements)
            
            # Create the result message
            result_message <- paste0(
                "Analysis of column '", self$options$ColumnToSplit, "':\n\n",
                "Separator used: ", separator, "\n",
                "Number of unique values found: ", length(all_elements), "\n\n",
                "List of unique values:\n",
                paste("- ", all_elements, collapse = "\n")
            )

            # Create keys, titles, and descriptions for each unique value
            keys <- character(length(all_elements))
            titles <- character(length(all_elements))
            descriptions <- character(length(all_elements))
            measureTypes <- rep("nominal", length(all_elements))

            for (i in seq_along(all_elements)) {
                keys[i] <- as.character(i)
                titles[i] <- paste0(self$options$ColumnToSplit, "_", all_elements[i])
                descriptions[i] <- paste0("Binary indicator for value '", all_elements[i], "' in column '", self$options$ColumnToSplit, "'")
            }

            # Set the factor scores output
            self$results$newColumns$set(
                keys=keys,
                titles=titles,
                descriptions=descriptions,
                measureTypes=measureTypes
            )

            # Create binary vectors for each unique value
            for (i in seq_along(all_elements)) {
                # Create a binary vector indicating presence/absence of the current value
                binary_vector <- sapply(var_to_split, function(x) {
                    split_values <- unlist(strsplit(x, separator))
                    factor(ifelse(all_elements[i] %in% split_values, 1, 0), levels=c(0,1))
                })
                
                # Set the values for this component
                self$results$newColumns$setValues(index=i, binary_vector)
            }

            # Set the results text
            self$results$text$setContent(result_message)
          
        })
)
