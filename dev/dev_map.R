df <- RCDBT::GetRegisterData(2245)
vl <- RCDBT::GetValueLabels(2245)

labs <- names(df)[names(df) %in% unique(vl[["ColumnName"]])]

droplevels <- TRUE
if (is.logical(droplevels)) {
  droplevels <- rep(droplevels, length(labs))
}

labels_list <- list()
suffix <- "_label"

for (var in labs) {
  var_labels <- vl[vl[["ColumnName"]] == var,]
  var_labels[["ColumnName"]] <- NULL
  names(var_labels) <- c(var, paste0(var, suffix))
  labels_list[[var]] <- var_labels
}

for (var in labs) {
  # Save column index for variable and variable names
  col_order <- colnames(df)
  idx_of_var <- which(col_order == var)
  # put label var in correct position next to codes
  col_order <- c(
    # All vars up to var
    col_order[2:idx_of_var-1],
    # Var and var_suffix
    var, paste0(var, suffix),
    # All vars after var
    col_order[(idx_of_var+1):length(col_order)]
  )

  df <- merge(df, labels_list[[var]], all.x = TRUE, sort = FALSE)
  # Re-arrange columns to original order
  df <- df[, col_order]
}

Map(merge, df, labels_list, all.x = TRUE)

df[, labs] <- Map(var_to_char, df[, labs], labels_list, labs)

df[, labs] <- Map(var_to_factor, df[, labs], labels_list, labs, TRUE)

