

#===========================#
#===== 4: ea no dups =======#
#===========================#

#===========================#
#== ROXYGEN documentation ==#
#===========================#

#' ea_no_dups
#'
#' removes duplicates by key variables
#'
#'@param in_data Dataset with duplicates
#'@param in_vars_key Variables to set the key by, for dropping
#'\cr e.g. \code{c("var_1", "var_2", "var_3")}
#'@param opt_delete_all Option to delete all duplicates, default is to delete duplicates starting with first duplicate
#'\cr \emph{default is 0}
#'@param opt_from_last Option to delete duplicates from the last duplicate, default is to delete duplicates starting with the first duplicate
#'\cr \emph{default is 0}
#'@param opt_key_all Option to set the key to every variable (will delete exact duplicates only)
#'\cr \emph{default is 0}
#'@param opt_key_set Option to override need for \code{in_vars_key}, if key is already set
#'\cr \emph{default is 0}
#'@param opt_print_dups Option to print duplicate values into the console
#'\cr \emph{default is 0}
#'@param opt_print Option to print key vars, number of duplicates and number removed (set to 0 to remove printing)
#'\cr \emph{default is 1}
#'@param opt_out_rate gives a summary of number of duplicates and drops
#'\cr \emph{default is 0}
#'@param opt_out_rate_by_vars gives a summary of number of duplicates and drops BY the variables in this character string
#'\cr e.g. \code{c("ea_test_name")}
#'@return Returns data with duplicate observations removed (defaults to removing from first duplicate)
#'@examples duplicate_free_data <- ea_no_dups(test_data, c("student_id","school_name"), opt_print_dups=1, opt_delete_all=1)
#'


#=================================#
#== define ea_out_dups function ==#
#=================================#

# add in option to output duplicates
# add in option to print duplicates
# add in option to have key already set

ea_no_dups <- function(in_data=NULL,
                       in_vars_key=NULL,
                       opt_delete_all=0,
                       opt_from_last=0,
                       opt_key_all=0,
                       opt_key_set=0,
                       opt_print_dups=0,
                       opt_out_rate=0,
                       opt_out_rate_by_vars= NULL,
                       opt_print=1){
  
  # setkey
  if(opt_key_all==0 & opt_key_set==0){setkeyv(in_data, in_vars_key)}
  if(opt_key_all==1 & opt_key_set==0){setkey(in_data)}
  
  # take all duplicates, also prints key
  how_many_dupes <- ea_out_dups(in_data, opt_key_set=1, opt_print=0)
  
  # print key vars
  if (opt_print==1){
    message(paste0("Key Variables: ",paste0(key(in_data),collapse=", ")),collapse=": ")
  }
  
  # save nrow
  nrow_before_drop <- nrow(in_data)
  
  # save name of dataset
  data_name <- deparse(substitute(in_data))
  
  #==================#
  #== remove dupes ==#
  #==================#
  
  # base remove dupes
  if(opt_delete_all==0 & opt_from_last==0){
    out_data <- subset(in_data, !duplicated(in_data, by = key(in_data)))
  }
  
  # option to remove from last
  if(opt_delete_all==0 & opt_from_last==1){
    out_data <- subset(in_data, !duplicated(in_data, by = key(in_data), fromLast=TRUE))
  }
  
  # option to delete all duplicated
  if(opt_delete_all==1 & opt_from_last==0){
    out_data <- subset(in_data, !duplicated(in_data, by = key(in_data)) & !duplicated(in_data, by = key(in_data), fromLast=TRUE))
  }
  
  #===========#
  #== print ==#
  #===========#
  
  # print how many dupes found
  if (opt_print==1){
    message(paste0(nrow(how_many_dupes), " Duplicates Were Found"))
  }
  
  # find how many dropped
  how_many_dropped <- nrow_before_drop-nrow(out_data)
  
  # print how many were dropped
  if (opt_print==1){
    message(paste0(how_many_dropped, " Duplicates Were Dropped"))
  }
  
  # print duplicates if necessary
  if(opt_print_dups==1){print(how_many_dupes)}
  
  #==========================#
  #== create summary table ==#
  #==========================#
  
  # export subset rate
  if (opt_out_rate == 1){
    
    # create subset rate output data table
    tnd_qc_rate_out <- data.table(in_data = data_name,
                                  start_rows = nrow_before_drop,
                                  end_rows = nrow(out_data),
                                  n_dups_total = nrow(how_many_dupes),
                                  n_dups_dropped = how_many_dropped,
                                  key_vars = paste(in_vars_key, collapse = ", "))
  }
  
  #====================================#
  #== summary table for by variables ==#
  #====================================#
  
  # export subset rate by variables
  if (is.null(opt_out_rate_by_vars) == 0){
    
    # calculate nrows before drops (start_rows)
    nrow_before_drop_by <- in_data[, .(start_rows = .N), by = opt_out_rate_by_vars]
    
    # calculate nrows after drops (end_rows)
    nrow_after_drop_by <- out_data[, .(end_rows = .N), by = opt_out_rate_by_vars]
    
    # calculate n_dups_total
    n_dups_total_by <- how_many_dupes[, .(n_dups_total = .N), by = opt_out_rate_by_vars]
    
    # merge data together
    tnd_qc_rate_out_by <- merge(nrow_before_drop_by, nrow_after_drop_by, by = opt_out_rate_by_vars)
    tnd_qc_rate_out_by <- merge(tnd_qc_rate_out_by, n_dups_total_by, by = opt_out_rate_by_vars)
    
    # set any NA's to zero
    tnd_qc_rate_out_by[is.na(n_dups_total), n_dups_total := 0]
    
    # calculate n_dups_dropped
    tnd_qc_rate_out_by[, n_dups_dropped := start_rows - end_rows]
    
    # add in_data name
    tnd_qc_rate_out_by[, in_data :=  data_name]
    
    # add key vars
    tnd_qc_rate_out_by[, key_vars :=  paste(in_vars_key, collapse = ", ")]
    
    # colorder
    setcolorder(tnd_qc_rate_out_by, c("in_data", opt_out_rate_by_vars, "start_rows", "end_rows", "n_dups_total", "n_dups_dropped", "key_vars"))
    
  }
  
  #===================#
  #== return output ==#
  #===================#
  
  # if opt_out_rate_by_vars is on, return de-dupped data and by-variable summary table
  if (opt_out_rate == 1 & is.null(opt_out_rate_by_vars) == 0){
    return(list(out_subset_data = out_data, out_rate_data = tnd_qc_rate_out, out_rate_data_by_vars = tnd_qc_rate_out_by))
  }
  
  # if opt_out_rate_by_vars is on, return de-dupped data and by-variable summary table
  if (opt_out_rate == 0 & is.null(opt_out_rate_by_vars) == 0){
    return(list(out_subset_data = out_data, out_rate_data_by_vars = tnd_qc_rate_out_by))
  }
  
  # if opt_out_rate is on, return de-dupped data and summray table
  if (opt_out_rate == 1 & is.null(opt_out_rate_by_vars)){
    return(list(out_subset_data = out_data, out_rate_data = tnd_qc_rate_out))
  }
  
  # if opt_out_rate is on, return de-dupped data
  if (opt_out_rate == 0){
    return(out_data)
  }
  
}

#===================#
#== 5: ea out dup ==#
#===================#

#===========================#
#== ROXYGEN documentation ==#
#===========================#

#' ea_out_dups
#'
#' outputs all duplicates by key variables
#'
#' @seealso \code{\link{ea_no_dups}} to remove duplicates
#'
#'@param in_data Dataset with duplicates
#'@param in_vars_key Variables to set the key by
#'\cr e.g. \code{c("var_1", "var_2", "var_3")}
#'@param opt_key_all Option to set the key to every variable (will delete exact duplicates only)
#'\cr \emph{default is 0}
#'@param opt_key_set Option to override need for \code{in_vars_key}, if key is already set
#'\cr \emph{default is 0}
#'@param opt_print Option to print key vars (set to 0 to remove printing)
#'\cr \emph{default is 1}
#'@return outputs duplicate obversvations
#'@examples qc_dups <- ea_out_dups(test_data)
#'


#=================================#
#== define ea_out_dups function ==#
#=================================#

ea_out_dups <- function(in_data=NULL,
                        in_vars_key=NULL,
                        opt_key_all=0,
                        opt_key_set=0,
                        opt_print=1){
  
  #============#
  #== setkey ==#
  #============#
  
  # setkey
  if(opt_key_all==0 & opt_key_set==0){setkeyv(in_data, in_vars_key)}
  if(opt_key_all==1 & opt_key_set==0){setkey(in_data)}
  
  #============================================#
  #== output duplicates from both directions ==#
  #============================================#
  
  # output duplicates
  out_data <- subset(in_data, duplicated(in_data, by = key(in_data)) | duplicated(in_data, by = key(in_data), fromLast=TRUE),)
  
  
  return(out_data)
}
