###############################################################################################
# Function description
#   AUC of a single attribute
#   Compute AUC of every single attribute
#
# Parameters
#   attribute: a matrix or data.frame containing attributes
#   D: the decision vector
#
# Return the list of
#   auc: AUC of a single attribute
#   item: attribute labels
#   summary: a summary table
###############################################################################################

startAuc <- function(attribute, D) {
    
    # attribute - the matix of attributes, every attribute must have headers D - the decision vector
    # testing for null length 
    if (length(names(attribute)) == 0) {
        outlist <- list(message("names(attribute)==NULL Create attribute LABELS (e.g. using colnames)"))
    } 
    else {
        # compute AUC for every attribute
        start.auc <- sapply(1:ncol(attribute), function(i)
            roc(D, attribute[, i], plotROC = FALSE)$auc)
        # create AUC one variable and AUC running total of data table
        tab <- data.frame(item = names(attribute)[1:(ncol(attribute))], 
                          auc = start.auc)
        # create output list
        outlist <- list(auc = tab$auc, item = as.character(tab$item), summary = tab)
    }
    class(outlist) = "startAuc"
    return(outlist)
}
