CheckAttr4Inclusion <-function(attribute,D, plotCheck=FALSE, method=c("delong", "bootstrap",
"venkatraman", "sensitivity", "specificity"), boot.n, alternative = c("two.sided", "less", "greater")){
if (length(names(attribute))==0){
outlist=list(message('****names(attribute)==NULL!!!! Create attribute LABELS (e.g. using colnames)'))}

else {
tot.auc <-totalAuc(attribute, D)$total.auc
item <-totalAuc(attribute, D)$item
o.attribute <-totalAuc(attribute, D)$ordered.attribute
j <-which.max(tot.auc) #global MAX criteria
 
item.next <-roc(D, rowSums(o.attribute[,1:(j+1)]), plotROC=FALSE)
rsr.auc <-roc(D, rowSums(o.attribute[,1:j]), plotROC=FALSE)
test <-roc.test(rsr.auc, item.next, method, alternative, boot.n) #H1: difference
outlist=list(test)
}

if (plotCheck==TRUE) {
x=1-test$roc1$specificities
y=test$roc1$sensitivities

x1=1-test$roc2$specificities
y1=test$roc2$sensitivities

plot(x,y, type='l', lwd=2)
lines(x1,y1, type='l', lwd=2, col='red')
legend("bottomright", lwd=c(1,2), col=c('black','red'), c(paste('Total AUC of rsr',round(test$roc1$auc,3)), 
paste("Total AUC of rsr plus next attribute",round(test$roc2$auc,3))))
abline(0,1,lwd=2, col='gray')
}

return(test)
}
