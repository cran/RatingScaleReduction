rsr <-function(attribute,D, plotRSR=FALSE, method=c("Stop1Max", "StopGlobalMax")){
if (length(names(attribute))==0){
outlist=list(message('****names(attribute)==NULL!!!! Create attribute LABELS (e.g. using colnames)'))}

if (nrow(attribute) != length(D)) {
      stop("Attributes and decision must have the same number of rows")
    }
	
else {
start.auc <-vector(length=ncol(attribute))
for (i in 1:(ncol(attribute))) {
start.auc[i] <-roc(D, attribute[,i], plotROC=FALSE)$auc}

mydata1 <-rbind(start.auc,attribute[,1:(ncol(attribute))])
s1 <-mydata1[,order(mydata1[1,],  decreasing = TRUE)]
s <-s1[2:nrow(s1),]

ss <-c()
ss[1]<-s1[1,1]	
for (i in 2:ncol(s)){
ss[i] <-roc(D, rowSums(s[,1:i]), plotROC=FALSE)$auc
 }

auc1 <-c()
if (method=='Stop1Max'){
i=1
while (ss[i+1]>ss[i]){
auc1[i] <-ss[i]
i=i+1
}
auc.reduct <-c(auc1, ss[i])} else {i=which.max(ss); auc.reduct <-ss[1:i]}

if (length(auc.reduct)>1){
tab <-t(rbind(s1[1,1:length(auc.reduct)],auc.reduct))
colnames(tab) <-c('AUC one variable', 'AUC running total')}
else {
tab <-data.frame(names(s1)[1],auc.reduct)
colnames(tab) <-c('Attribut', 'AUC running total')}

if (plotRSR==TRUE){
	if (length(auc.reduct)==1){
	y=s[,1]
	p <-plot.roc(D, y, plotROC=TRUE)
	outlist=list(rsr.auc=auc.reduct, rsr.label=names(s1)[1], summary=tab, p)}
		else {
		p <-roc(D, rowSums(s[,1:i]))
		x=1-p$specificities
		y=p$sensitivities
		dfr <-data.frame(x,y)
		pp <-ggplot(dfr, aes(x,y))+geom_line(col='red', size=1)+geom_abline(intercept = 0, slope = 1, size=0.5)+
		geom_polygon(fill='gray')+theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(),
		panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))+
		ggtitle("ROC curve of total AUC of reducted rating scale")+ labs(x = "1-specificities", y = "sensitivities")+
		theme(plot.title = element_text(hjust = 0.5))
		
		outlist=list(rsr.auc=auc.reduct, rsr.label=names(s1[1,1:length(auc.reduct)]), summary=tab, pp)}}
else
outlist=list(rsr.auc=auc.reduct, rsr.label=names(s1)[1], summary=tab)}

class(outlist) = "RatingScaleReduction"
return(outlist)	
}

