

library(mgcv)
library(gratia)
library(itsadug)
library(funk)
library(ggplot2)

theme_set(theme_sleek())

gamit_SZ<-function(df, species, plot =F, save = F){

	df <- df[df$Species == species,]

	df$time <- scale(df$DATE.ym)
	df$dmi.scaled <- scale(df$dmi)
	df$enso.scaled <- scale(df$benso)
	df$month.scaled <- scale(df$month)

	## cap outliers to 95% max.
	one_col<-df$cpue
	a<-as.vector(round(quantile(one_col, c(0.95), na.rm = T),2))
	one_col[which(one_col > a)]<-a
	df$cpue<-one_col


	mgam<-mgcv::gam(cpue ~ 
	              s(time, bs='cr') + 
	              # s(time, bs='cr', by = type) + 
	              s(enso.scaled, bs = 'cr') +  
	              s(dmi.scaled, bs ='cr') + 
	              s(month.scaled, bs = 'cc') +
	              s(time, SZ, bs = "fs", m=1), 
	              data = df, family='Gamma'(link='log'))

	print(summary(mgam))

	if(plot == TRUE){
	pdf(file = paste0('figures/model_output/', species, '.pdf'), height= 7, width=12)
	print(draw(mgam))
	print(appraise(mgam))
	dev.off()
}

	## setup predictor df for temporal smooths
	time.pred<-expand.grid(time=seq(min(df$time), max(df$time),length.out=100), 
					dmi.scaled=0, enso.scaled=0, month.scaled=0, SZ = '330', 
					#type = c('pelagic', 'demersal'), 
					species=species)
	time.pred$time.raw<-seq(min(df$DATE.ym), max(df$DATE.ym),length.out=100)

	pred<-predict(mgam, newdata=time.pred, type='response', exclude = 's(time,SZ)', se.fit=T)
	time.pred$fit<-pred$fit
	time.pred$se<-pred$se.fit*2

	## get predicted covariate effects
	enso.pred<-evaluate_smooth(mgam, "enso.scaled")
	enso.pred$enso.raw<-seq(min(df$benso), max(df$benso),length.out=100)
	enso.pred$species<-species

	dmi.pred<-evaluate_smooth(mgam, "dmi.scaled")
	dmi.pred$dmi.raw<-seq(min(df$dmi), max(df$dmi),length.out=100)
	dmi.pred$species<-species

	month.pred<-evaluate_smooth(mgam, "month.scaled")
	month.pred$month.raw<-seq(min(df$month), max(df$month),length.out=100)
	month.pred$species<-species

	## save observed data
	observed<-df
	observed$species<-species

	results<-list(time.pred, enso.pred, dmi.pred, month.pred, observed)

	if(save == TRUE){
	save(results, mgam, file=paste0('data/results/gam_', species, '.Rdata'))
}

	return(results)
	

}


gamit_full<-function(df, species, plot =F, save = F, cap = NULL){

	df <- df[df$Species == species,]

	df$time <- scale(df$DATE.ym)
	df$dmi.scaled <- scale(df$dmi)
	df$enso.scaled <- scale(df$benso)
	df$month.scaled <- scale(df$month)

	# cap outliers to 95% max.
	if(!missing(cap)){
	one_col<-df$cpue
	a<-as.vector(round(quantile(one_col, c(cap), na.rm = T),2))
	one_col[which(one_col > a)]<-a
	df$cpue<-one_col
}

	mgam<-mgcv::gam(cpue ~ 
	              s(time, bs='cr', k=-1) + 
	              s(enso.scaled, bs = 'cr', k=-1) +  
	              s(dmi.scaled, bs ='cr', k=-1) + 
	              s(month.scaled, bs = 'cc', k=-1) +
				  s(SZ, bs = "re"), 
	              data = df, family='Gamma'(link='log'))

	print(summary(mgam))

	if(plot == TRUE){
	pdf(file = paste0('figures/model_output/', species, '.pdf'), height= 7, width=12)
	print(draw(mgam))
	print(appraise(mgam))
	dev.off()
}

	## setup predictor df for temporal smooths
	time.pred<-expand.grid(time=seq(min(df$time), max(df$time),length.out=100), 
					dmi.scaled=0, enso.scaled=0, month.scaled=0, SZ = unique(df$SZ)[1], 
					#type = c('pelagic', 'demersal'), 
					species=species)
	time.pred$time.raw<-seq(min(df$DATE.ym), max(df$DATE.ym),length.out=100)

	pred<-predict(mgam, newdata=time.pred, type='response', exclude = 's(SZ)', se.fit=T)
	time.pred$fit<-pred$fit
	time.pred$se<-pred$se.fit*2

	## get predicted covariate effects
	enso.pred<-evaluate_smooth(mgam, "enso.scaled")
	enso.pred$enso.raw<-seq(min(df$benso), max(df$benso),length.out=100)
	enso.pred$species<-species

	dmi.pred<-evaluate_smooth(mgam, "dmi.scaled")
	dmi.pred$dmi.raw<-seq(min(df$dmi), max(df$dmi),length.out=100)
	dmi.pred$species<-species

	month.pred<-evaluate_smooth(mgam, "month.scaled")
	month.pred$month.raw<-seq(min(df$month), max(df$month),length.out=100)
	month.pred$species<-species

	## save observed data
	observed<-df
	observed$species<-species

	results<-list(time.pred, enso.pred, dmi.pred, month.pred, observed, mgam)

	if(save == TRUE){
	save(results, mgam, file=paste0('data/results/gam_', species, '.Rdata'))
}

	return(results)
	

}