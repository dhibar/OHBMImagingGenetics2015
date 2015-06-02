mdsplot <- function(mds.df,pop.interest="CEU",pruningf=0.03,plotfinal=FALSE,flip.x=FALSE,flip.y=FALSE){
	
	#mds.cluster = read.csv(as.character("HM3mds2R.mds.csv"), header=T);
	
	ceu=which(mds.cluster$FID=="CEU")
	chb=which(mds.cluster$FID=="CHB")
	yri=which(mds.cluster$FID=="YRI")
	tsi=which(mds.cluster$FID=="TSI")
	jpt=which(mds.cluster$FID=="JPT")
	chd=which(mds.cluster$FID=="CHD")
	mex=which(mds.cluster$FID=="MEX")
	gih=which(mds.cluster$FID=="GIH")
	asw=which(mds.cluster$FID=="ASW")
	lwk=which(mds.cluster$FID=="LWK")
	mkk=which(mds.cluster$FID=="MKK")
	
	pops=c(ceu, chb, yri, tsi, jpt, chd, mex, gih, asw, lwk, mkk)
	
	my.ancestry=which(mds.cluster$FID==pop.interest)
	
	mean_c1=mean(mds.cluster$C1[my.ancestry])
	mean_c2=mean(mds.cluster$C2[my.ancestry])
	
	my.sample=mds.cluster[-pops,]
	
	#alloc vector to store distances
	dists=rep(0,nrow(my.sample))

	for(x in 1:nrow(my.sample)){
		dists[x]=sqrt((my.sample[x,4] - mean_c1)^2 + (my.sample[x,5] - mean_c2)^2) #get distance from each point to the pop mean
	}

	all.pops=mds.cluster[pops,]

	my.out=my.sample[which(dists < pruningf),]
	names(my.out)=names(mds.cluster)
	write.table(my.out, file=paste0("HM3mds_Pruned_",pruningf,"_",pop.interest,".txt"),quote=F, row.names=F, col.names=T)
	
	my.out=rbind(my.out,all.pops)
	
	
	#Plot MDS Components
	if(plotfinal){
		mds.cluster = my.out
	}
	if(flip.x){
		flip.dimx=-1
	} else {
		flip.dimx=1
	}
	
	if(flip.y){
		flip.dimy=-1
	} else {
		flip.dimy=1
	}
	
	ceu=which(mds.cluster$FID=="CEU")
	chb=which(mds.cluster$FID=="CHB")
	yri=which(mds.cluster$FID=="YRI")
	tsi=which(mds.cluster$FID=="TSI")
	jpt=which(mds.cluster$FID=="JPT")
	chd=which(mds.cluster$FID=="CHD")
	mex=which(mds.cluster$FID=="MEX")
	gih=which(mds.cluster$FID=="GIH")
	asw=which(mds.cluster$FID=="ASW")
	lwk=which(mds.cluster$FID=="LWK")
	mkk=which(mds.cluster$FID=="MKK")
	
	pops=c(ceu, chb, yri, tsi, jpt, chd, mex, gih, asw, lwk, mkk)
	
	#initialize color vector
	colors=rep("red",length(mds.cluster$C2))
	colors[ceu]="lightblue"
	colors[chb]="brown"
	colors[yri]="yellow"
	colors[tsi]="green"
	colors[jpt]="purple"
	colors[chd]="orange"
	colors[mex]="grey50"
	colors[gih]="black"
	colors[asw]="darkolivegreen"
	colors[lwk]="magenta"
	colors[mkk]="darkblue"

	plot(flip.dimx * rev(mds.cluster$C2), flip.dimy * rev(mds.cluster$C1), col=rev(colors), ylab="Dimension 1", xlab="Dimension 2",pch=20)
	legend("topright", c("My Sample", "CEU", "CHB", "YRI", "TSI", "JPT", "CHD", "MEX", "GIH", "ASW","LWK", "MKK"), fill=c("red", "lightblue", "brown", "yellow", "green", "purple", "orange", "grey50", "black", "darkoliv egreen", "magenta", "darkblue"))
	
	if(!plotfinal){
		r=pruningf
		nseg=360
		x.cent <- mean_c2 * flip.dimx
		y.cent <- mean_c1 * flip.dimy
		yy <- y.cent + r*cos( seq(0,2*pi, length.out=nseg) )
		xx <- x.cent + r*sin( seq(0,2*pi, length.out=nseg) )

		lines(xx,yy, col='red')

	}
}