

# WAS A WHILE AGO, BUT THE FUNCTION AT BOTTOM OF THIS CODE IS WHAT I USED TO COMPARE SIGNIFICANCE OR ALL POSSIBLE ISLAND-YEAR PAIRS for both bia (human data) and cnet (robot)
# ITS A BIT COMPLICATED, SO HAVE JUST PULLED OUT THE CODE TO GENERATE CI HERE ...

#BASE data used in the function below was essentially an entry for each island-year combination with N = # of sites, mean and se for BIA (human) and mean and se for CNET (robot)
df<-data.frame(ISLAND=M$ISLAND, N=M$N, ANALYSIS_YEAR=M$ANALYSIS_YEAR, bia=M[,BENTH], cnet=M[,aBENTH], bia_se=S[,BENTH], cnet_se=S[,aBENTH], stringsAsFactors=F)


#TO SIMPLIFY, DOING THIS FOR JUST ONE ISLAND-YEAR COMBO (e.g. for record df[1,])
df<-df[1,]


#calculate mean and SE of difference between CNET and BIA
mean_diff<-abs(df$bia-df$cnet)
se_diff<-sqrt((df$bia_se^2) + (df$bia_se^2))


#calculate how many times SE to get CI, depending on the sample sizes (of the two groups you are comparing).
# Dividing alpha by 2 because this is two tailed test, therefore for alpha of 5%, we need to know how many times SE to get to the 2.5% or 97.5% of the distribution (both are same number of SEs, as this is a normal=balanced distribution)
# ALSO, although written as a way tog et CI for a comparison between two groups, this is also a standard way to generate a CI from any SE - as long as you know the degrees of freedom	
ALPHA=0.05			#Chosen signficance level
SE_SCALER<-abs(qt(ALPHA/2, (df$N+df$N)-2))      #second part of this is degrees of freedom, which is N for first group + N for second group minus 2. Here, both groups have size N (but forumula works whether that is true or not)
diff_CI<-se_diff*SE_SCALER


#RESULT
#if(mean_diff>diff_CI) then differences between the two groups is significant at the chosen alpha

##### ORIGINAL FUNCTION BELOW

CompareCNetBIA<-function(M, S, BENTH="CORAL"){
#m or s have to be for a single scheme and region

	aBENTH<-paste("a", BENTH, sep="")
	df<-data.frame(ISLAND=M$ISLAND, N=M$N, ANALYSIS_YEAR=M$ANALYSIS_YEAR, bia=M[,BENTH], cnet=M[,aBENTH], bia_se=S[,BENTH], cnet_se=S[,aBENTH], stringsAsFactors=F)
#	df$ISLAND<-as.character(df$ISLAND)
	df$idx<-seq(1,dim(df)[1])

	prs<-combn(df$idx, m=2)
	pairs_df<-data.frame(a=prs[1,],b=prs[2,],isl1=NA, yr1=0, isl2=NA, yr2=0, n1=0, n2=0, bia_diff=0, bia_diff_se=0, cnet_diff=0, cnet_diff_se=0, bia_mean=0, cnet_mean=0, bia_diff_CI=0, cnet_diff_CI=0, bia_outcome="NS", cnet_outcome="NS", stringsAsFactors=F)

	#Now find all combinations 
	for(i in 1:dim(pairs_df)[1]){
		a<-pairs_df$a[i]
		b<-pairs_df$b[i]
		pairs_df[i,]$isl1<-df[a,]$ISLAND
		pairs_df[i,]$yr1<-df[a,]$ANALYSIS_YEAR
		pairs_df[i,]$isl2<-df[b,]$ISLAND
		pairs_df[i,]$yr2<-df[b,]$ANALYSIS_YEAR
		pairs_df[i,]$bia_diff<-abs(df[a,]$bia-df[b,]$bia)
		pairs_df[i,]$cnet_diff<-abs(df[a,]$cnet-df[b,]$cnet)
		pairs_df[i,]$bia_diff_se<-sqrt((df[a,]$bia_se^2) + (df[b,]$bia_se^2))
		pairs_df[i,]$cnet_diff_se<-sqrt((df[a,]$cnet_se^2) + (df[b,]$cnet_se^2))
		pairs_df[i,]$bia_mean<-mean(df[a,]$bia,df[b,]$bia)
		pairs_df[i,]$cnet_mean<-mean(df[a,]$bia,df[b,]$cnet)

		pairs_df[i,]$n1<-df[a,]$N
		pairs_df[i,]$n2<-df[b,]$N
		
		SE_SCALER<-abs(qt(0.05/2, (df[a,]$N+df[b,]$N)-2)) 
		pairs_df[i,]$cnet_diff_CI<-pairs_df[i,]$cnet_diff_se*SE_SCALER
		pairs_df[i,]$bia_diff_CI<-pairs_df[i,]$bia_diff_se*SE_SCALER

		if(abs(pairs_df[i,]$bia_diff)-pairs_df[i,]$bia_diff_CI>0)
			pairs_df[i,]$bia_outcome<-"SIG"
		if(abs(pairs_df[i,]$cnet_diff)-pairs_df[i,]$cnet_diff_CI>0)
			pairs_df[i,]$cnet_outcome<-"SIG"

	}
	return(pairs_df)
} #CompareCNetBIA

