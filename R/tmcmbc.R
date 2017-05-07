#' A main Function
#'
#' This function allows you to understand articles.
#' @param localfreq localfreq.
#' @param ratio localfreq/globalfreq
#' @keywords tmbc
#' @export 
#' @examples
#' tmcmbc()

tmcmbc <- function(localfreq=0.005,ratio=2){
setwd("D:\\cmbctm\\cmbctm")
cat("welcome to use cmbc semi-cluster textming program.\n")

numOfClus <- readline("please input the number of class you want\n");
numOfClusters <- as.integer(numOfClus)
termSeed4EachCluster <- list()
for(i in 1:numOfClusters)
{	
clusNum = i;
cat("\n");
cat(paste("please input the",clusNum,"class keyWords split it whith blank if more than one word input):",sep="")); 
keyWords <-  readline(paste("please input the",clusNum,"class keyWords split it whith blank if more than one word input):",sep=""));
termSeed4EachCluster[[i]] <- keyWords;
}


originalTextFileName <-  readline("please the text filename:\n");

originalTextFile <- readLines(originalTextFileName);

d.vec2 <- lapply(originalTextFile,removeNumbers)

mixseg <- jiebaR::worker(stop_word='StopWords20160201.txt')

d.vec <- lapply(d.vec2,jiebaR::segment,mixseg)

d.vec1 <- lapply(d.vec,stringr::str_trim)

plain <- as.data.frame(table(unlist(d.vec1)))[-1,]
plain$Var1 <- as.character(plain$Var1)
globalListOfTerms <- NULL
globalListOfTerms <- hash::hash(plain$Var1,plain$Freq)



thisLoopKeyWords <- list()
for(i in 1:length(termSeed4EachCluster)){
  thisLoopKeyWords[[i]] = unlist(stringr::str_split(termSeed4EachCluster[i]," "))
  }



  
continueLoop = TRUE; 
loopNum = 1;
termList4EachCluster <- list()

while(continueLoop) {

cat("==========loop times",loopNum,"==========","\n");
  cc <- list()
  dd <- list()
  ee <- list()
  ff <- list()
  for(i in 1:length(thisLoopKeyWords)){
  clusNum = i;
  cat("----------class NO:",clusNum,"-----------","\n");  
  for(j in 1:length(thisLoopKeyWords[[i]])){
  aa <- thisLoopKeyWords[[i]][j]
  cc[[j]] <- fun.test(aa,originalTextFile = originalTextFile)
  dd[[clusNum]] <- cc
  cat("source keyWords:",unlist(aa),"the article the keyword chuxian:",fun.test(aa,originalTextFile = originalTextFile),"[the number of sample response to  the keyword",ifelse(!is.null(fun.test(aa,originalTextFile = originalTextFile)),length(fun.test(aa,originalTextFile = originalTextFile)),0),"]","\n");  
  ee[[clusNum]] <- unique(unlist(dd))
  }
  ff[[loopNum]] <- ee
  cat("this loop this class's keyWords create xinzeng text (remove dups):",length(unique(unlist(dd))),"\n")
  }
  cat("----------jiezhibenlun,ge lei yang ben shuliang----------","\n");
  for(t in 1:length(dd)){
  cat("leibie",t,":",length(unique(unlist(dd[t]))),"\n");
  }
              stp = TRUE;

			for(x in 1:length(ff[[loopNum]]))
			{	sizeNow = length(ff[[loopNum]][[x]]);
				sizeLast = ifelse(loopNum == 1,0,length(ff[[loopNum-1]][[x]]));
				#sizeLast = length(ff[[loopNum-1]][[x]]);
				if(sizeNow == sizeLast)
				{	
					thisStop = TRUE;
					stp = stp && thisStop;
				}
				else
				{	
					thisStop = FALSE;
					stp = stp && thisStop;
				}
			}
	cat("-----ben lun shi fou wei zhong zhi lun 1-bu zai chan sheng xin de wen ben",stp,"-----","\n")
	newthisLoopKeyWords = list()
	if(stp){
		continueLoop = FALSE
		}else{
			  for(x in 1:length(ff[[loopNum]])){
			  listOfDocumentsInTerms  <- d.vec1[ff[[loopNum]][[x]]]
			  #listOfDocumentsInTerms <- list()
			  #for(i in 1:length(subtermtext))
			  #{	
			  #tLine <- substring(subtermtext[i],2,nchar(subtermtext[i])-1);
			  #thisDocLine <- lapply(subtermtext,str_trim)
			  #listOfDocumentsInTerms <- thisDocLine;
			  #}
			  localplain <- as.data.frame(table(unlist(listOfDocumentsInTerms)))[-1,]
			  localplain$Var1 <- as.character(localplain$Var1)
			  localListOfTerms <- NULL
			  localListOfTerms <- hash::hash(localplain$Var1,localplain$Freq)			  						
			  
			  newClusTermsInThisLoop = vector()
			  
			  for(v in ls(localListOfTerms)){
			  localTermCount <- localListOfTerms[[v]]
			  localTermFrequency = 1.0 * localTermCount / sum(hash::values(localListOfTerms));
			  #cat(v,":",localTermFrequency,"\n")
			  if(localTermFrequency < localfreq){
			  #cat("hi","\n")
			  }else{
			  	  for(w in ls(globalListOfTerms)){
				  if(v == w){
			      globalTermCount <- globalListOfTerms[[w]]
			      globalTermFrequency = 1.0 * globalTermCount / sum(hash::values(globalListOfTerms));
				  theRatio = localTermFrequency/globalTermFrequency;
					if(theRatio < ratio) { 
						#cat("hi","\n")
						}
						else{		
							cat("lei bie",x,"[",v," ",localTermFrequency," ",theRatio,"] ","\n");
							newClusTermsInThisLoop = c(newClusTermsInThisLoop,v);
										}
										
										}else{
										
										}
						
						}		  
			  }
			  
			  }
		    cat("leibie",x,":{xin sheng cheng guan jian ci ge shu wei",length(newClusTermsInThisLoop),"}\n");
			
			newthisLoopKeyWords[[x]] = newClusTermsInThisLoop;
			  }  
		    }
            stp1 = TRUE;

			for(y in 1:length(newthisLoopKeyWords))
			{	#sizeNow1 = length(newthisLoopKeyWords[[y]]);
				#sizeLast = length(ff[[loopNum-1]][[x]]);
				if(length(newthisLoopKeyWords[[y]]) == 0)#warnings
				{	
					thisStop1 = TRUE;
					stp1 = stp1 && thisStop1;
				}
				else
				{	
					thisStop1 = FALSE;
					stp1 = stp1 && thisStop1;
				}
			}
	        cat("-----ben lun shi fou wei zhong zhi lun(2-bu zai chan sheng guan jian ci)",stp1,"-----","\n")
			
			thisLoopKeyWordsdup <- list()
			thisLoopKeyWordsnodup <- list()
				if(stp1){
					continueLoop = FALSE;
			        }else{ 
				        for(z in 1:length(thisLoopKeyWords)){
						ll = thisLoopKeyWords[[z]]
                        nn = newthisLoopKeyWords[[z]]
						thisLoopKeyWordsdup[[z]] <- unique(nn[!nn%in%ll])
						thisLoopKeyWordsnodup[[z]] <- unique(c(ll,nn))

					   }
				   

			    cat("----------benlunjingguozhenglihoude(quchuzhiqianyijingcunzaide) xin sheng guan jian ci---------\n");
				
				
				gg <- list()
								
				for(i in 1:length(thisLoopKeyWordsdup)){
				clusNum = i;
				if(length(thisLoopKeyWordsdup[[i]]) == 0){
				cat("----------leibiehao",clusNum,"-----------","zhenglihoudexinshengguanjianci:","NONE","\n");
				gg[[i]] <- c(unlist(thisLoopKeyWords[i]))
				}else{
				cat("----------leibiehao",clusNum,"-----------","zhenglihoudexinshengguanjianci:","\n");
				for(u in 1:length(thisLoopKeyWordsdup[[i]])){
				    cat(u,thisLoopKeyWordsdup[[i]][u]," ","\n")
					}
				
				
				cat("qing shu ru guan jian ci hao ma:\n");
				cat("---A. all A\n");
				cat("---B.none B\n");
				cat("---C.\n");
				
				termSelection <-  readline("input");
				 if(termSelection == "A"){
				   gg[[i]] <- thisLoopKeyWordsnodup[[i]]
				   }else if(termSelection == "B"){
					         gg[[i]] <- thisLoopKeyWordsdup[[i]]
							}else{
							gg[[i]] <- c(unlist(thisLoopKeyWords[i]),thisLoopKeyWordsdup[[i]][as.integer(stringr::str_split(termSelection," ")[[1]])])
							#cat(thisLoopKeyWords[[i]][as.integer(str_split(termSelection," ")[[1]])],"\n")
							}
							
			    }
			 termList4EachCluster[[loopNum]] <- gg
			}
			
			
			   stp2 = TRUE;
		
		       if(loopNum == 1){
					thisStop2 = FALSE;
					stp2 = stp2 && thisStop2;
				
				}else if(length(setdiff(termList4EachCluster[[loopNum]],termList4EachCluster[[loopNum-1]]))== 0){
					thisStop2 = TRUE;
					stp2 = stp2 && thisStop2;				
				}

	        cat("-----benlunshifouweizhongzhilun(3-rengchanshengguanjiancidanwuxinci?",stp2,"-----","\n")
			

			cat("----------jiezhibenlun,geleibieguanjiancitognjiqignkuang----------");
			
				for(i in 1:length(termList4EachCluster[[loopNum]])){
				clusNum = i;
				cat("----------leibiehao",clusNum,"all seedwords and keyWords",length(termList4EachCluster[[loopNum]][[i]]),"article NO:","\n");
				for(j in 1:length(termList4EachCluster[[loopNum]][[i]])){
                aa <- termList4EachCluster[[loopNum]][[i]][j]
                cat(unlist(fun.test(aa,originalTextFile = originalTextFile)),"\n")  
               }
			    cat("\n");
				}
			    thisLoopKeyWords <- termList4EachCluster[[loopNum]]
				
				
				if(stp2){
				continueLoop = FALSE; 
				}else{	
				loopNum = loopNum + 1;
				}
	}	
}			
}