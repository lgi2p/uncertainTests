########################################
# Functions library for uncertain tests#
########################################

######################################################################################
p_box <- function(data, data2, data3, data4) {
  
  # C'est une fonction qui nous permet de tracer les F_A(x)
  
  result <- sort(c(data,data2,data3,data4))
  result <- cbind(result, rep(0,length(result)))
  n = length(data)
  current_value <- 0
  last_value <- 0
  for (k in 1:nrow(result)) {
    if (result[k,1] %in% data && result[k,1] != last_value) {
      current_value = current_value + 1
    }
    result[k,2] <- current_value/n
    last_value <- result[k,1]
  }
  return(result)
}
desterckeStraussTest <- function(x, y){
  x <- t(apply(x,1,sort))
  y <- t(apply(y,1,sort))
  
  sup_x <- sort(x[,1])
  inf_x <- sort(x[,2])
  sup_y <- sort(y[,1])
  inf_y <- sort(y[,2])
  
  f_sup_x <- p_box(sup_x,inf_y,sup_y,inf_x)
  f_inf_x <- p_box(inf_y,sup_x,sup_y,inf_x)
  f_sup_y <- p_box(sup_y,inf_x,sup_x,inf_y)
  f_inf_y <- p_box(inf_x,sup_y,sup_x,inf_y)
  
  high <- max(max(abs(f_sup_x[,2] - f_inf_y[,2])),max(abs(f_inf_x[,2] - 
                                                            f_sup_y[,2])))
  D <- c()
  for (k in 1:nrow(f_sup_x)) {
    if ((f_inf_x[k,2] > f_inf_y[k,2] && f_sup_x[k,2] > f_inf_y[k,2]) || 
        (f_inf_x[k,2] < f_inf_y[k,2] && f_inf_x[k,2] < f_sup_y[k,2])) {
      
      D <- c(D,0)
    } else {
      D <- 
        c(D,min(abs(f_inf_x[k,2]-f_sup_y[k,2]),abs(f_inf_y[k,2]-f_sup_x[k,2])))
    }
  }
  low <- max(D)
  return(list("low" = low, "high" = high))
}
######################################################################################
perolatTest <- function(dataframeintervalle1, dataframeintervalle2){
  
  n=length(dataframeintervalle1[,1])
  m=length(dataframeintervalle2[,1])
  l=n+m
  wh=0
  ph=1
  zh=c(dataframeintervalle1[,2],dataframeintervalle2[,1])
  zh=sort(zh)
  dataframeintervalle2[,1] = sort(dataframeintervalle2[,1])
  
  for(i in 1:l){
    if (zh[i]==dataframeintervalle2[,1][ph] && ph<=length(dataframeintervalle2[,1])){
      ph=ph+1
    } else{
      wh=wh+i
    }
  }
  
  z=c(dataframeintervalle1[,1],dataframeintervalle2[,2])
  z=sort(z)
  dataframeintervalle1[,1]=sort(dataframeintervalle1[,1])
  w=0
  q=1
  for(j in 1:l){
    if (dataframeintervalle1[,1][q]==z[j] && q<=length(dataframeintervalle1[,1])){
      q=q+1
      w=w+j
    }
  }
  
  nu=n*(n+m+1)/2
  s2=n*m*(n+m+1)/12
  tbas=(w-nu)/sqrt(s2)
  thaut=(wh-nu)/sqrt(s2)
  val=max(thaut,-tbas)
  val2=max(0,-thaut,tbas)
  pbas=2*(1-pnorm(val))
  phaut=2*(1-pnorm(val2))
  return(list(low=pbas,high=phaut))
}
######################################################################################
##test Wilkoxon Signe Rank Trapeze Vaidyanathan
#on calcul la difference entre deux trapèzes
difference<- function(l,m){
  d=c((l[,1]-m[,4]),(l[,2]-m[,3]),(l[,3]-m[,2]),(l[,4]-m[,1]))
  return(d)
}
vaidyanathanTest<- function(dataframeTrapeze1,dataframeTrapeze2){
    
  #on calcul la moyenne pour pouvoir ordonner les différences
  diff=c()
  moyenne=c()
  for (i in 1:length(dataframeTrapeze1[,1])){
    diff=cbind(diff,difference(dataframeTrapeze1[i,],dataframeTrapeze2[i,]))#on calcule la différence entre deux trapèze
  }
  diff=t(diff)
  
  for (i in 1:length(dataframeTrapeze1[,1])){
    moyenne=c(moyenne,mean(diff[i,])) #on classe les différences en calculant la moyenne
  }
  rang=c()
  mo=sort(moyenne)
  for (i in 1:length(moyenne)){
    rang=c(rang,which(mo==moyenne[i]))
  }
  tableau=cbind(diff,moyenne,rang)
  
  sm=sum(rang[moyenne<0])
  sp=sum(rang[moyenne>0])
  s=min(sp,sm)
  n=length(moyenne)
  sigma=n*(n+1)*(2*n+1)/24
  pvalue= pnorm((s-n*(n+1)/4)/sqrt(sigma))
  return(pvalue)
}
######################################################################################
##wilkoxon Signed Test Hesamian Chachi
#On calcul à quel point a est supérieur à b
superieur<- function(a,b){
  if(b[3]<=a[1]){
    return(1)
  }
  else if (b[3]>a[1]){
    return((b[3]-2*b[2]+((b[3]*(a[2]-a[1])+a[1]*(b[3]-b[2]))/(a[2]-a[1]+b[3]-b[2])))/(2*(b[3]-b[2])))
  }
}

#calcul la différence de 2 triangles
differencet<- function(l,m){
  d=c((l[1]-m[3]),(l[2]-m[2]),(l[3]-m[1]))
  return(d)
}
hesaminanChachiTest <- function(u,v){
    
  supe=c()
  
  for(i in 1:length(u[,1])){
    supe=c(supe,superieur(u[i,],v[i,]))#on met toutes les valeurs de supériorité pour chaque paire de valeur
  }
  
  g=c()
  listealpha=seq(0.5,1,0.001)#on créer plusieurs valeurs de alpha entre 
  # 0.5 et 1
  for(i in 1:length(listealpha)){
    tr=supe[supe>listealpha[i]]#on prend toutes les valeur de supériorité qui sont supérieur à alpha
    g=c(g,length(tr)) #on ajoute dans une liste le nombre d'élément supérieur à ce alpha
  }
  f0=min(g)
  f1=max(g)
  pbas=0
  #on calcul pbas et phaut avec le binome
  for (i in 0:f0){
    pbas=pbas+choose(length(u[,1]),i)*(0.5)**(length(u[,1]))
  }
  phaut=0
  for (i in 0:f1){
    phaut=phaut+choose(length(u[,1]),i)*(0.5)**(length(u[,1]))
  }
  return(list(low=pbas,high=phaut))
}
############################################################################################################################
# Taheri-Hesamian:
# fonction qui calcul l'alphacut pour des données trapezoidale
alphacut<- function(t,alpha){
  return(c(((t[2]-t[1])*alpha+t[1]),(t[4]-(t[4]-t[3])*alpha)))
}
#on calcul la difference entre le minimum de l'alphacut d'un triangle et 
#le maximum de l'alphacut d'un autre triangle
gldiff<- function(t1,t2,alpha){
  return(min(as.numeric(alphacut(t1,alpha)))-max(as.numeric(alphacut(t2,alpha))))
}
#on calcul la difference entre le minimum de l'alphacut d'un triangle et 
#le maximum de l'alphacut d'un autre triangle mais dans l'autre sens que 
#gldiff
gudiff<- function(t1,t2,alpha){
  return(max(as.numeric(alphacut(t1,alpha))-min(as.numeric(alphacut(t2,alpha)))))
}
gl<- function(liste1,liste2,alpha){
  listegldiff=c()
  for (i in 1:length(liste1[,1])){#on calcul la difference pour deux listes de triangle
    listegldiff=c(listegldiff,gldiff(liste1[i,],liste2[i,],alpha))
  }
  rang=c()
  mo=sort(listegldiff) #on tri les différence
  for (i in 1:length(listegldiff)){
    rang=c(rang,which(mo==listegldiff[i]))#on donne le rang de chaque différence
  }
  tableau=cbind(listegldiff,rang)
  sp=sum(rang[listegldiff>0]) #on fait la somme de tous les rangs dont la différence est positive
  return(sp)
}
gu<- function(liste1,liste2,alpha){ #on fait pareil que gl mais avec la fonction gudiff
  listegudiff=c()
  for (i in 1:length(liste1[,1])){
    listegudiff=c(listegudiff,gudiff(liste1[i,],liste2[i,],alpha))
  }
  rang=c()
  mo=sort(listegudiff)
  for (i in 1:length(listegudiff)){
    rang=c(rang,which(mo==listegudiff[i]))
  }
  tableau=cbind(listegudiff,rang)
  sp=sum(rang[listegudiff>0])
  return(sp)
}
Tl<- function(liste1,liste2,alpha){
  listebeta=seq(alpha,1,0.01) #pour tous les betas superieur à alpha
  listegl=c()
  listegu=c()
  for(i in 1:length(listebeta)){
    listegl=c(listegl,gl(liste1,liste2,listebeta[i]))
    listegu=c(listegu,gu(liste1,liste2,listebeta[i]))
  }
  return(min(min(listegl),min(listegu)))
}
Tu<- function(liste1,liste2,alpha){
  listebeta=seq(alpha,1,0.01)# pour tous les betas superieur à alpha
  listegl=c()
  listegu=c()
  for(i in 1:length(listebeta)){
    listegl=c(listegl,gl(liste1,liste2,listebeta[i]))
    listegu=c(listegu,gu(liste1,liste2,listebeta[i]))
  }
  return( c(listegl,listegu))
}
nutplus<- function(liste1,liste2,t){
  listealpha=seq(0,1,0.01)
  listenu=c()
  for (i in 1:length(listealpha)){
    a=seq(Tl(liste1,liste2,listealpha[i]), Tu(liste1,liste2,listealpha[i]))
    if(min(a)<=t && t<=maa){
      listenu=c(listenu,listealpha[i])
    }
  }
  if(is.null(listenu)){#car max d'une liste vide donne moins l'infini
    return(0)
  }
  else{
    return(malistenu)
  }
}
calaculnutpluspourtousT<- function(liste1,liste2){
  listenutplus=c()
  n=length(liste1[,1])
  listet=seq(1,n*(n+1)/2)
  t=c()
  for(i in 1:length(listet)){
    listenutplus=c(listenutplus,nutplus(liste1,liste2,listet[i]))
    t=c(t,i)
  }
  return(cbind(listenutplus,t))
}
# nutplus(trapezematelas1D0,trapezematelas2D0,1485)
# nutplus(trapezematelas1D30,trapezematelas2D30,1431)
cdelta<- function(liste1,liste2,delta){
  #calcul de Cdelta
  n=length(liste1[,1])
  #on observe si liste1-liste2<0
  t=0
  z=(t-(n*(n+1)/4)-0.5)/(sqrt((2*n*(n+1)*(2*n+1))/48))
  listet=seq(1,n*(n+1)/2)
  max=0
  for (i in  1:length(listet)){
    t=listet[i]
    if(pnorm(z)<delta){
      max=t
    }
  }
  return(max)
}








