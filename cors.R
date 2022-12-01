anx = foreign::read.spss("/Users/stephangoerigk/Desktop/Universität/Fresenius/Übung zur Computergestützten Datenanalyse/SS2017/Field_DataSets/dsus4data/Exam Anxiety.sav", to.data.frame = T)
anx = BBmisc::dropNamed(anx, drop = "Code")

levels(anx$Gender) = c("Female", "Male")

anx$Exam[anx$Gender == "Male" & anx$Exam > 90] =  anx$Anxiety[anx$Gender == "Male" & anx$Exam > 90] + 3


ggplot(anx, aes(x=Anxiety, y = Exam)) +
  geom_point() +
  geom_smooth(method="lm")

corrplot(cor(anx[,1:3]))
shapiro.test(anx$Exam)

library(kableExtra)

var1 = rnorm(1000000, 100, 15)
var2 = faux::rnorm_pre(var1, mu = 10, sd = 5, r = sample(0.5, 1))
population = data.frame(var1, var2)

cor(population$var1, population$var2)

stichprobe = population[sample(1:1000000, 30),]

ggplot(population, aes(var1, var2)) +
  geom_point()

ggplot(population[sample(1:1000000, 30),], aes(var1, var2)) +
  geom_point()


summary(correlation::correlation(anx[,1:3]))

kable_classic(kable(summary(correlation::correlation(anx[,1:3]))))

ggplot(anx, aes(x=Anxiety, y = Exam, colour = Gender)) +
  geom_point() +
  geom_smooth(method="lm")

a = lm(Exam ~ Anxiety, data = anx)

corstars <-function(x, method=c("pearson", "spearman"), removeTriangle=c("upper", "lower"),
                    result=c("none", "html", "latex")){
  #Compute correlation matrix
  require(Hmisc)
  x <- as.matrix(x)
  correlation_matrix<-rcorr(x, type=method[1])
  R <- correlation_matrix$r # Matrix of correlation coeficients
  p <- correlation_matrix$P # Matrix of p-value

  ## Define notions for significance levels; spacing is important.
  mystars <- ifelse(p < .001, "*** ", ifelse(p < .01, "**  ", ifelse(p < .05, "*   ", "    ")))

  ## trunctuate the correlation matrix to two decimal
  R <- format(round(cbind(rep(-1.11, ncol(x)), R), 2))[,-1]

  ## build a new matrix that includes the correlations with their apropriate stars
  Rnew <- matrix(paste(R, mystars, sep=""), ncol=ncol(x))
  diag(Rnew) <- paste(diag(R), " ", sep="")
  rownames(Rnew) <- colnames(x)
  colnames(Rnew) <- paste(colnames(x), "", sep="")

  ## remove upper triangle of correlation matrix
  if(removeTriangle[1]=="upper"){
    Rnew <- as.matrix(Rnew)
    Rnew[upper.tri(Rnew, diag = TRUE)] <- ""
    Rnew <- as.data.frame(Rnew)
  }

  ## remove lower triangle of correlation matrix
  else if(removeTriangle[1]=="lower"){
    Rnew <- as.matrix(Rnew)
    Rnew[lower.tri(Rnew, diag = TRUE)] <- ""
    Rnew <- as.data.frame(Rnew)
  }

  ## remove last column and return the correlation matrix
  Rnew <- cbind(Rnew[1:length(Rnew)-1])
  if (result[1]=="none") return(Rnew)
  else{
    if(result[1]=="html") print(xtable(Rnew), type="html")
    else print(xtable(Rnew), type="latex")
  }
}



