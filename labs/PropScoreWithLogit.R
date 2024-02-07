#########################################################################################
# This code replicates the example provided by Olmos and Govindasamy (2015) on
# propensity score matching. 
# See https://journals.sfu.ca/jmde/index.php/jmde_1/article/view/431.
# For a short description of the dataset and original sources see
# https://search.r-project.org/CRAN/refmans/designmatch/html/lalonde.html
# Created by: Thomas Heckelei
# Date: January 24, 2024
#######################################################################################

# Clear environment
rm(list = ls())
# Disable scientific notation
options(scipen=999)

# Load libraries
# install.packages("MatchIt")
# install.packages("RItools")
# install.packages("Hmisc")
library("MatchIt") # For matching
library("RItools")
library("Hmisc")
library("tidyr")
library("dplyr")

# load data
data("lalonde")
N=length(lalonde$treat)
NT=sum(lalonde$treat)
NC=N-NT

# ################
# Analysis follows
# ################

# --- Computing	indices	of covariate	imbalance	before	matching
# you will see that the tests reject the balanced distribion across all 
# variables simultaneously (chi-square test) and for most variables separately
# (std.diffs test) 
xBalance(treat	~ age	+ educ + nodegree	+ re74	+ re75,	data=lalonde,
         report=c("std.diffs","chisquare.test", "p.values"))

# --- Treatment effect estimation using mean difference between treated and
# non-treated --> we get a negartive income effect of training program
ATE_naive = (sum(lalonde$treat*lalonde$re78)/NT 
            - sum((1-lalonde$treat)*lalonde$re78)/NC)
ATE_naive

# You get the same if you run a simple regression on treatment without covariates
reg	<-lm(re78	~ treat,
data = lalonde)
summary(reg)

# --- Treatment effect estimation of regression coefficient of treatment 
# without matching but with controls. Now estimated treatment effect is positive 
# Why should you not trust this estimate either?
reg	<-lm(re78	~ treat	+ age	+ educ	+ nodegree	+ re74	+ re75	+ married,
        data = lalonde)
summary(reg)

# --- Estimation of propensity	scores with the glm function
# choosing family = "binomial" will pick a "logit" (or logistic) model
# see https://www.datacamp.com/tutorial/logistic-regression-R for a quick
# intro to logistic regression
ps	<- glm(treat	~ age	+ educ	+ nodegree	+ re74	+ re75,
        data	=lalonde,	family	= binomial())
summary(ps)

# --- Attach	the	predicted	propensity	score	to	the	datafile
# Note: the predictions from a logit model are probabilities
# In this case probabilities to be treated given the covariates chosen, 
# i.e., the propensity scores
lalonde$psvalue	<- predict(ps,	type	= "response")

# --- Drawing back to back histograms for propensity scores for treated and 
# non-treated before matching
histbackback(split(lalonde$psvalue,	lalonde$treat),	main= 
  "Propensity	score	before	matching",	xlab=c("control",	"treatment"))

# --- Match	using	nearest-neighbor approach, i.e. treated units are assigned the 
# non-treated unit with the closest propensity score as match 
# You will see that matches are found for all 185 treated units (guaranteed
# with the nearest-neighbor approach)
m.nn	<- matchit(treat	~ age	+ educ	+ nodegree	+ re74	+ re75,
        data	=lalonde,	method= "nearest",	ratio	= 1)
summary(m.nn)
match.data	= match.data(m.nn)

# --- Computing	indices	of covariate	imbalance	after	matching
# same command as above but using the matched data now
# what you will see is that matching by propensity scores balances
# the covariates between treated and non-treated that were used in the
# estimation of the propensity scores
xBalance(treat	~ age	+ educ + nodegree	+ re74	+ re75,	data=match.data,
         report=c("std.diffs","chisquare.test", "p.values"))

# Drawing back to back histograms for propensity scores for treated and 
# non-treated after matching
histbackback(split(match.data$psvalue,	match.data$treat),	main= "Propensity
        score	after	matching",	xlab=c("control",	"treatment"))

# --- Treatment effect estimation using average outcome difference of matched pairs
# do this with for loop but likely more elegant approaches are possible
# note that equal entries in "subclass" of match.data defines the match
# this loop sums up the differences of outcomes between matches and then 
# divides by NT to derive the mean difference as the ATE estimate

sumdiff<-0
for (i in 1:NT){
  temp <- match.data[is.element(match.data$subclass, paste0(i)),]
  sumdiff <- sumdiff + temp[1,"re78"]-temp[2,"re78"] 
}
sumdiff
ATE_m_nn = 1/NT * sumdiff
ATE_m_nn

# alternative in tidyverse
sumdiff_data<-match.data%>%
  group_by(subclass)%>%
  mutate(diff=re78[treat==1]-re78[treat==0])

sumdiff<-sum(sumdiff_data$diff)/2
ATE_m_nn = 1/NT * sumdiff
ATE_m_nn

# --- estimate treatment effect with IPW estimator
# DOES NOT YET WORK
# lalonde$npsvalue <- lalonde$psvalue / sum(lalonde$psvalue)
# ATE_IPW <- (1/(sum(lalonde$psvalue*lalonde$treat))
#           *sum(lalonde$treat*lalonde$re78/lalonde$psvalue)
#           - 1/(sum((1-lalonde$psvalue)*(1-lalonde$treat)))
#             * sum((1-lalonde$treat)*(lalonde$re78/(1-lalonde$psvalue))))
 
 ATE_IPW <- (1/sum(lalonde$psvalue) *
            sum((lalonde$treat*lalonde$re78/lalonde$psvalue)
            - (1-lalonde$treat)*lalonde$re78/(1-lalonde$psvalue)))
ATE_IPW

# --- estimate treatment effect with Weighted Least Squares estimator
# DOES NOT YET WORK
lalonde$wgt = (lalonde$treat/lalonde$psvalue + 
              (1-lalonde$treat)/(1-lalonde$psvalue))
# reg_wls	<-lm(re78	~ treat	+ age	+ educ	+ nodegree	+ re74	+ re75	+ married,
#          data = lalonde, weights = wgt)
