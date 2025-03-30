###############################################################################
# 29-03-25
# Author: Oreng' Purity

###############################################################################
# Library loads
library(tidyverse)

###############################################################################
# loading data
# load the variables separately
# =============================================================================

# 1.Marital status and severity i.e MS.S
MSS <- c( 17,3,70,2,57,1)
MS.S <- matrix(MSS,nrow = 2) # Create a 2x3 matrix
dimnames(MS.S) <- list(c("S", "M"), c("Mild", "Moderate", "Severe")) # Assign dimnames
names(dimnames(MS.S)) <- c("Marital", "severity") # labeling two variables of matrix

# compute the proportion of each element in the matrix,
# based on the sum of all elements in the matrix.
# It normalizes the values of ms.s by dividing each element by the total sum of the matrix, 
# making the sum of all elements in the matrix equal to 1.
prop.table(MS.S)
prop.table(MS.S, margin = 1)
prop.table(MS.S, margin = 2)

# Running chi Square test
Xsq <- chisq.test(MS.S, correct = T)
Xsq

# Running odds ratio to further test the significance
Xsq$expected
Xsq$residuals

# 2. Age and severity i.e., A.S
AS <- c(4,11,5,30,32,10,26,29,3)
A.S <- matrix(AS,nrow = 3) # Create a 3x3 matrix
dimnames(A.S) <- list(c("<20", "20-23", "24>"), c("Mild", "Moderate", "Severe")) # Assign dimnames
names(dimnames(A.S)) <- c("Age", "severity") # labeling two variables of matrix

# Running chi Square test
Xsq <- chisq.test(A.S, correct = T)
Xsq

# Running odds ratio to further test the significance
Xsq$expected
Xsq$residuals

# 3. Employment status and severity i.e., E.S
ES <- c(12,17,3,53,2,64)
E.S <- matrix(ES,nrow = 2) # Create a 2x3 matrix
dimnames(E.S) <- list(c("Yes", "No"), c("Mild", "Moderate", "Severe")) # Assign dimnames
names(dimnames(E.S)) <- c("Employment", "severity") # labeling two variables of matrix

# Running chi Square test
Xsq <- chisq.test(E.S, correct = T)
Xsq

# Running odds ratio to further test the significance
Xsq$expected
Xsq$residuals

# 4. Geographical location and severity i.e., L.S
LS <- c(50,9,20,48,13,8,5,6,2)
L.S <- matrix(LS,nrow = 3) # Create a 3x3 matrix
dimnames(L.S) <- list(c("urban", "periurban", "rural"), c("Mild", "Moderate", "Severe")) # Assign dimnames
names(dimnames(L.S)) <- c("location", "severity") # labeling two variables of matrix

# Running chi Square test
Xsq <- chisq.test(L.S, correct = T)
Xsq

# Running odds ratio to further test the significance
Xsq$expected
Xsq$residuals

# 5. Academic Achievements and severity i.e., AC.S
ACS <- c(16,1,35,4,16,37)
AC.S <- matrix(ACS,nrow = 2) # Create a 2x3 matrix
dimnames(AC.S) <- list(c("Yes", "No"), c("Mild", "Moderate", "Severe")) # Assign dimnames
names(dimnames(AC.S)) <- c("Academics", "severity") # labeling two variables of matrix

# Running chi Square test
Xsq <- chisq.test(AC.S, correct = T)
Xsq

# Running odds ratio to further test the significance
Xsq$expected
Xsq$residuals

# 6. Frequency of impact on academic achievements and severity i.e., F.S
FS <- c(12,5,3,23,41,8,8,32,18)
F.S <- matrix(FS,nrow = 3) # Create a 3x3 matrix
dimnames(F.S) <- list(c("N/A", "sometimes", "always"), c("Mild", "Moderate", "Severe")) # Assign dimnames
names(dimnames(F.S)) <- c("frequency.of.impact", "severity") # labeling two variables of matrix

# Running chi Square test
Xsq <- chisq.test(F.S, correct = T)
Xsq

# Running odds ratio to further test the significance
Xsq$expected
Xsq$residuals

# ==============================================================================

## Chi square analysis of supplementary data
# A. Lifestyle characteristics and dysmenorrhea severity

# 1. Smoking and severity i.e., S.S
SS <- c(18,2,67,5,55,3)
S.S <- matrix(SS,nrow = 2) # Create a 2x3 matrix
dimnames(S.S) <- list(c("Yes", "No"), c("Mild", "Moderate", "Severe")) # Assign dimnames
names(dimnames(S.S)) <- c("Smoking", "severity") # labeling two variables of matrix

# Running chi Square test
Xsq <- chisq.test(S.S, correct = T)
Xsq

# Running odds ratio to further test the significance
Xsq$expected
Xsq$residuals

# 2. Alcohol consumption and severity i.e.,AL.S
ALS <- c(14,6,48,24,38,20)
AL.S <- matrix(ALS,nrow = 2) # Create a 2x3 matrix
dimnames(AL.S) <- list(c("Yes", "No"), c("Mild", "Moderate", "Severe")) # Assign dimnames
names(dimnames(AL.S)) <- c("Alcohol", "severity") # labeling two variables of matrix

# Running chi Square test
Xsq <- chisq.test(AL.S, correct = T)
Xsq

# Running odds ratio to further test the significance
Xsq$expected
Xsq$residuals

# 3. Daily activities and dysmenorrhea severity i.e., DA.S
DAS <- c(16,4,27,45,5,53)
DA.S <- matrix(DAS,nrow = 2) # Create a 2x3 matrix
dimnames(DA.S) <- list(c("Yes", "No"), c("Mild", "Moderate", "Severe")) # Assign dimnames
names(dimnames(DA.S)) <- c("Dalily activities", "severity") # labeling two variables of matrix

# Running chi Square test
Xsq <- chisq.test(DA.S, correct = T)
Xsq

# Running odds ratio to further test the significance
Xsq$expected
Xsq$residuals

# B.Reproductive & Menstrual features and their relationship with dysmenorrhea severity

# 1. Age at menarche and dysmenorrhea severity i.e AM.S
AMS <- c(2,18,0,5,66,1,10,48,0)
AM.S <- matrix(AMS,nrow = 3) # Create a 3x3 matrix
dimnames(AM.S) <- list(c("<12", "13-19", ">20"), c("Mild", "Moderate", "Severe")) # Assign dimnames
names(dimnames(AM.S)) <- c("menarche", "severity") # labeling two variables of matrix

# Running chi Square test
Xsq <- chisq.test(AM.S, correct = T)
Xsq

# Running odds ratio to further test the significance
Xsq$expected
Xsq$residuals

# 2. Menstrual flow pattern and dysmenorrhrea severity i.e., MP.S
MPS <- c(14,4,41,28,34,22)
MP.S <- matrix(MPS,nrow = 2) # Create a 2x3 matrix
dimnames(MP.S) <- list(c("Regular", "Irregular"), c("Mild", "Moderate", "Severe")) # Assign dimnames
names(dimnames(MP.S)) <- c("menstrual pattern", "severity") # labeling two variables of matrix

# Running chi Square test
Xsq <- chisq.test(MP.S, correct = T)
Xsq

# Running odds ratio to further test the significance
Xsq$expected
Xsq$residuals

# 3. Bleeding intensity and dysmenorrhea severity i.e., BI.S
BIS <- c(19,0,1,55,2,15,33,8,17)
BI.S <- matrix(BIS,nrow = 3) # Create a 3x3 matrix
dimnames(BI.S) <- list(c("light", "moderate", "heavy"), c("Mild", "Moderate", "Severe")) # Assign dimnames
names(dimnames(BI.S)) <- c("bleeding intensity", "severity") # labeling two variables of matrix

# Running chi Square test
Xsq <- chisq.test(BI.S, correct = T)
Xsq

# Running odds ratio to further test the significance
Xsq$expected
Xsq$residuals

# 4. Disruptive symptoms and dysmenorrhea severity i.e., DS.S
DSS <- c(5,15,4,68,0,58)
DS.S <- matrix(DSS,nrow = 2) # Create a 2x3 matrix
dimnames(DS.S) <- list(c("Yes", "No"), c("Mild", "Moderate", "Severe")) # Assign dimnames
names(dimnames(DS.S)) <- c("Disruptive symptoms", "severity") # labeling two variables of matrix

# Running chi Square test
Xsq <- chisq.test(DS.S, correct = T)
Xsq

# Running odds ratio to further test the significance
Xsq$expected
Xsq$residuals

# 5. Parity and dysmenorrhea severity i.e. P.S
PS <- c(20,0,63,9,53,5)
P.S <- matrix(PS,nrow = 2) # Create a 2x3 matrix
dimnames(P.S) <- list(c("Yes", "No"), c("Mild", "Moderate", "Severe")) # Assign dimnames
names(dimnames(P.S)) <- c("parity", "severity") # labeling two variables of matrix

# Running chi Square test
Xsq <- chisq.test(P.S, correct = T)
Xsq

# Running odds ratio to further test the significance
Xsq$expected
Xsq$residuals

# 6. Contraceptive use and dysmenorrhea severity i.e., C.S
CS <- c(15,5,52,20,40,18)
C.S <- matrix(CS,nrow = 2) # Create a 2x3 matrix
dimnames(C.S) <- list(c("Yes", "No"), c("Mild", "Moderate", "Severe")) # Assign dimnames
names(dimnames(C.S)) <- c("contraceptive", "severity") # labeling two variables of matrix

# Running chi Square test
Xsq <- chisq.test(C.S, correct = T)
Xsq

# Running odds ratio to further test the significance
Xsq$expected
Xsq$residuals