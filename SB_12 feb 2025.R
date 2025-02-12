Before=c(6.42,6.76,6.56,4.8,8.43,7.49,8.05,5.05,5.77,3.91,6.77,6.44,6.17,7.67,7.34,6.85,5.13,5.73)
After=c(5.83,6.2,5.83,4.27,7.71,7.12,7.25,4.63,5.31,3.7,6.15,5.59,5.56,7.11,6.84,6.4,4.52,5.13)
Data= data.frame(Before,After)
shapiro.test(Data$Before)

#Check for normality
shapiro.test(Data$After)

#Check for correlation
cor.test(Data$Before,Data$After)

#t test
t.test(Data$Before,Data$After,paired = TRUE)

U= Before-After
U
# Check for symmetricity
symmetry.test(U)
