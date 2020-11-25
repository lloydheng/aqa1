log using homework6.smcl, replace

* 1b. Constructing the correlation matrix as C, then listing it to check
mat input C = (1, 0.64, 0.75, 0.59 \ 0.64, 1, 0.92, 0.89 \ 0.75, 0.92, 1, 0.87 \ 0.59, 0.89, 0.87, 1)
mat list C


*Creating artificial data using correlation matrix C, then running corr with all variables to check
corr2data i s p r, n(185) corr(C)
corr i s p r

*Estimating equations from part 1a
pathreg (p i s)(r i s p)


* 2a. 

* Constructing the correlation matrix K, then listing it to check

mat input K = (1, 0.288, 0.194 \ 0.288, 1, 0.435 \ 0.194, 0.435, 1)
mat list K

* Creating artificial data using correlation matrix K, then running corr with all variables to check

corr2data SES IQ ACACH, n(100) corr(K) clear
corr SES IQ ACACH

* Estimating path coefficients using artificial data
pathreg (ACACH IQ SES)(IQ SES)

translate homework6.smcl LHeng_Homework_6_Stata.pdf, replace
