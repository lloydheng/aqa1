cd "/Users/Lloyd/Desktop/AQR/Greenberg/Homework"

log using LHeng_Homework_7.smcl, replace

// Creating frames for each question
	frame create Q1
	frame create Q2
	frame create Q3
	
***************Q1***************
	
// Load Q1 frame
	frame change Q1
	
// Load lowbirthweight.dta
	use http://www.stata-press.com/data/r13/gsem_lbw
	describe
		/* For our independent variables,
			1. smoke, ht are binary
			2. race is categorical
			3. age, lwt, ptl are interval-level
				*/
	
// Running a logistic regression
	logit low age lwt i.race smoke ptl ht
		// The show a pseudo R-squared of 0.13, but I turn to other measures of fit
		
		
// Checks for fit and class tables

	// Using estat gof
		estat gof
			// Ok fit (p = 0.31)
			
	// Because of the high number of covariate patterns (181 patterns in 189 observations), I also attempt the Homer-Lemeshow test with 10 groups
		estat gof, group(10) table
			// The test still suggests ok fit (p = 0.24)
			
			
// Creating a classification table
	estat class
		/* Correctly classifies 73% of the time. Great specificity (91.54%), meaning the model is good at identifying those of acceptable birth weight (or true negatives). It does a poorer job identifying positives, those with low birth weight, with a sensitivity of 32.2% - high false positive rate. 
			*/
		
***************Q2***************
	
// Load Q2 frame
	frame change Q2
	
// Load voter.dta
	use voter.dta
	describe
	
// Recoding sex as binary (1 = male)
	replace sex = 0 if sex == 2
		
		// To check sex has become binary
		tab sex
		tab sex, nol
	
// Running an ordinal logistic regression
	ologit degree sex age, or
	
	/* Determinants of educational attainment:
		1. Older people have poorer educational attainment (p < 0.0001)
		2. Men and women do not differ 
			*/
	
	// Getting an asymptotic correlation matrix
		estat vce, corr
		
	// Testing the parallel regression assumption	
		brant, detail
			// The brant test suggests that the parallel assumption has been violated for age (p < 0.0001) but not sex (p = 0.078)
		oparallel
			// The oparallel procedure shows that other tests also suggest the parallel assumption has been violated. All in all, this suggests a mlogit might work better.

			
// Running a multinomial logistic regression
	mlogit degree sex age, base(0) rrr
	
	// Predicted probability of being in each category
	predict pr0 pr1 pr2 pr3 pr4, pr 

	// Creating a classification table by hand (credits to https://www.statalist.org/forums/forum/general-stata-discussion/general/1449677-multinominal-model-command-for-correctly-predicted)
	gen byte predcat = .
	gen maxp = -1
	forval i = 0/4 {
	replace predcat = `i' if (pr`i' > maxp)
	replace maxp = pr`i' if (pr`i' > maxp)
	}
	
	// Compare observed and predicted.
	tab degree predcat
		// From this table, we see that the model does a great job predicting high school graduates, only misallocating 17 of the 947 observations. However, it does a terrible job predicting the other attainment categories. All attainment levels above high schools (junior college, bachelor and graduate degree) are predicted correctly 0% of the time. For the lowest attainment category (< high school), the model is also bad at predicting it - only 15 of the 189 observations are predicted correctly into this category.
		
		
/* Lessons from the mlogit versus the ologit:
	1. From the ologit, we learn that the parallel lines assumption is violated, such that age and sex may have differential effects for different attainment categories.
	2. However, the mlogit results only make sense in relation to a base group, such that age or sex may or may not matter, and potentially in different directions, RELATIVE to the chosen base. In other words, the results change depending on the basegroup selected in line 78.
		*/

	
***************Q3***************
	
// Load Q3 frame
	frame change Q3
	
// Load crime1.dta
	use crime1.dta
	describe

// Looking at the variables more closely
	tab nfarr86	// nfarr86 has a ton of zeroes, suggesting possible zero-inflation
	tab qemp // ditto
	tab inc86 // ditto
	tab durat // ditto
	
// I first attempt to use countfit as a catch-all, but it does not work (fails to converge)
		/*
	countfit nfarr86 qemp inc86 durat black hispan, inflate(inc86)
		 */
	
	// I turn to check if variance = mean
	tabstat nfarr86, stat(mean v)
		// nfarr86 is not overdispersed, thus a zip (versus a zinb) should suffice
		
		
// Running a zero-inflated poisson regression. I choose to inflate by income, with the assumption that those with zero income in the entire year may possibly already be in prison and thus incapable of being arrested while in prison
	zip nfarr86 qemp86 durat black hispan, inflate(inc86) irr
		// The output confirms my suspicion - inc86 is a likely factor inflating zeroes in this model (p < 0.0001). On top of that, increasing quarters employed is associated with decreased odds of felony arrests (IRR = 0.84; p < 0.0001). Being black or hispanic is associated with increased odds of arrests (IRR = 2.12; p < 0.0001, and IRR = 1.45; p = 0.001 respectively). Duration of recent unemployment is not significantly associated with felony arrests (p = 0.911).
		
		
// Looking at zip model fit. Since estat gof does not work for zip, I turn to fitstat as a rough measure of fitness.
	fitstat
		// The model has a McFadden R-squared of 0.054
	estat ic
		// AIC = 3115, BIC = 3156; to compare with poisson model
	
	
// Comparing to just a poisson regression
	poisson nfarr86 qemp durat black hispan inc86
	estat gof
		// The deviance gof tells us that we cannot reject the hypothesis that the data is poisson distributed (thus a poisson model suffices). The Pearson gof suggests a good fit.
	estat ic
		/* AIC = 3146, BIC = 3182. Information criterion improves in the zero-inflated poisson, but only slightly, suggesting rather surprisingly that it does not help much to think of it as a zip versus a standard poisson. Regardless, given the improvement in AIC anyway, I stick to the zip interpretation of results outlined earlier. Notably, the standard poisson differs from the zip in one predictor - it fails to find statistical significant for quarters employed.
			*/

			
// Checking for possible heterogeneity using Heckman

	// I first generate a variable such that zeroes in nfarr86 = missing
		gen hecknfarr86 = nfarr86
		replace hecknfarr86 = . if nfarr86 == 0
		
	// I then run a heckman procedure to check for selection bias. I use inc86, qemp86, black, and hispan variables as possible causes of selection.
	heckman hecknfarr86 qemp durat black hispan inc86, select(inc86 qemp86 black hispan) twostep
		// The heckman procedure (rho = 1) suggests selection on the basis of income and race.


// Exporting log as pdf
translate LHeng_Homework_7.smcl LHeng_Homework_7.pdf, replace
