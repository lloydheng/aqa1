cd "/Users/Lloyd/Desktop/AQR/Greenberg/Homework"

log using diaghw.smcl, replace

// Creating dataframes
	frame create Q1
	frame create Q2
	
// Load dataframe	
	frame change Q1

// Load dataset
	bcuse hprice2a

	
// 1a. Plotting distribution for price, we see that price is not normally distributed and has a considerable right tail
	hist price, normal
	
	// To check that it truly defies normality, I run a sktest. We reject the null for normality (i.e. price is not normally distributed)
	sktest price

	
// 1b. Running a box cox, we do not reject the null for theta = 0, thus we should use a log transformation
	boxcox price crime nox rooms dist radial proptax stratio lowstat, lrtest

	// Generating a ln(price) variable
	gen logprice = ln(price)
	
	// Regress logprice on the other predictors
	reg logprice crime nox rooms dist radial proptax stratio lowstat
	
	// Comparing R-squared with untransformed price, we see that using the transformed price, R-squared improves from 0.71 to 0.76. This conforms with what I expected -- the transformation should improve the line-fit and raise the R-squared
	reg price crime nox rooms dist radial proptax stratio lowstat


// 1bb. Checking for heteroskedasticity, we reject the null (for constant variance; p < 0.00001). Thus, it violates the homoskedasticity assumption
	reg logprice crime nox rooms dist radial proptax stratio lowstat
	hettest
	
	// To check, I run rvfplot. It shows that as logprice increases, the residuals converges (i.e. heteroskedastic)
	rvfplot
	
	
// 1c. Rerunning regression with robust SE
	reg logprice crime nox rooms dist radial proptax stratio lowstat, robust
	
	
// 1d. Attempting to test heteroskedasticity with this "robust" regression -- we can't tell. Stata refuses to run this, citing inappropriateness -- hettest is not appropriate after robust cluster()
	hettest
	
	
// 1e. I opt for the aweight route. I first run avplots to get a sense of which predictor is accounting for the heteroskedisticity.
	avplots
		// It seems like crime is the problem here.

	// Next, just to make sure I am on the right track, I ran the following regression with aw = each predictor variable, followed by a hettest. This series of tests confirms that crime is the reason for heteroskedasticity. Weighting for crime, and only crime, the ensuing hettest (p = 0.27) does not allow us to reject the null for constant variance.
	qui reg logprice lowstat crime nox rooms dist radial proptax stratio [aw = crime]
	hettest
	
	qui reg logprice lowstat crime nox rooms dist radial proptax stratio [aw = lowstat]
	hettest
	
	qui reg logprice lowstat crime nox rooms dist radial proptax stratio [aw = nox]
	hettest
	
	qui reg logprice lowstat crime nox rooms dist radial proptax stratio [aw = rooms]
	hettest
	
	qui reg logprice lowstat crime nox rooms dist radial proptax stratio [aw = dist]
	hettest
	
	qui reg logprice lowstat crime nox rooms dist radial proptax stratio [aw = radial]
	hettest
	
	qui reg logprice lowstat crime nox rooms dist radial proptax stratio [aw = proptax]
	hettest
	
	qui reg logprice lowstat crime nox rooms dist radial proptax stratio [aw = stratio]
	hettest
	
	// I then attempt it with the next power for crime. When crime is raised to its 2nd power, we can more definitely assume homoskedasticity. Hettest shows p = 0.99 when crime^2 is used as the weighted variable
	reg logprice lowstat crime nox rooms dist radial proptax stratio [aw = crime^2]
	hettest
	
	
*******************************************************************************
// Frame change
	frame change Q2

// Load dataset
	use chirot.dta
	
// Generating product term for c*t
	gen ct = c*t
	
// Repeat Chirot and Ragin's analysis:
	reg i c t ct m g
	avplots
	rvfplot

	// At first, we see that adj R-squared = 0.61, with only c and ct showing significance at the 0.05 level. Avplots suggests no clear curvilinearity, although highly suspect of outliers and heteroskedasticity

// First, I check for outliers
	predict std_resids, rstandard
	list std_resids c t ct m g if abs(std_resids) > 2
			//id 3 and 26 have high residuals
	
// Next, I check for points of high leverage
	predict levs, leverage
	
	// Generating threshold beyond which a highly leveraged point is problematic (lk = (2k+2)/n, where k is number of predictors)
	count
	gen lk = (2*5+2)/32
	list c t ct m g levs id if levs > lk
		// id 20, 31, 32 are problematic

// Next, points of high influence
	reg i c t ct m g
	dfbeta
	
	// Getting the threshold k for influence = 2/sqrt(n)
	gen k = 2/sqrt(32)

	// Listing all points of high influence
	list id if _dfbeta_1 > k //id 20 is problematic
	list id if _dfbeta_2 > k //id 20 is problematic
	list id if _dfbeta_3 > k //nil
	list id if _dfbeta_4 > k //id 20 is problematic
	list id if _dfbeta_5 > k //id 23 is problematic
	
// Based on the examination of outliers, leverage, and influence, I make the choice to drop 6 observations: id 3, 20, 23, 26, 31 and 32
	
// Dropping id 3, 20, 23, 26, 31, 32
	drop if id == 3 | id == 20 | id == 23 | id == 26 | id == 31 | id == 32
	
// Rerun the regression and outlier tests
	reg i c t ct m g
	
	// Checking for large residuals 
	predict std_resids2, rstandard
	list std_resids2 c t ct m g if abs(std_resids) > 2 
		// no large residuals
		
	// Checking for leverage
	predict levs2, leverage
	count
	gen lk2 = (2*5+2)/26
	list c t ct m g levs id if levs > lk2
		// no obs with high leverage
		
	// Checking for influence
	reg i c t ct m g
	dfbeta
	gen k2 = 2/sqrt(26)
	list id if _dfbeta_6 > k2 //id 18 and 30 are problematic
	list id if _dfbeta_7 > k2 //id 30 is problematic
	list id if _dfbeta_8 > k2 //id 19 is problematic
	list id if _dfbeta_9 > k2 //id 11 and 30 are problematic
	list id if _dfbeta_10 > k2 //nil

// Based on the the above, I think I'd like to drop id 30 as well, but at this point I am comfortable including the other points.

	// Dropping id 30, and rerunning the regression
	drop if id == 30
	reg i c t ct m g
	predict e, res
	kdensity e, normal // residuals appears to be pretty normally distributed
		swilk e // swilk test shows p = 0.06, which is just shy of normality; ??? I don't know how to correct this though
	
		// Running avplots, which looks balanced
		avplots
			
// Next, I check for multicollinearity by running vif, which shows that ct, c and t  > 10, and are thus problematic
	vif 
	
	// I produce a correlation matrix, which suggests that c and t are highly correlated (0.965), which necessarily makes the product term between them problematic too
	vce, corr
	
	// To correct for this, I attempt to center the highly correlated variables c and t
	center c
	center t
	
	// I also do this for the product term c*t, by generating a new variable using the centered versions of c and t
	gen c_ct = c_c*c_t
	
	// I run the regression with these new centered terms
	reg i c_c c_t c_ct m g
	hettest // p = 0.61; residuals are homoskedastic

		// Checking for multicollinearity again, all vifs < 10 (solved)
		vif
		
		// Running avplots; looking pretty
		avplots
		rvfplot, yline(0)
	
// Running linktest to check for specification error
	linktest
		//_hat < 0.0001 and _hatsq = 0.18; these are what we want to see.
	
// Running ovtest to check for omitted variables
	ovtest
		// p = 0.198; we don't have an omitted variable problem
	
// Diagnostic outcomes: hettest good, functional form good, outliers checked, multicollinearity checked
	
	/* Results:

	1. Overall adj R-squared is high (0.77), suggesting that the model does a great job explaining intensity
	
	2. c_c barely fails significance at the 0.05 level (p = 0.052), beta = 0.08
	
	3. c_t is statistically signficant (p = 0.045), beta = 0.23
	
	4. c_ct is statistically significant (p = 0.001), beta = 0.034
	
	5. m and g does not appear to independently predict intensity
	
	6. Overall, the above provides most support for the traditionalism theory. Commercialization almost reaches statistical significant, with a modest beta of 0.08. The interaction between commercialization and traditionalism, despite being highly significant, is nonetheless for the most part ignorable because of its miniscule beta of 0.034. m and g does not appear to independently influence rebellion intensity.
*/

translate "diaghw.smcl" Ldiaghw.pdf, replace
