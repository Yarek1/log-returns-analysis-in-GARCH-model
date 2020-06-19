# log-returns-analysis-in-GARCH-model
Analysis residuals and log-returns in WIG20 index in two periods, first before coronovirus epidemic  and second after it.

Benefit of using returns, versus prices, is normalization: 
measuring all variables in a comparable metric, thus enabling evaluation of analytic relationships amongst two or more variables despite 
originating from price series of unequal values. This is a requirement for many multidimensional statistical analysis and machine learning techniques. 
For example, interpreting an equity covariance matrix is made sane when the variables are both measured in percentage.

While the returns for stocks usually have a normal distribution, the stock price itself is often log-normally distributed. 
This is because extreme moves become less likely as the stock's price approaches zero.
