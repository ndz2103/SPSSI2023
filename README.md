# SPSSI2023

This is a repository for my Society for the Psychological Study of Social Issues poster submission. I will be presenting this research on from June 25th to June 27th, 2023 in Denver, CO. 

## Acitvity Description

This study investigates the degree to which the depth of connection with LGBT people informs feelings about LGBT folks. 1,164 participants completed survey items measuring their conservatism, religiosity, and support for LGBT people. Results show that conservatives and religious people are more likely to view LGBT people unfavorably and disapprove of same-sex marriage. However, LGBT favorability is moderated through LGBT closeness, but this is not the case for LGBT policy. The findings suggest intergroup contact, particularly how close a person is to an LGBT person informs attitudes about LGBT people, but not policy support.

## Data Description

### Data Source Availability Statement

Data used in this exercise were obtained through the American National Election Studies[(ANES)](https://electionstudies.org/data-center/anes-gss-2020-joint-study/) and General Social Survey[(GSS)](https://gss.norc.org/getthedata/Pages/SAS.aspx) websites. Access is restricted for statistical and research purposes. To downloadd the data, you must first create a free account on each data base.Thee terms of use for each dataset can be found on the organizations' websites.

### Codebook and Data

The ANES-GSS joint study was conducted in 2020, however, there is not concatenated dataset for download. Therefore, you must download each organization's half of the data using the links in the section above and merge them on your own. 

Access to the codebook for the ANES 2020 timeseries data can be found [here](https://acrobat.adobe.com/link/review?uri=urn:aaid:scds:US:fb8bc509-8b72-380e-856d-a048ea9e0b71). Similarly, the codebook for the GSS 2016-2020 panel data can be viewed [here](https://acrobat.adobe.com/link/review?uri=urn:aaid:scds:US:b2d60355-2576-3cc7-ae56-724d3ef88d7e).

### Citation

American National Election Studies. 2022. ANES-GSS 2020 Joint Study [dataset and documentation]. April 8, 2022 version. www.electionstudies.org

Davern, Michael; Bautista, Rene; Freese, Jeremy; Morgan, Stephen L.; and Tom W. Smith. General Social Survey
2016-2020 Panel. [Machine-readable data file]. Principal Investigator, Michael Davern; Co-Principal
Investigators, Rene Bautista, Jeremy Freese, Stephen L. Morgan, and Tom W. Smith. NORC ed. Chicago, 2021.
1 datafile (5,125 cases) and 1 codebook (4,586 pages).

## Files Included

<table>
  <tr>
    <th>File Name</th>
    <th>Description</th> 
  </tr>
  <tr>
    <td>spssi2023_raw.csv</td>
    <td>Merged raw ANES and GSS data, needs processing</td>
  </tr>
  <tr>
    <td>spssi2023_clean.csv</td>
    <td>Merged ANES and GSS data processed and cleaned</td>
  </tr>
  <tr>
    <td>spssi2023_regression.csv</td>
    <td>Regression ready (e.g., complete cases) ANES and GSS data</td>
  </tr>
  <tr>
    <td>SPSSI2023_Environ.RData</td>
    <td>RData file consisting of R studio environment with custom correation matrix function, raw, cleaned, and regression-ready data for analysis</td>
  </tr>
  <tr>
    <td>SPSSI2023.R</td>
    <td>R file containing code for data processing, cleaning, analysis, and visualization</td>
  </tr>
  <tr>
    <td>SPSSI2023_report.pdf</td>
    <td>PDF file functioning as project program report with code, visuals, and rationale of throughout research process</td>
  </tr>
  <tr>
    <td>SPSSI_proposal.pdf</td>
    <td>PDF file containing the abstract of my SPSSI 2023 proposal</td>
  </tr>
  <tr>
    <td>References</td>
    <td>Git document of references used in SPSSI 2023 presentation</td>
  </tr>
  <tr>
    <td>equal_mar.pdf</td>
    <td>PDF file of marriage equality regression plot</td>
  </tr>
  <tr>
    <td>feel_lgbt.pdf</td>
    <td>PDF file of LGBT favorability regression plot</td>
  </tr>
  <tr>
    <td>cor_mat.pdf</td>
    <td>PDF file of correlation matrix of variables of interest</td>
  </tr>
  <tr>
    <td>me_list.xlsx</td>
    <td>.xlsx file containing regression output for marriage equality models.</td>
  </tr>
  <tr>
    <td>fvr_list.xlsx</td>
    <td>.xlsx file containing regression output for LGBT favorability models.</td>
  </tr>
</table>

## Instructions

For pre-processing and analyzing the data, I used R packages including readr, haven, dplyr, and tidyr. Visualizations were created using ggplot2.

To import the supplied RData file containing the necessary R environment, first open the SPSSI2023.R file. Next, navigate to the "environment" pane and select the icon in the top-left corner of the designated area. This allows you to load an R environment. Navigate to and open EPA2023_RawEnviron.RData, which contains the raw ANES (anes) and GSS (gss) data for reproducibility. 

### Processing

To start from scratch, import the SPSSI2023_Environ.RData file which contains the merged ANES and GSS 2020 data (e.g., spssi2023_raw). This was accomplished by matching the ANES and GSS data files by their "V200001" and "anesid" columns, respectively. To do so, I filter the GSS and ANES data frames so that the variable "V200001"/"anesid" is greater than 0. This variable indicates that a respondent completed the joint ANES-GSS 2020 survey. Next, order the ANES and GSS data frames by their ID. Once the data frames were ordered numerically, I bound them using the cbind() function, which creates a concatenated data frame of the raw data. This file constitutes the spssi2023_raw.csv file. 

To select the variables of interest, use the select() function. Once selected, use the rename() function to rename the variables so they are understandable. The following predictor and control variables and their descriptions are as follows:

<table>
  <tr>
    <th>Variable Name</th>
    <th>Value range</th>
    <th>Rename</th>
    <th>Description</th>
  </tr>
  <tr>
    <td>AGE_1A</td>
    <td>continuous</td>
    <td>age</td>
    <td>Respondent age</td>
  </tr>
  <tr>
    <td>SEX_1A</td>
    <td>1 = male, 2 = female</td>
    <td>sex</td>
    <td>Respondent sex</td>
  </tr>
  <tr>
    <td>RACE_1A</td>
    <td>1=White, 2=Black, 3=other</td>
    <td>race</td>
    <td>Respondent race</td>
  </tr>
  <tr>
    <td>SEXORNT_1A</td>
    <td>1 = homosexual, 2 = bisexual, 3 = straight</td>
    <td>sexori</td>
    <td>Respondent sexual orientation</td>
  </tr>
  <tr>
    <td>ATTEND_1A</td>
    <td>0 = never to 8 = several times a week
    <td>relig_attend</td>
    <td>Religiosity measured as religious service attendance</td>
  </tr>
  <tr>
    <td>MARHOMO1_1A</td>
    <td>1 = strongly agree to 5 = strongly disagree</td>
    <td>equal_mar</td>
    <td>Approval of marriage equality</td>
  </tr>
  <tr>
    <td>POLVIEWS_1A</td>
    <td>1 = extremely liberal to 7 = extremely conservative</td>
    <td>poli</td>
    <td>Political ideology</td>
  </tr>
  <tr>
    <td>V202166</td>
    <td>0 = disfavor to 100 = favor</td>
    <td>feel_LGB</td>
    <td>Feeling thermometer of lesbian, gay, and bixsexual (LGB) persons</td>
  </tr>
  <tr>
    <td>V202172</td>
    <td>0 = disfavor to 100 = favor</td>
    <td>feel_T</td>
    <td>Feeling thermometer of transgender persons</td>
  </tr>
  <tr>
    <td>V202472a</td>
    <td>1 = yes, 0 = no know</td>
    <td>lgb_fam</td>
    <td>Respondent knows an immediate family member who is LGB</td>
  </tr>
  <tr>
    <td>V202472b</td>
    <td>1 = yes, 0 = no know</td>
    <td>lgb_relate</td>
    <td>Respondent knows an extended family member who is LGB</td>
  </tr>
  <tr>
    <td>V202472c</td>
    <td>1 = yes, 0 = no know</td>
    <td>lgb_neigh</td>
    <td>Respondent knows a neighbor who is LGB</td>
  </tr>
  <tr>
    <td>V202472d</td>
    <td>1 = yes, 0 = no know</td>
    <td>lgb_cowork</td>
    <td>Respondent knows a coworker who is LGB</td>
  </tr>
  <tr>
    <td>V202472e</td>
    <td>1 = yes, 0 = no know</td>
    <td>lgb_friend</td>
    <td>Respondent knows a friend who is LGB</td>
  </tr>
  <tr>
    <td>V202474a</td>
    <td>1 = yes, 0 = no know</td>
    <td>trans_fam</td>
    <td>Respondent knows an immediate family member who is transgender</td>
  </tr>
  <tr>
    <td>V202474b</td>
    <td>1 = yes, 0 = no know</td>
    <td>trans_relate</td>
    <td>Respondent knows an extended family member who is transgender</td>
  </tr>
  <tr>
    <td>V202474c</td>
    <td>1 = yes, 0 = no know</td>
    <td>trans_neigh</td>
    <td>Respondent knows a neighbor who is transgender</td>
  </tr>
  <tr>
    <td>V202474d</td>
    <td>1 = yes, 0 = no know</td>
    <td>trans_cowork</td>
    <td>Respondent knows a coworker who is transgender</td>
  </tr>
  <tr>
    <td>V202474e</td>
    <td>1 = yes, 0 = no know</td>
    <td>trans_friend</td>
    <td>Respondent knows a friend who is transgender</td>
  </tr>
  <tr>
</table>

Using the lapply() function, reclassify the variables of interest to a numeric class. Once this is done, complete the following processing changes:

1) reverse code equal_mar so those who support marriage equality have a higher score than those who don't.
2) filter any respondent with an age < 18 years old.
3) recode lgb and transgender relationship columns based on relative closeness:

<table>
<tr>
  <th>relationship</th>
  <th>recoded value</th> 
</tr>
<tr>
  <td>immediate family</td>
  <td>5</td>
</tr>
<tr>
  <td>extended family</td>
  <td>4</td>
</tr>
<tr>
  <td>friend</td>
  <td>3</td>
</tr>
<tr>
  <td>neighbor</td>
  <td>2</td>
</tr>
<tr>
  <td>coworker</td>
  <td>1</td>
</tr>
</table> 

4) calculate the sum of each row's lgb and transgender relationship columns (ranging from 0 to 15)
5) create a new variable called "lgbt_close" which is the average of "lgb_close" and "trans_close". This creates a column containing values 0-15 representing average lgbt-closeness.
6) recode feel_LGB and feel_T so that any 0 values = NA.
7) create a new variable called "lgbt_feel" which is the average of the feel_LGB and feel_T columns to create an aggregated feeling thermometer score (0 to 100).

This file constitutes the spssi2023_clean.csv file found in SPSSI2023_Environ.RDdata.

### Correlations

Import a [custom function](https://www.r-bloggers.com/2020/07/create-a-publication-ready-correlation-matrix-with-significance-levels-in-r/) created by Paul van der Laken (2020), to create a publication-ready correlation matrix in R. 

Using the anes_gss2020clean.csv data, create a data frame consisting of only the variables poli, relig_attend, equal_mar, feel_LGBT, and lgbt_close. Now using the custom function correlation_matrix(), create a data frame of the function output. 

Import the library gridExtra to export this data frame as a PDF file. First, use the pdf() function and specify the path, along with the dimensions of the PDF (I chose height = 4 and width = 12). Then use the grid.table() function and specify that you want to use the data frame of the correlation matrix. To fully render the PDF at the location you specify, use the function dev.off(). 

This creates a correlation matrix of the variables of interest. 

To get the specific p-values, t-values, and r of the correlations of interest, use the cor.test() function on the anes_gss2020clean data frame. Get the correlations of relig_attend X poli, relig_attend & poli X equal_mar, and relig_attend & poli X lgbt_feel. 

### Regression

For regression modeling, create a data frame from the spssi2023_clean data frame that contains only complete cases. this is the spssi2023_regression.csv data frame; specify this data frame in the regression equations.

**Marriage Equality**
1) linear regression: y = equal_mar and x's = relig_attend, poli, age, sex, race, and sexori.
2) linear regression: y = equal_mar and x's = (relig_attend X lgbt_close), (poli X lgbt_close), age, sex, race, and sexori.
3) anova of both models. 

**LGBT Favorability**
1) linear regression: y = lgbt_feel and x's = relig_attend, poli, age, sex, race, and sexori.
2) linear regression: y = lgbt_feel and x's = (relig_attend X lgbt_close), (poli X lgbt_close), age, sex, race, and sexori.
3) anova of both models. 

### Data Visualization

Import the ggplot2 package. Using the attach() function, specify the spssi2023_regression data frame. 

Personally, I was having difficulty using the predict() function to plot the regression lines of my models. As a last resort, I manually wrote the regression equations as a custom function and specified the variables. The regression equation followed the following pattern, where B1 and B2 are the coefficients of interest and k represents the controls. 

y = intercept + B1(relig_attend) + B2(poli) + k
y = intercept + B1(relig_attend X lgbt_close) + B2(poli X lgbt_close) + k

Using the regression functions following the pattern above, input the variables in the order that their coefficients are shown so that the variable is paired with the proper coefficient. Using the 4 regression functions, create 4 new columns of predicted values. 

**Marriage Equality**

To plot the regression lines associated with marriage equality, use the ggplot() function and specify the x aesthetic as the interaciton between poli and lgbt_close and the y aesthetic as equal_mar. 

Using the geom_jitter() geometry, specify jitter width and height along with point characteristics. Then using the stat_smooth() geometry, specify the parameter geom as "line", the x aesthetic as the interaction between poli and know_LGBT, and the y aesthetic as the predicted values of the non-interaction model. Finally, use the geom_smooth() geometry to plot the interaction model, specifying the x aesthetic as the interaction between poli and know_LGBT and y aesthetic of the corresponding interaction model's predicted values. In the geom_jitter(), stat_smooth(), and geom_smooth() geometries, you can specify parameters like standard error, alpha, and color to preference. Additional geometries like labels and annotations can be added as well. 

**LGBT Favorability**

To plot the regression lines associated with LGBT favorability follow the same steps as above, but set the ggplot() y aesthetic to feel_LGBT, and the stat_smooth() and geom_smooth y aethetics to their corresponding predicted values. 

### Software, Packages, and Applications for Wrangling and Analysis

I used RStudio (2022.12.0+353) as my statistical software for this activity. The wrangling and analyses conducted in the reposited files required the installation and use of several R packages including readr, tidyr, and dplyr. 

## Other Requirements

### Time

Total processing time takes less than an hour. Starting from scratch takes approximately 2-3 hours if unacquainted with the data.

### Computing 

The wrangling, analysis, and documentation required of this activity was done on macOS Ventura v 13.2.1 with a 2.6 GHz 6-Core Intel Core i7 processer and 16 GB 2400 MHz DDR4 memory. 
