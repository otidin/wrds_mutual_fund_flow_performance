<!-- This is an R Markdown document. Markdown is a simple formatting syntax for authoring HTML, PDF, and MS Word documents. For more details on using R Markdown see <http://rmarkdown.rstudio.com>. -->
<!-- When you click the **Knit** button a document will be generated that includes both content as well as the output of any embedded R code chunks within the document. You can embed an R code chunk like this: -->
<!-- Plain text -->
<!-- End a line with two spaces to start a new paragraph. -->
<!-- *italiˆcs* and _italics_ -->
<!-- **bold** and __bold__ -->
<!-- superscript^2^ -->
<!-- ~~strikethrough~~ -->
<!-- # Header 1 -->
<!-- ## Headegr 2 -->
<!-- ### Header 3 -->
<!-- #### Header 4 -->
<!-- ##### Header 5 -->
<!-- ###### Header 6 -->
<!-- endash: -- -->
<!-- emdash: --- -->
<!-- ellipsis: ... -->
<!-- inline equation: $A = \pi*r^{2}$ -->
<!-- <!-- image: ![](path/to/smallorb.png) -->
<!-- <!-- horizontal rule (or slide break): -->
<!-- > block quote -->
<!-- * unordered list -->
<!-- * item 2 -->
<!--     + sub-item 1 -->
<!--     + sub-item 2 -->
<!-- 1. ordered list -->
<!-- 2. item 2 -->
<!--     + sub-item 1 -->
<!--     + sub-item 2 -->
<!-- Table Header  | Second Header -->
<!-- ------------- | ------------------ -->
<!-- Table Cell    | Cell 2 -->
<!-- Cell 3        | Cell 4 -->
<!-- Two plus two equals 4 -->
<!-- Here’s some code -->
<!-- ```{r} -->
<!-- dim(iris) -->
<!-- ``` -->
<!-- Here’s some code -->
<!-- ```{r echo=FALSE} -->
<!-- dim(iris) -->
<!-- ``` -->
<!-- Here’s some code -->
<!-- ```{r eval=FALSE} -->
<!-- dim(iris) -->
<!-- ``` -->
<!-- ```{r cars} -->
<!-- summary(cars) -->
<!-- ``` -->
<!-- ## Including Plots -->
<!-- You can also embed plots, for example: -->
<!-- Choose a time series: -->
<!-- ```{r echo = FALSE} -->
<!-- selectInput("data", "", -->
<!--   c("co2", "lh")) -->
<!-- ``` -->
<!-- See a plot: -->
<!-- ```{r echo = FALSE} -->
<!-- renderPlot({d <- get(input$data) -->
<!-- plot(d) })  -->
<!-- ``` -->
Data Overview
-------------

Data retrieved from Wharton Research Data Services

### Fund Headers Information

    ##     cusip8 crsp_fundno crsp_portno crsp_cl_grp
    ## 1 00036M30           1          NA     2000001
    ## 2 00036W20           2          NA     2000002
    ## 3 00078H80           3          NA     2000003
    ## 4 00036J60           4          NA     2000004
    ## 5                    5          NA          NA
    ## 6 02317K67           7          NA          NA
    ##                                                                     fund_name
    ## 1                                AARP Income Trust: AARP Bond Fund for Income
    ## 2 AARP Managed Investment Portfolios Trust: AARP Diversified Growth Portfolio
    ## 3            ABN AMRO Funds: International Fixed Income Fund; Investor Shares
    ## 4                            AARP Growth Trust: AARP International Stock Fund
    ## 5                                                   Active Assets Money Trust
    ## 6                                Ambassador Funds:Established Co Gr/Fiduciary
    ##   ticker    ncusip first_offer_dt                         mgmt_name
    ## 1  AABIX 00036M307       19970201 AMERICAN ASSOC OF RETIRED PERSONS
    ## 2  AADGX 00036W206       19970203 AMERICAN ASSOC OF RETIRED PERSONS
    ## 3  AAIFX 00078H802       19930426           ABN AMRO ASSET MGMT INC
    ## 4  AAISX 00036J601       19970203 AMERICAN ASSOC OF RETIRED PERSONS
    ## 5  AAMXX                 19820101                                  
    ## 6  ABCFX 02317K679       19920101                                  
    ##   mgmt_cd            mgr_name   mgr_dt                   adv_name
    ## 1     ARP   Robert S. Cessine 19980101 SCUDDER KEMPER INVESTMENTS
    ## 2     ARP Tajbakhsh/Bruno/Chu 19970101 SCUDDER KEMPER INVESTMENTS
    ## 3     ABN      Wouter Weijand 19970101    ABN AMRO ASSET MGMT INC
    ## 4     ARP  Cheng/Team Managed 19990101 SCUDDER KEMPER INVESTMENTS
    ## 5                                   NA                           
    ## 6                                   NA                           
    ##   open_to_inv retail_fund inst_fund m_fund index_fund_flag vau_fund
    ## 1           Y           N         N      N                        N
    ## 2           Y           N         N      N                        N
    ## 3           Y           Y         N      N                        N
    ## 4           Y           N         N      N                        N
    ## 5                                        Y                        N
    ## 6                                        N                        N
    ##   et_flag   end_dt dead_flag delist_cd merge_fundno
    ## 1         20000731         Y         M         8441
    ## 2         20000831         Y         M         8425
    ## 3         20000831         Y         L           NA
    ## 4         20000731         Y         M         8513
    ## 5         19901231         Y         L           NA
    ## 6         19950531         Y         M        21296

### Fund Styles Information

    ##   crsp_fundno    begdt    enddt crsp_obj_cd si_obj_cd accrual_fund
    ## 1           1 19971231 19981230        ICQM       CMQ             
    ## 2           1 19981231 19991230        ICQY                       
    ## 3           1 19991231 20000731        ICQY                      Y
    ## 4           2 19971231 19981230        EDYG       GRO             
    ## 5           2 19981231 19991230        EDYB                       
    ## 6           2 19991231 20000831        EDYB                      N
    ##   sales_restrict wbrger_obj_cd policy lipper_class
    ## 1                                                 
    ## 2                                                 
    ## 3              Y                               BBB
    ## 4                                                 
    ## 5                                                 
    ## 6              Y                              MLVE
    ##                lipper_class_name lipper_obj_cd
    ## 1                                             
    ## 2                                          BBB
    ## 3 Corporate Debt Funds BBB-Rated           BBB
    ## 4                                             
    ## 5                                           GI
    ## 6          Multi-Cap Value Funds            GI
    ##                  lipper_obj_name lipper_asset_cd lipper_tax_cd
    ## 1                                                             
    ## 2 Corporate Debt Funds BBB-Rated                              
    ## 3 Corporate Debt Funds BBB-Rated              TX       Taxable
    ## 4                                                             
    ## 5        Growth and Income Funds                              
    ## 6        Growth and Income Funds              EQ       Taxable

### Monthly TNA Return NAV

    ##       caldt crsp_fundno   mtna      mret       mnav
    ## 15 19970530           1 26.157  0.013523 14.8700000
    ## 16 19970630           1 34.609  0.012502 14.9700000
    ## 17 19970731           1 42.490  0.029325 15.3200000
    ## 18 19970829           1 46.629 -0.010477 15.0700000
    ## 19 19970930           1 57.900  0.014313 15.2000000
    ## 20 19971031           1 69.717  0.008461 15.2400000

Filtering of funds
------------------

Total number of funds

    ## [1] 67066

Fund start date filtering
-------------------------

Funding starting date interval : 1923.05.31-2019.09.26

    ##     Min.  1st Qu.   Median     Mean  3rd Qu.     Max.     NA's 
    ## 19230531 19980430 20050803 20040338 20120430 20190926      265

Take after the year 2000 how many of them and what percentage and how
many filtered in

    ## [1] 46417

    ## [1] 69.21093

    ## [1] 46417

ETF and ETN flag filter
-----------------------

Discard funds with ETF and ETN flag

how many without flags and what percentage and how many filtered in

    ## [1] 63803

    ## [1] 95.13464

    ## [1] 43186

Retail-institutional filtering
------------------------------

Discard retail funds, include institutional

how many of institutional and what percentage and how many filtered in

    ## [1] 26937

    ## [1] 40.16491

    ## [1] 20092
