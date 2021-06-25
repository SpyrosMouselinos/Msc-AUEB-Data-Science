*******Online Appendix Replication File, Updated July 11, 2011 by Erica Chenoweth

******CHAPTER 1******

***Figure 1.4
tab2 nonviol outcome, chi2 row

***Figure 1.5
sum success if nonviol==1 & regchange == 1
sum success if nonviol==1 & regchange == 0 & other == 0
sum success if viol==1 & other == 1
sum success if viol==1 & regchange == 1
sum success if viol==1 & regchange == 0 & other == 0
sum success if viol==1 & other == 1

******CHAPTER 2******

***Table 2.2reg lmembers nonviol lnpop, robust cluster(tccode)

***Figure 2.1
logit success mempc lnpop, robust nolog cluster(tccode)predict xbtwoway (scatter xb mempc, sort) drop xb
***Table 2.3, Model 1Alogit defect lmembers lnpop, nolog robust cluster(lccode)mfx

***Figure 2.2
logit defect nonviol mempc ixnNVpc tgovcap tpolity, nolog robusttest nonviol ixnNVpc mempcpredict xb
twoway (spike xb mempc, by(nonviol) sort) drop xb

***Table 2.3, Model 2A***Effects of Resistance Type and Participation on the Likelihood of Economic Sanctionslogit sdirect nonviol lmembers ixnNVmem lnpop, nolog robusttest ixnNVmem nonviol lmembersprtab nonviolmfx***Table 2.3, Model 3A***Effects of Resistance Type and Participation on the Likelihood of Foreign State Supportlogit statesup nonviol lmembers lnpop , nolog robust cluster(tccode)prtab nonviolmfx***Table 2.4, Model 1B logit success nonviol lnpop, robust nolog cluster(tccode), if regviol==1mfxprtab nonviol***Table 2.4, Model 2Blogit success defect lmembers statesup regviol sdirect, nolog robust cluster(tccode), if nonviol==1prtab defect prtab lmembersmfx

***Table 2.4, Model 3Blogit success defect lmembers statesup regviol sdirect, nolog robust cluster(tccode), if nonviol==0prtab statesup mfx******CHAPTER 3******

***Table 3.1, Model 1
logit success nonviol tpolity lmembers lnpop, robust nolog cluster(tccode)mfx

***Figure 3.1tab2 loclpolity1 nonviol, chi2 column

***Table 3.1, Model 2
logit success nonviol tgovcap lmembers lnpop, nolog robust cluster(tccode)test nonviol lmembersmfx

***Figure 3.2
tab2 loclcap1 nonviol, chi2 columntab2 loclcap2 nonviol, chi2 columntab2 loclcap3 nonviol, chi2 columntab2 loclcap4 nonviol, chi2 column

***Table 3.1, Model 3
logit success nonviol regviol lmembers lnpop, nolog robust cluster(tccode)test nonviol lmembers
mfx

***Table 3.1, Model 4
logit success nonviol secession fselfdet regchange, robust nolog cluster (tccode)mfx

***Table 3.2
tab2 outcome nonviol, chi2 column, if regchange==1tab2 outcome nonviol, chi2 column, if fselfdet==1tab2 outcome nonviol, chi2 column, if secession==1tab2 outcome nonviol, chi2 column, if other==1

***Table 3.1, Model 5
logit success nonviol americas asia fsu europe middleeast, robust nolog cluster(tccode)mfx

***Table 3.1, Model 6
logit success nonviol t1 t2 t3 t4 t5 t6 t7 t8 t9 t11, robust nolog cluster (tccode)mfx

***Table 3.1, Model 7
logit success nonviol lmembers tpolity tgovcap regviol lnpop secession fselfdet regchange, robust nolog cluster (tccode)
test nonviol lmembers

***Table 3.1, Model 8
logit success nonviol lmembers tpolity tgovcap regviol lnpop secession fselfdet regchange americas asia middleeast fsu europe t1 t2 t3 t4 t5 t6 t7 t8 t9 t11 t11 , robust nolog cluster (tccode)
test nonviol lmembers

***Table WA.6. Determinants of Violent Resistance
probit viol secession violsim lgdppc lpolity1 lcinc1 wars lmtnest ef extfor , cluster(tccode)predict prob_violpwcorr prob_viol viol
sum prob_viol

***Table 3.3, Exogenous Model
probit success viol lnpop regviol, robust nolog cluster(tccode)

***Table 3.3, Endogenous Model 1
probit success prob_viol lnpop regviol, robust nolog cluster(tccode)
drop prob_viol

***Table 3.3, Endogenous Model 2
ivprob success, endog(viol) iv(secession violsim lgdppc lpolity1 lcinc1 wars lmtnest ef extfor ) exog( lnpop regviol)divprob success, endog(viol) iv(secession violsim lgdppc lpolity1 lcinc1 wars lmtnest ef extfor ) exog( lnpop regviol)

***Table 3.3, Endogenous Model 3
ivprob success, endog(viol) iv(secession violsim ) exog( lnpop regviol)divprob success, endog(viol) iv(secession violsim ) exog( lnpop regviol)

******CHAPTER 8******

***Table 8.1, Model 1
reg polityt5 lpolity nonviol lduration, robust cluster(lccode)

***Table 8.1, Model 2logit demdum nonviol lpolity lduration, robust cluster(lccode)mfxpredict xb**Figure 8.1separate xb, by(nonviol)twoway (scatter xb0 lpolity) (scatter xb1 lpolity), ytitle (Probability of Democracy) legend (order(1 "Violent" 2 "Nonviolent"))drop xbdrop xb0drop xb1

***Table 8.1, Model 3
logit demdum nonviol lpolity lduration, robust cluster(lccode), if success==0mfx

***Table 8.2
logit recur10 nonviol polityt10 violsim lgovcap, robust cluster(lccode) nologprtab nonviolmfx

******WEB APPENDIX*****

***Introduction

***Table WA1: Determinants of Missing Membership Valuelogit memdum nonviol fselfdet regchange secession success middleeast americas asia fsu europe lduration violsim, nolog robust***Controlling for Time Periodlogit memdum nonviol fselfdet regchange secession success middleeast americas asia fsu europe t1 t2 t3 t4 t5 t6 t7 t8 t9 t10 lduration violsim lpolity lnpop lgovcap, nolog robust

***Appendix to Chapter 2

***Table WA2
logit defect nonviol mempc ixnNVpc tgovcap tpolity, nolog robusttest nonviol ixnNVpc mempc
mfx

***Additional Chapter 2 Tests Not Reported in Text or Web Appendix

***Table 2.4, Model 2B removing lmembers to increase observationslogit success defect statesup regviol sdirect, nolog robust cluster(tccode), if nonviol==1prtab defect prtab regviolmfx***Table 2.4, Model 2B controlling for Logged Populationlogit success defect lmembers statesup regviol sdirect lnpop, nolog robust cluster(tccode), if nonviol==1prtab defect prtab lmembersmfx
***Table 2.4, Model 3B controlling for Logged Populationlogit success defect lmembers statesup regviol sdirect lnpop, nolog robust cluster(tccode), if nonviol==0prtab statesupmfx

***Appendix to Chapter 3

***Table WA.4: Random Effects Estimates 
***Model 1
xtlogit success lmembers lnpop, nolog i (eyear) , if nonviol==1
***Model 2
xtlogit success nonviol lnpop, nolog i (eyear)
***Model 3
xtlogit success nonviol, nolog i (eyear) 

***Table WA.5: Delta Variables
logit success nonviol lmembers deltatpolity deltatgdp deltatgovcap, nolog robust cluster(tccode)
test nonviol lmembers

***Table WA.6: Predictors of Violent Resistance
probit viol secession violsim lgdppc lpolity1 lcinc1 wars lmtnest ef extfor , cluster(tccode)predict prob_violpwcorr prob_viol viol
sum prob_viol

***Additional Chapter 3 Tests Not Reported in Text or Web Appendix

*** Assume that half of the violent campaigns (146) are really failed NV campaigns?tab2 success nonviol, chi2 column***57 successful NV conflicts / 252 total campaigns (with 146 viol campaigns considered NV "failures") = 22.6% success. ***Not statistically significant from violent campaign outcomes (25.3%).tab2 outcome nonviol, chi2 column***If we divide the total number of successes or partial successes of NV conflicts by the total number of campaigns (323), ***assuming that V campaigns are really just failed NV campaigns, we see that the total rate of success is 83/323=25.7%.***Comparing that to successful violent conflicts, the number is about the same: 55/217=25.4%.
***Appendix to Chapter 8

***Table WA.7: Controlling for Determinants of Democracy, Accounting for Potential for Endogeneitycdsimeq(politychanget5 lupop growthrgdppc rgdppc) (nonviol secession tgovcap extfor)
**Table WA.8: Effects of Resistance Type on Probability of Recurrent Civil War Onset over 1 and 5-Year Incrementslogit recur1 nonviol polityt1 violsim lgovcap, robust cluster(lccode) nologlogit recur5 nonviol polityt5 violsim lgovcap, robust cluster(lccode) nolog

***Additional Chapter 8 Tests Not Reported in Text or Web Appendix

**Effects of Nonviolent Resistance Type on Pr(Post-Conflict Democracy), Succesful Campaigns Onlylogit demdum nonviol lpolity lduration, robust cluster(lccode), if success==1mfx**Effects of Nonviolent Resistance on Pr(Democracy) in Authoritarian Regimes Onlylogit demdum nonviol lpolity lduration, robust cluster(lccode), if authdum == 1mfx**Effects of Insurgent Type of Democracy, Regime Change Campaigns Onlyreg polityt5 lpolity nonviol lduration, robust cluster(lccode), if regchange==1 **Effects of Insurgent Type on Democracy, Self-Determination, Secession, and "Other" Campaignsreg polityt5 lpolity nonviol lduration, robust cluster(lccode), if regchange==0***Alternative Time Periods (1, 10 years civil war recurrence)reg polityt1 lpolity nonviol lduration, robust cluster(lccode)reg polityt10 lpolity nonviol lduration, robust cluster(lccode)***Alternative Measure: Democratization over 0, 1, 5, and 10-Year Incrementsreg politychanget0 nonviol lduration, robust cluster(lccode)reg politychanget1 nonviol lduration, robust cluster(lccode)reg politychanget5 nonviol lduration, robust cluster(lccode)reg politychanget10 nonviol lduration, robust cluster(lccode)***Controlling for Determinants of Democracy, Accounting for Potential for Endogeneitycdsimeq(lpolity lupop growthrgdppc rgdppc) (nonviol lmembers tgovcap extfor)cdsimeq(politychanget0 lupop growthrgdppc rgdppc) (nonviol lmembers tgovcap extfor)cdsimeq(politychanget1 lupop growthrgdppc rgdppc) (nonviol secession tgovcap extfor)cdsimeq(politychanget10 lupop growthrgdppc rgdppc) (nonviol secession tgovcap extfor)
