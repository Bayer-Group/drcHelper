# William's Test

    Code
      williamsTest_JG(df = Response, trt = "Rate", resp = "y", direction = "decreasing")
    Output
           Rate Y.Tilde     Y0  Se.Diff DF    Will TCrit Signif
      Q6     10 0.02885 0.1264 0.003987 23 24.4700 1.827      *
      Q5   3.39 0.02885 0.1264 0.003987 23 24.4700 1.825      *
      Q4   1.15 0.04630 0.1264 0.003987 23 20.0900 1.817      *
      Q3   0.39 0.07210 0.1264 0.003987 23 13.6200 1.808      *
      Q2  0.132 0.09990 0.1264 0.003987 23  6.6470 1.785      *
         0.0448 0.12370 0.1264 0.003987 23  0.6772 1.714      .

---

    Code
      williamsTest_JG(df = Response, trt = "Rate", resp = "y", direction = "increasing")
    Output
           Rate Y.Tilde     Y0  Se.Diff DF   Will TCrit Signif
      Q6     10 0.07101 0.1264 0.003987 23 -13.89 1.827      .
      Q5   3.39 0.07101 0.1264 0.003987 23 -13.89 1.825      .
      Q4   1.15 0.07101 0.1264 0.003987 23 -13.89 1.817      .
      Q3   0.39 0.07101 0.1264 0.003987 23 -13.89 1.808      .
      Q2  0.132 0.07101 0.1264 0.003987 23 -13.89 1.785      .
         0.0448 0.07101 0.1264 0.003987 23 -13.89 1.714      .

---

    Code
      PMCMRplus::williamsTest(y ~ Rate, data = Response, alternative = "less")
    Output
      
      	 Williams trend test 
      
      data:  y by Rate 
      alternative hypothesis:  less 
      
      H0
                     t'-value df t'-crit decision alpha
      mu1 - ctr >= 0    0.672 23   1.714   accept  0.05
      mu2 - ctr >= 0    6.635 23   1.784   reject  0.05
      mu3 - ctr >= 0   13.624 23   1.808   reject  0.05
      mu4 - ctr >= 0   20.082 23   1.817   reject  0.05
      mu5 - ctr >= 0   24.468 23   1.825   reject  0.05
      mu6 - ctr >= 0   24.468 23   1.827   reject  0.05
      ---

# Dunnett Test

    Code
      DunnettTest(y ~ Rate, data = Response)
    Output
      
        Dunnett's test for comparing several treatments with a control :  
          95% family-wise confidence level
      
      $`0`
                                diff               lwr.ci                upr.ci
      0.0448-0 -0.002678756020212419 -0.01386315720902699  0.008505645168602154
      0.132-0  -0.026453838597394613 -0.03763823978620918 -0.015269437408580040
      0.39-0   -0.054313977114983716 -0.06549837830379829 -0.043129575926169145
      1.15-0   -0.080063746129196256 -0.09124814731801083 -0.068879344940381684
      3.39-0   -0.098516712187812439 -0.10970111337662701 -0.087332310998997867
      10-0     -0.096579534025759486 -0.10776393521457406 -0.085395132836944915
                  pval    
      0.0448-0  0.9702    
      0.132-0  3.4e-06 ***
      0.39-0   3.2e-15 ***
      1.15-0   < 2e-16 ***
      3.39-0   < 2e-16 ***
      10-0     < 2e-16 ***
      
      ---
      Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
      

---

    Code
      PMCMRplus::dunnettTest(y ~ Rate, alternative = "less")
    Message
      
      	Pairwise comparisons using Dunnett's-test for multiple	
                               comparisons with one control
      
      data: y by Rate
      
    Output
             0      
      0.0448 0.65   
      0.132  3.3e-06
      0.39   3.3e-16
      1.15   < 2e-16
      3.39   < 2e-16
      10     < 2e-16
    Message
      
      P value adjustment method: single-step
      alternative hypothesis: less

---

    Code
      PMCMRplus::dunnettTest(y ~ Rate, alternative = "greater")
    Message
      
      	Pairwise comparisons using Dunnett's-test for multiple	
                               comparisons with one control
      
      data: y by Rate
      
    Output
             0   
      0.0448 0.98
      0.132  1.00
      0.39   1.00
      1.15   1.00
      3.39   1.00
      10     1.00
    Message
      
      P value adjustment method: single-step
      alternative hypothesis: greater

---

    Code
      PMCMRplus::dunnettTest(y ~ Rate, alternative = "two.sided")
    Message
      
      	Pairwise comparisons using Dunnett's-test for multiple	
                               comparisons with one control
      
      data: y by Rate
      
    Output
             0      
      0.0448 1      
      0.132  3.4e-06
      0.39   6.7e-16
      1.15   < 2e-16
      3.39   < 2e-16
      10     < 2e-16
    Message
      
      P value adjustment method: single-step
      alternative hypothesis: two.sided

---

    Code
      glht(mod1, linfct = mcp(Dose = "Dunnett"))
    Output
      
      	 General Linear Hypotheses
      
      Multiple Comparisons of Means: Dunnett Contrasts
      
      
      Linear Hypotheses:
                                Estimate
      0.0448 - 0 == 0 -0.002678756020212
      0.132 - 0 == 0  -0.026453838597395
      0.39 - 0 == 0   -0.054313977114984
      1.15 - 0 == 0   -0.080063746129196
      3.39 - 0 == 0   -0.098516712187812
      10 - 0 == 0     -0.096579534025759
      

---

    Code
      glht(mod1, linfct = mcp(Dose = "Dunnett"), alternative = "less")
    Output
      
      	 General Linear Hypotheses
      
      Multiple Comparisons of Means: Dunnett Contrasts
      
      
      Linear Hypotheses:
                                Estimate
      0.0448 - 0 >= 0 -0.002678756020212
      0.132 - 0 >= 0  -0.026453838597395
      0.39 - 0 >= 0   -0.054313977114984
      1.15 - 0 >= 0   -0.080063746129196
      3.39 - 0 >= 0   -0.098516712187812
      10 - 0 >= 0     -0.096579534025759
      

