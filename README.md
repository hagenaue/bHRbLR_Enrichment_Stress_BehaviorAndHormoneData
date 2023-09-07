# Code Repository: bHRbLR_Enrichment_Stress_BehaviorAndHormoneData

This repository includes the code for the analysis of behavioral and hormonal data within the publication "Adolescent social and environmental enrichment produces long-term increases in social resilience in a genetic rodent model of stress vulnerability: Impact on behavior, circulating hormones, and brain gene expression" by *Angela M. O'Connor, *Megan H. Hagenauer, Liam Cannon Thew Forrester, Pamela Patterson, Keiko Arakawa, Huzefa Khalil, Evelyn R. Richardson, Elaine K. Hebda-Bauer, Farizah I. Rob, Yusra Sannah, Stanley J. Watson, Jr., Huda Akil. (DOI: TBA)

Contributors to this repository include Megan H. Hagenauer (ORCID: 0000-0002-3715-9475), Liam Cannon Thew Forrester (ORCID: 0000-0003-4101-0902), and Evelyn R. Richardson (ORCID: 0000-0002-3473-8201).
This work was completed at the University of Michigan between 10/2019-09/2023.

The input dataset for this code can be found in the repository here:
https://github.com/hagenaue/bHRbLR_Enrichment_Stress_BehaviorAndHormoneData/blob/master/HRLR_EE_Stress_AllBehavData_forR_withNewCORTOxytIL6_SI_OFSDScoresFixed_FixedFormatIDs_TimeOnTop_forFullBehavior2.csv

Or the full Rproject can be downloaded from the repository here:
https://github.com/hagenaue/bHRbLR_Enrichment_Stress_BehaviorAndHormoneData/blob/master/Angela_HRLR_StressEnrichData.Rproj

The behavioral and hormonal data used in this analysis will also be released on Figshare (DOI: 10.6084/m9.figshare.24085524) with detailed variable definitions and metadata. 

The analysis code files are numbered based on the order that they were run (#01-12). Occassionally analyses/plots were updated in response to coauthor requests or new formatting requirements. These changes are often noted in the code documents themselves, but sometimes led to a new code document for the same goal. In those cases, the updated code document will have the same number (e.g., #05). 

There are several naming conditions in the code file that differ from what was used in the final paper (and Figshare data release):

Adolescent Enrichment:
EE=Social and Environmental ("Enhanced) Enrichment - sometimes just referred to as environmental enrichment in the code
SE=Social Enrichment - referred to as EC or "cage enrichment" in the code
NIL=Standard Housing 

Bred Line:
bLR=bred Low Responder rat line - sometimes just called LR in the code
bHR=bred High Responder rat line - sometimes just called HR in the code


*******************

Additional code files that represent analyses that were not included in this publication are located in the "NotUsedInPaper" folder:
https://github.com/hagenaue/bHRbLR_Enrichment_Stress_BehaviorAndHormoneData/tree/134ffc13e0d8741e647e1c62af2697fb3c1bf149/NotUsedInPaper

These analyses include:
1) Analyses of ultrasonic vocalization data (which we eventually realized was recorded incorrectly - 10x differences in gain)
2) Detailed analyses of the open field habituation data (which we realized cannot be interpreted as usual in terms of exploration/anxiety because we used a non-standard protocol in which the rats were placed in the center at start of the test causing anxious animals to sometimes freeze in the center instead of showing traditional thigmotaxis).
3) Analyses of the subset of animals that provided tissue for later proteoglycan immunocytochemistry analyses (separate publication, in prep).




