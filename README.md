# Okanagan Chinook Population Viability Analysis 

## Context
Okanagan Chinook were assessed in May 2005 by the Committee on the Status of Endangered Wildlife in Canada (COSEWIC) as Endangered in an Emergency Assessment.This status was re-examined in April 2006 to be Threatened due to the potential for rescue from nearby Upper Columbia River Chinook populations.  In 2010, the Federal Minister of Environment recommended that the Okanagan Chinook population not be listed under the Federal Species at Risk Act.  Reasons for not listing this population included substantial losses in revenue to the BC economy and even in the complete absence of fisheries exploitation the recovery potential was low.  COSEWIC reassessed the status as Endangered in 2017, stating that rescue via straying from nearby populations is considered unlikely.  In B.C., Okanagan Chinook Salmon are listed as "apparently secure" – not at risk of extinction – however the population has a relatively high conservation priority under the BC Ministry of Environment Conservation Framework Priority.

Since 2002, the [Okanagan Nation Alliance (ONA)](https://www.syilx.org) has been actively involved in the study and conservation of Okanagan Chinook, including enumeration, biological sampling studies, and habitat enhancement.  ONA collaborated with Fisheries and Oceans, Columbia River Intertribal Fish Commission, and Summit Environmental to produce an Okanagan Chinook Recovery Potential Assessment (RPA) in 2006.  Furthermore, ONA produced a subsequent RPA in 2016.

As part of the RPA process a population viability analysis (PVA) was conducted.  This
repository contains the revised PVA code used for the most recent RPA, which
is scheduled to be published in 2020.

## Usage

The analysis code is contained in the custom R-package available in the `/CNPVA`
directory. An example analysis script  written as an RMarkdown notebook is 
provided (`PVA-analysis.Rmd`) along with the compiled html document
 (`PVA-analysis.nb.html`) generated from the notebook script, which 
 can be viewed in any modern web browser.

The code may be re-applied to other situations, but care should be taken to review
the codebase first as there are a couple occasions were years are hard-coded.