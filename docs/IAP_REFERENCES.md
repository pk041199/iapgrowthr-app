# IAP references currently available

This file lists the references that are clearly attributable to the Indian Academy of Pediatrics (IAP) and are relevant to this package.

## Confirmed IAP references

### 1. IAP 2015 growth charts for 5 to 18 years

Use for:

- height
- weight
- BMI

Primary publication:

- Indian Academy of Pediatrics Growth Charts Committee, et al. "Revised IAP growth charts for height, weight and body mass index for 5- to 18-year-old Indian children." `Indian Pediatrics` 2015;52(1):47-55.
- PubMed: https://pubmed.ncbi.nlm.nih.gov/25638185/

Supporting review:

- Khadilkar VV, Khadilkar AV. "Revised Indian Academy of Pediatrics 2015 growth charts for height, weight and body mass index for 5-18-year-old Indian children." `Indian Journal of Endocrinology and Metabolism` 2015;19(4):470-476.
- PubMed: https://pubmed.ncbi.nlm.nih.gov/26180761/

Notes:

- This is the main IAP reference set suitable for percentile and z-score implementation.
- The paper states that the charts were constructed using Cole's LMS method.
- For package implementation, the exact age-sex LMS table still needs to be extracted from the article or any official machine-readable source.

### 2. IAP recommendation for children under 5 years

Use for:

- growth assessment below 5 years

Source:

- Same IAP 2015 growth charts publication above.
- PubMed: https://pubmed.ncbi.nlm.nih.gov/25638185/

Notes:

- IAP recommends `WHO standards` for growth assessment of children below 5 years of age.
- This means there is no newer under-5 IAP-specific growth reference to package if the goal is to stay with currently recommended IAP sources.

## Related IAP guidance, but not a direct LMS reference set

### 3. IAP obesity guideline update

Relevant for:

- BMI interpretation
- use of waist circumference in obesity workup

Source:

- "Indian Academy of Pediatrics Revised Guidelines on Evaluation, Prevention and Management of Childhood Obesity."
- PubMed: https://pubmed.ncbi.nlm.nih.gov/38087786/

Notes:

- This guideline supports use of IAP 2015 BMI charts for ages 5 to 18 years.
- It also says waist circumference should be measured and plotted on India-specific charts.
- It does not itself provide an official IAP LMS reference table for waist circumference.

### 4. Indian Pediatrics blood pressure reference tables from Karnataka

Relevant for:

- systolic blood pressure
- diastolic blood pressure
- pediatric BP percentile classification

Primary publication:

- Krishna P, PrasannaKumar KM, Desai N, Thennarasu K. "Blood pressure reference tables for children and adolescents of Karnataka." `Indian Pediatrics` 2006;43(6):491-501.
- Indian Pediatrics full text: https://www.indianpediatrics.net/june2006/june-491-501.htm
- PubMed: https://pubmed.ncbi.nlm.nih.gov/16820658/

What it provides:

- age range `3 to 18 years`
- systolic and diastolic BP tables
- percentiles `50th`, `90th`, `95th`, and `99th`
- stratification by height percentile within age

Important limitation:

- This is a published Indian reference in the journal `Indian Pediatrics`, but it is not clearly an official IAP-issued nationwide standard in the same way the 2015 IAP growth charts are.
- The data are from Karnataka, so this should be treated as a regional Indian reference unless a broader official adoption source is identified.
- The paper gives percentile tables, not LMS parameters, so BP support will need a table-lookup implementation rather than the LMS z-score pipeline used for height, weight, and BMI.

## Not found as official IAP reference sets

I did not find a clearly published IAP reference dataset for:

- waist circumference
- hip circumference
- neck circumference
- head circumference
- blood pressure

However, a usable Indian Pediatrics BP percentile table is available from the Karnataka study above.

There are Indian studies for some of these measures, but they are not clearly published as official IAP reference standards in the sources reviewed above.

## Package implication

If the package is kept strictly IAP-based, the initial release should support:

- `height`
- `weight`
- `bmi`
- age range `5 to 18 years`

And optionally provide:

- a wrapper or documented fallback to `WHO` references for under-5 children

Any support for `wc`, `hc`, `nc`, `hip`, or `bp` should be added only after selecting non-IAP Indian reference sources explicitly.

For `bp`, the Karnataka 2006 Indian Pediatrics tables are the strongest currently identified candidate.

## CDC head circumference source choice

For `hc`, this package should use CDC references as a separate source family rather than folding them into the IAP set.

Recommended source boundary:

- CDC growth charts for head circumference by age
- use only official CDC growth-chart material for the HC reference extraction

Implementation note:

- keep `hc` in a dedicated CDC file and reader path
- do not label CDC-derived HC values as IAP references
