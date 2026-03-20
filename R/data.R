#' ANR2: Binary Psychiatric Comorbidity Dataset
#'
#' A binary indicator dataset recording the presence (1) or absence (0) of
#' nine psychiatric diagnoses for a sample of patients. The dataset is
#' included as the primary example dataset for the binary MCA pipeline
#' (\code{\link{alsi_workflow}}).
#'
#' @format A data frame with 13 columns:
#' \describe{
#'   \item{MDD}{Major Depressive Disorder (0/1)}
#'   \item{DYS}{Dysthymia (0/1)}
#'   \item{DEP}{Depressive disorder NOS (0/1)}
#'   \item{PTSD}{Post-Traumatic Stress Disorder (0/1)}
#'   \item{OCD}{Obsessive-Compulsive Disorder (0/1)}
#'   \item{GAD}{Generalized Anxiety Disorder (0/1)}
#'   \item{ANX}{Anxiety disorder NOS (0/1)}
#'   \item{SOPH}{Social Phobia (0/1)}
#'   \item{ADHD}{Attention Deficit Hyperactivity Disorder (0/1)}
#'   \item{pre_EDI}{Pre-treatment EDI score (numeric)}
#'   \item{post_EDI}{Post-treatment EDI score (numeric)}
#'   \item{pre_bmi}{Pre-treatment BMI (numeric)}
#'   \item{post_bmi}{Post-treatment BMI (numeric)}
#' }
#'
#' @usage data(ANR2)
#'
#' @examples
#' data(ANR2)
#' vars <- c("MDD", "DYS", "DEP", "PTSD", "OCD", "GAD", "ANX", "SOPH", "ADHD")
#' \donttest{
#' results <- alsi_workflow(ANR2, vars = vars, B_pa = 100, B_boot = 100)
#' }
"ANR2"


#' Big Five Inventory (Full Dataset)
#'
#' Full responses to the 50-item Big Five Inventory (BFI; John & Srivastava,
#' 1999) for N = 500 adults, rated on a 5-point Likert scale from 1
#' (Disagree strongly) to 5 (Agree strongly), along with demographic
#' variables. The ten Extraversion items (E1--E10) are used as the primary
#' example for the ordinal ALSI pipeline
#' (\code{\link{alsi_workflow_ordinal}}).
#'
#' @format A data frame with 57 columns:
#' \describe{
#'   \item{ID}{Participant identifier}
#'   \item{age}{Age in years}
#'   \item{gender}{Gender (coded)}
#'   \item{country}{Country of origin}
#'   \item{source}{Data collection source}
#'   \item{E1}{Extraversion item 1 (1--5 scale)}
#'   \item{E2}{Extraversion item 2, reverse-keyed (1--5 scale)}
#'   \item{E3}{Extraversion item 3 (1--5 scale)}
#'   \item{E4}{Extraversion item 4, reverse-keyed (1--5 scale)}
#'   \item{E5}{Extraversion item 5 (1--5 scale)}
#'   \item{E6}{Extraversion item 6, reverse-keyed (1--5 scale)}
#'   \item{E7}{Extraversion item 7 (1--5 scale)}
#'   \item{E8}{Extraversion item 8, reverse-keyed (1--5 scale)}
#'   \item{E9}{Extraversion item 9 (1--5 scale)}
#'   \item{E10}{Extraversion item 10, reverse-keyed (1--5 scale)}
#'   \item{E_mean}{Mean of Extraversion items}
#'   \item{A1}{Agreeableness item 1 (1--5 scale)}
#'   \item{A2}{Agreeableness item 2 (1--5 scale)}
#'   \item{A3}{Agreeableness item 3 (1--5 scale)}
#'   \item{A4}{Agreeableness item 4 (1--5 scale)}
#'   \item{A5}{Agreeableness item 5 (1--5 scale)}
#'   \item{A6}{Agreeableness item 6 (1--5 scale)}
#'   \item{A7}{Agreeableness item 7 (1--5 scale)}
#'   \item{A8}{Agreeableness item 8 (1--5 scale)}
#'   \item{A9}{Agreeableness item 9 (1--5 scale)}
#'   \item{A10}{Agreeableness item 10 (1--5 scale)}
#'   \item{A_mean}{Mean of Agreeableness items}
#'   \item{C1}{Conscientiousness item 1 (1--5 scale)}
#'   \item{C2}{Conscientiousness item 2 (1--5 scale)}
#'   \item{C3}{Conscientiousness item 3 (1--5 scale)}
#'   \item{C4}{Conscientiousness item 4 (1--5 scale)}
#'   \item{C5}{Conscientiousness item 5 (1--5 scale)}
#'   \item{C6}{Conscientiousness item 6 (1--5 scale)}
#'   \item{C7}{Conscientiousness item 7 (1--5 scale)}
#'   \item{C8}{Conscientiousness item 8 (1--5 scale)}
#'   \item{C9}{Conscientiousness item 9 (1--5 scale)}
#'   \item{C10}{Conscientiousness item 10 (1--5 scale)}
#'   \item{C_mean}{Mean of Conscientiousness items}
#'   \item{N1}{Neuroticism item 1 (1--5 scale)}
#'   \item{N2}{Neuroticism item 2 (1--5 scale)}
#'   \item{N3}{Neuroticism item 3 (1--5 scale)}
#'   \item{N4}{Neuroticism item 4 (1--5 scale)}
#'   \item{N5}{Neuroticism item 5 (1--5 scale)}
#'   \item{N6}{Neuroticism item 6 (1--5 scale)}
#'   \item{N7}{Neuroticism item 7 (1--5 scale)}
#'   \item{N8}{Neuroticism item 8 (1--5 scale)}
#'   \item{N9}{Neuroticism item 9 (1--5 scale)}
#'   \item{N10}{Neuroticism item 10 (1--5 scale)}
#'   \item{N_mean}{Mean of Neuroticism items}
#'   \item{O1}{Openness item 1 (1--5 scale)}
#'   \item{O2}{Openness item 2 (1--5 scale)}
#'   \item{O3}{Openness item 3 (1--5 scale)}
#'   \item{O4}{Openness item 4 (1--5 scale)}
#'   \item{O5}{Openness item 5 (1--5 scale)}
#'   \item{O6}{Openness item 6 (1--5 scale)}
#'   \item{O7}{Openness item 7 (1--5 scale)}
#'   \item{O8}{Openness item 8 (1--5 scale)}
#'   \item{O9}{Openness item 9 (1--5 scale)}
#'   \item{O10}{Openness item 10 (1--5 scale)}
#'   \item{O_mean}{Mean of Openness items}
#' }
#' @source Open-Source Psychometrics Project,
#'   \url{https://openpsychometrics.org/}
#' @references
#'   John, O. P., & Srivastava, S. (1999). The Big-Five trait taxonomy:
#'   History, measurement, and theoretical perspectives. In L. A. Pervin
#'   & O. P. John (Eds.), \emph{Handbook of personality: Theory and
#'   research} (2nd ed., pp. 102--138). Guilford Press.
#' @keywords datasets
"BFI_Extraversion"


#' WAIS-IV and WMS-IV Cognitive Domain Scores
#'
#' Continuous domain scores from the Wechsler Adult Intelligence
#' Scale---Fourth Edition (WAIS-IV) and Wechsler Memory Scale---Fourth
#' Edition (WMS-IV) for N = 900 individuals. All scores are expressed
#' on the standard score metric (normative M = 100, SD = 15).
#' Data are de-identified and provided for replication purposes only;
#' please do not redistribute.
#'
#' @format A data frame with 17 columns:
#' \describe{
#'   \item{ID}{Participant identifier}
#'   \item{age}{Age in years}
#'   \item{sex}{Sex (coded)}
#'   \item{edu}{Education level (coded)}
#'   \item{eth}{Ethnicity (coded)}
#'   \item{fsiq}{Full Scale IQ score}
#'   \item{X}{Row index}
#'   \item{VC}{Verbal Comprehension index score}
#'   \item{PR}{Perceptual Reasoning index score}
#'   \item{WO}{Working Memory index score}
#'   \item{PS}{Processing Speed index score}
#'   \item{IM}{Immediate Memory index score}
#'   \item{DM}{Delayed Memory index score}
#'   \item{VWM}{Visual Working Memory index score}
#'   \item{VM}{Visual Memory index score}
#'   \item{AM}{Auditory Memory index score}
#'   \item{AVE}{Average of domain scores}
#' }
#' @source De-identified clinical assessment data.
#' @keywords datasets
"wawm4"

