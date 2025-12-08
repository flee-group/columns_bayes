dat = readRDS("data/cleaned_data.rds")


dat <- dat |>
  mutate(day_no = factor(day_no, 
                         levels = c("Day00", "Day1", "Day2", "Day3", "Day7", "Day9", "Day10", "Day12", "Day14", "Day17")))


llm_bix <- lmerTest::lmer(bix ~ day_no * col_no + (1 | replicate) + (1|columnID), data = dat)
summary(llm_bix)
anova(llm_bix)


llm_bix_2 <- lmerTest::lmer(bix ~ day_no * col_no  + (1|columnID), data = dat)
summary(llm_bix_2)
anova(llm_bix_2)

llm_bix_3 <- lmerTest::lmer(bix ~ day_no * col_no  + (1 | replicate), data = dat)
summary(llm_bix_3)
anova(llm_bix_3)

llm_bix_4 <- lm(bix ~ day_no * col_no  , data = dat)
summary(llm_bix_4)
anova(llm_bix_4)


anova(llm_bix, llm_bix_2, llm_bix_3, llm_bix_4)



# TODO make the lmer and following posthocs for all the variables
fm <-list()
# Apply experiment_lmer to columns 2 to 9 in ER_data
fm <- lapply(names(dat)[c(2:4, 8:10)], function(colname) {
  # Get the formula to be used in lmer
  formula <- as.formula(paste0(colname, " ~ sample_date * col_no + (1 | replicate) + (1|column_id)"))
  
  # Fit the model using lmer
  fit <- lme4::lmer(formula, data = dat)
  # warning in case of isSingular
  if (isSingular(fit)) {
    warning(paste("Singular fit detected for variable:", colname))
  }
  
  # Get the summary of the model
  summary <- summary(fit)
  # Returns the fit and the summary of each variable
  # Do not return fit if just checking out the summaries
  return(list(fit, summary))
})
names(fm) <- names(dat)[c(2:4, 8:10)]