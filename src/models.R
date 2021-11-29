library(readr)
library(MASS)
library(dplyr)
library(tidyr)
library(nnet)
library(emmeans)
source("accelerometry_data_analysis/src/bonferroni.R")

# glm(), polr(), multinom()

profile <- read_rds("./data/profile.rds")

profile <- profile %>%
  mutate(chronotype = factor(chronotype, ordered = T))

ord1 <- polr(chronotype ~ age, data = profile, Hess = T)

ord2 <- polr(chronotype ~ age + night_shift_work, data = profile, Hess = T)
anova(ord1, ord2)

ord2 <- polr(chronotype ~ age + night_shift_work, data = profile, Hess = T)
anova(ord1, ord2)

ord3 <- polr(chronotype ~ age + night_shift_work + smoking, data = profile, 
             Hess = T)
anova(ord2, ord3)

ord4 <- polr(chronotype ~ age + night_shift_work + smoking + shift_work, 
             data = profile, Hess = T)
anova(ord3, ord4)

ord5 <- polr(chronotype ~ age + night_shift_work + smoking + shift_work + sex, 
             data = profile, Hess = T)
anova(ord4, ord5)

ord6 <- polr(chronotype ~ age + night_shift_work + smoking + shift_work + sex +
               ethnic, data = profile, Hess = T)
anova(ord5, ord6)

ord7 <- polr(chronotype ~ age + night_shift_work + smoking + shift_work + sex +
               ethnic + employment, data = profile, Hess = T)
anova(ord6, ord7)

ord8 <- polr(chronotype ~ age + night_shift_work + smoking + shift_work + sex +
               ethnic + employment + alcohol, data = profile, Hess = T)
anova(ord7, ord8)

ord9 <- polr(chronotype ~ age + night_shift_work + smoking + shift_work + sex +
               ethnic + employment + alcohol + loneliness, data = profile, Hess = T)
anova(ord8, ord9)

ord10 <- polr(chronotype ~ age + night_shift_work + smoking + shift_work + sex +
               ethnic + employment + alcohol + loneliness + depressed,
              data = profile, Hess = T)
anova(ord9, ord10)

ord11 <- polr(chronotype ~ age + night_shift_work + smoking + shift_work + sex +
               ethnic + employment + alcohol + loneliness + gp_depressed, 
              data = profile, Hess = T)
anova(ord10, ord11)

ord12 <- polr(chronotype ~ age + night_shift_work + smoking + shift_work + sex +
                ethnic + employment + alcohol + loneliness + gp_depressed +
                insomnia, data = profile, Hess = T)
anova(ord11, ord12)

ord13 <- polr(chronotype ~ age + night_shift_work + smoking + shift_work + sex +
                ethnic + employment + alcohol + loneliness + gp_depressed +
                insomnia + work_hours, data = profile, Hess = T)
anova(ord12, ord13)

ord14 <- polr(chronotype ~ age + night_shift_work + smoking + shift_work + sex +
                ethnic + employment + alcohol + loneliness + gp_depressed +
                insomnia + work_hours + sleep_hours, data = profile, Hess = T)
anova(ord13, ord14)

ord15 <- polr(chronotype ~ age + night_shift_work + smoking + shift_work + sex +
                ethnic + employment + alcohol + loneliness + gp_depressed +
                insomnia + work_hours + sleep_hours + ses, data = profile, Hess = T)
anova(ord14, ord15)

ord16 <- polr(chronotype ~ age + night_shift_work + smoking + shift_work + sex +
                ethnic + employment + alcohol + loneliness + gp_depressed +
                insomnia + work_hours + sleep_hours + ses + bmi, data = profile, Hess = T)
anova(ord15, ord16)

ord17 <- polr(chronotype ~ age + night_shift_work + smoking + shift_work + sex +
                ethnic + employment + alcohol + loneliness + gp_depressed +
                insomnia + work_hours + sleep_hours + ses + bmi + urbanization, 
              data = profile, Hess = T)
anova(ord16, ord17)

bonferroni(ord17, "alcohol")

