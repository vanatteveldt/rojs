#install.packages("RMySQL")
library(RMySQL)
library(tidyverse)

# setup ssh tunnel
# ssh -L 3307:localhost:3306 computationalcommunication.org, or if you're cool:
system2("ssh", c("-L 3307:localhost:3306", "-N", "-T", "computationalcommunication.org"), wait=FALSE)

# connect
user = Sys.getenv("LOGNAME")
conn = dbConnect(MySQL(), user=user, password=.rs.askForPassword("Enter password:"), dbname='ojs2', host='127.0.0.1', port=3307)
# helper function to read full table as tibble
t = function(...) dbReadTable(conn, ...) %>% as.tibble

# informational: list tables / columns / data:
dbListTables(conn) %>% (function(x) x[grep("subm", x)])
dbListFields(conn, 'review_assignments')
t('edit_decisions') 

# submissions
submissions = t('submissions') %>% mutate(date_submitted=as.Date(date_submitted)) %>% select(submission_id, date_submitted, status)
titles = t('submission_settings') %>% filter(setting_name=='cleanTitle') %>% select(submission_id, title=setting_value)

# decisions
last_decision = t('edit_decisions') %>%  mutate(date_decided=as.Date(date_decided)) %>% 
  group_by(submission_id) %>% arrange(desc(date_decided), desc(edit_decision_id)) %>% filter(row_number() == 1) %>%
  left_join(t('users') %>% select(editor_id=user_id, editor=first_name)) %>% select(submission_id, editor, decision) 



today = Sys.Date()
longago = Sys.Date() - 7
reviews = t('review_assignments') %>% select(submission_id, review_round_id, date_assigned, date_confirmed, date_completed, date_response_due, date_due, declined, round, step) %>%
  mutate(date_assigned=as.Date(date_assigned), date_due=as.Date(date_due), date_confirmed=as.Date(date_confirmed), date_response_due=as.Date(date_response_due))
reviews = reviews %>% mutate(status=ifelse(declined==1, "DECLINED", ifelse(is.na(date_confirmed), 
                                                                           ifelse(date_response_due<today, ifelse(date_response_due<longago, "PENDING_WAY_OVERDUE", "PENDING_OVERDUE"), "PENDING"), 
                                                                           ifelse(!is.na(date_completed), "COMPLETE", 
                                                                                  ifelse(date_due<today, ifelse(date_due<longago, "WAY_OVERDUE",  "OVERDUE"),  "INPROGRESS")))))

# count reivews per status per article
nok = reviews %>% group_by(submission_id, round) %>% mutate(n = ifelse(status %in% c("COMPLETE", "INPROGRESS", "OVERDUE", "PENDING_OVERDUE"), 1,0)) %>% summarize(NREVIEW=sum(n)) 
review_status = reviews %>% group_by(submission_id, round, status) %>% summarize(n=n()) %>% spread(key=status, value=n, fill = 0) %>% left_join(nok) 
# select only most recent round per article
review_status = review_status %>% group_by(submission_id) %>% arrange(-round) %>% filter(row_number() == 1) 

known_decisions = tibble(decision=c(8,9), decision_name=c("Review", "Reject"))
known_steps = tibble(step=c(1,2, 3,4), step_name=c("Declined_or_pending", "due", "due", "completed"))
# join em all
report = submissions %>% left_join(titles) %>% left_join(last_decision) %>% left_join(known_decisions) %>% left_join(review_status) %>% arrange(editor, submission_id)
# problems
report %>% filter(is.na(editor) | NREVIEW<2) %>% select(-title)
