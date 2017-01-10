activity_to_str <- function(act_num) {
  act_num[act_num == 1] <- 'WALKING'
  act_num[act_num == 2] <- 'WALKING_UPSTAIRS'
  act_num[act_num == 3] <- 'WALKING_DOWNSTAIRS'
  act_num[act_num == 4] <- 'SITTING'
  act_num[act_num == 5] <- 'STANDING'
  act_num[act_num == 6] <- 'LAYING'
  act_num
}

loadData <- function (filename_data, filename_subj, filename_acts, n=-1) {
  feat <- read.table('features.txt', col.names=c('id', 'name'))
  needed_feat <- grep('(mean[^F]|std)', feat$name)
  d <- read.table(filename_data, col.names=feat$name, nrows=n)[, needed_feat]
  colnames(d) <- gsub('\\.\\.*', '.', colnames(d))
  colnames(d) <- gsub('\\.*$', '', colnames(d))
  
  acts <- read.table(filename_acts, nrows=n)
  d <- cbind(activity_to_str(acts), d)
  colnames(d)[1] <- 'activity'
  
  subs <- read.table(filename_subj, nrows=n)
  d <- cbind(subs, d)
  colnames(d)[1] <- 'subject'
  d
}

computeAverages <- function(sub_act_data) {
  if (nrow(sub_act_data) == 0)
    return(sub_act_data[0,])
  
  res <- sub_act_data[1, ]
  
  for (n in names(sub_act_data)) {
    if (n == 'subject')
      res$subject[1] <- sub_act_data[1, n]
    else if (n == 'activity')
      res$activity[1] <- sub_act_data[1, n]
    else
      res[1, n] <- mean(sub_act_data[, n])
  }
  
  res
}

n <- -1
d <- loadData('test/X_test.txt', 'test/subject_test.txt', 'test/y_test.txt', n)
d <- rbind(d, loadData('train/X_train.txt', 'train/subject_train.txt', 'train/y_train.txt', n))

avrgs <- d[0,]
subs <- unique(d$subject)
acts <- unique(d$activity)
for (subj in subs)
  for (act in acts)
    avrgs <- rbind(avrgs, computeAverages(subset(d, subject == subj & activity == act)))
write.table(avrgs, file='averages.txt', row.names = FALSE)


