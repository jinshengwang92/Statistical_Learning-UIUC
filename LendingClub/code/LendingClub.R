# the code for predicting the lending club on Kaggle
setwd("~/Documents/Classes_taken/STAT542/Rcode/LendingClub/")
raw.train = read.csv('train.csv', header = TRUE)
raw.test  = read.csv('test.csv', header = TRUE)

train_keep_col = c('id','loan_amnt','funded_amnt','term','int_rate','installment','sub_grade','emp_length','home_ownership',
             'annual_inc','verified_status_joint','issue_d','loan_status','pymnt_plan','purpose','dti','delinq_2yrs','earliest_cr_line',
             'inq_last_6mths','mths_since_last_delinq','mths_since_last_record','open_acc','pub_rec','revol_bal','revol_util','total_acc',
             'initial_list_status','out_prncp','out_prncp_inv','total_pymnt','total_pymnt_inv','total_rec_int','total_rec_late_fee',
             'recoveries','collection_recovery_fee','last_pymnt_d','last_pymnt_amnt','last_credit_pull_d','collections_12_mths_ex_med',
             'mths_since_last_major_derog','policy_code','application_type','acc_now_delinq','tot_coll_amt','tot_cur_bal')
test_keep_col = c('id','loan_amnt','funded_amnt','term','int_rate','installment','sub_grade','emp_length','home_ownership',
                   'annual_inc','verified_status_joint','issue_d','pymnt_plan','purpose','dti','delinq_2yrs','earliest_cr_line',
                   'inq_last_6mths','mths_since_last_delinq','mths_since_last_record','open_acc','pub_rec','revol_bal','revol_util','total_acc',
                   'initial_list_status','out_prncp','out_prncp_inv','total_pymnt','total_pymnt_inv','total_rec_int','total_rec_late_fee',
                   'recoveries','collection_recovery_fee','last_pymnt_d','last_pymnt_amnt','last_credit_pull_d','collections_12_mths_ex_med',
                   'mths_since_last_major_derog','policy_code','application_type','acc_now_delinq','tot_coll_amt','tot_cur_bal')
train1 = raw.train[,names(raw.train) %in% train_keep_col]
test1  = raw.test[,names(raw.test) %in% test_keep_col]
new_names = names(train1)
#names(loan) %in% keep_col]


# preprocess to deal with the NA and missing values
# term transfromed into numerical 36 and 60
train1$term = ifelse(as.character(train1$term) == " 36 months", 36, 60)
test1$term = ifelse(as.character(test1$term) == " 36 months", 36, 60)

#subgrade to dummy variables, and the grade should be removed


#emp_length, employment length to dummy variable or months



