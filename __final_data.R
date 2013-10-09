
#######################################################################################################################
#import and merge
t1<-read.csv(file="train.csv")
h1<-read.csv(file="test.csv")

t1$id<-100000+c(1:dim(t1)[1])
h1$ACTION<-(-1)

th1<-rbind(t1, h1)

th1$ACTION0<-th1$ACTION
th1$ACTION<-(1-th1$ACTION0)
th1$y<-th1$ACTION
th1$y[th1$y>1]<-0
th1$ws<-1
th1$ws[th1$ACTION0<0]<-0

th1$dummy<-'A'
th1$split1<-0
th1$split1[th1$ACTION0<0]<-2

#######################################################################################################################
#convert variables into sequential IDs
th1$res_f<-factor(th1$RESOURCE)
th1$res_f_id<-as.integer(th1$res_f)
th1$mgr_f<-factor(th1$MGR_ID)
th1$mgr_f_id<-as.integer(th1$mgr_f)
th1$rocd_f<-factor(th1$ROLE_CODE)
th1$rocd_f_id<-as.integer(th1$rocd_f)
th1$rr1_f<-factor(th1$ROLE_ROLLUP_1)
th1$rr1_f_id<-as.integer(th1$rr1_f)
th1$rr2_f<-factor(th1$ROLE_ROLLUP_2)
th1$rr2_f_id<-as.integer(th1$rr2_f)
th1$rt_f<-factor(th1$ROLE_TITLE)
th1$rt_f_id<-as.integer(th1$rt_f)
th1$rf_f<-factor(th1$ROLE_FAMILY)
th1$rf_f_id<-as.integer(th1$rf_f)
th1$rd_f<-factor(th1$ROLE_DEPTNAME)
th1$rd_f_id<-as.integer(th1$rd_f)
th1$rfd_f<-factor(th1$ROLE_FAMILY_DESC)
th1$rfd_f_id<-as.integer(th1$rfd_f)

th1$mgr_res_cnt<-my.f2cnt(th1, "MGR_ID", "RESOURCE")

th1$mgr_res_f<-as.factor(th1$MGR_ID*10000+th1$RESOURCE)
th1$mgr_res_f_id<-as.integer(th1$mgr_res_f)

#######################################################################################################################
#one way count
mean_t<-with(th1[th1$split1==0,], sum(ACTION)*1.0/length(ACTION))
for(ii in 2:10) {
  print(names(th1)[ii])
  th1$x<-th1[, ii]
  sum1<-sqldf("select x, sum(1) as cnt
              from th1  group by 1 ")
  tmp<-sqldf("select cnt from th1 a left join sum1 b on a.x=b.x")
  th1[, paste(names(th1)[ii], "_cnt", sep="")]<-tmp$cnt
}

#######################################################################################################################
#selected 2-way count
th1$mgr_rocd_cnt<-my.f2cnt(th1, "MGR_ID", "ROLE_CODE")
th1$mgr_rd_cnt<-my.f2cnt(th1, "MGR_ID", "ROLE_DEPTNAME")
th1$mgr_rf_cnt<-my.f2cnt(th1, "MGR_ID", "ROLE_FAMILY")
th1$mgr_rfd_cnt<-my.f2cnt(th1, "MGR_ID", "ROLE_FAMILY_DESC")
th1$mgr_rt_cnt<-my.f2cnt(th1, "MGR_ID", "ROLE_TITLE")
th1$mgr_rr1_cnt<-my.f2cnt(th1, "MGR_ID", "ROLE_ROLLUP_1")
th1$mgr_rr2_cnt<-my.f2cnt(th1, "MGR_ID", "ROLE_ROLLUP_2")

th1$res_rf_cnt<-my.f2cnt(th1, "RESOURCE", "ROLE_FAMILY")
th1$res_rfd_cnt<-my.f2cnt(th1, "RESOURCE", "ROLE_FAMILY_DESC")
th1$res_rt_cnt<-my.f2cnt(th1, "RESOURCE", "ROLE_TITLE")
th1$res_rd_cnt<-my.f2cnt(th1, "RESOURCE", "ROLE_DEPTNAME")
th1$res_rocd_cnt<-my.f2cnt(th1, "RESOURCE", "ROLE_CODE")
th1$res_rr1_cnt<-my.f2cnt(th1, "RESOURCE", "ROLE_ROLLUP_1")
th1$res_rr2_cnt<-my.f2cnt(th1, "RESOURCE", "ROLE_ROLLUP_2")

th1$rr1_rr2_cnt<-my.f2cnt(th1, "ROLE_ROLLUP_1", "ROLE_ROLLUP_2")
th1$rr1_rf_cnt<-my.f2cnt(th1, "ROLE_ROLLUP_1", "ROLE_FAMILY")
th1$rr1_rfd_cnt<-my.f2cnt(th1, "ROLE_ROLLUP_1", "ROLE_FAMILY_DESC")
th1$rr1_rt_cnt<-my.f2cnt(th1, "ROLE_ROLLUP_1", "ROLE_TITLE")
th1$rr1_rd_cnt<-my.f2cnt(th1, "ROLE_ROLLUP_1", "ROLE_DEPTNAME")
th1$rr1_rocd_cnt<-my.f2cnt(th1, "ROLE_ROLLUP_1", "ROLE_CODE")

th1$rr2_rf_cnt<-my.f2cnt(th1, "ROLE_ROLLUP_2", "ROLE_FAMILY")
th1$rr2_rfd_cnt<-my.f2cnt(th1, "ROLE_ROLLUP_2", "ROLE_FAMILY_DESC")
th1$rr2_rt_cnt<-my.f2cnt(th1, "ROLE_ROLLUP_2", "ROLE_TITLE")
th1$rr2_rd_cnt<-my.f2cnt(th1, "ROLE_ROLLUP_2", "ROLE_DEPTNAME")
th1$rr2_rocd_cnt<-my.f2cnt(th1, "ROLE_ROLLUP_2", "ROLE_CODE")

th1$rf_rfd_cnt<-my.f2cnt(th1, "ROLE_FAMILY", "ROLE_FAMILY_DESC")
th1$rf_rt_cnt<-my.f2cnt(th1, "ROLE_FAMILY", "ROLE_TITLE")
th1$rf_rd_cnt<-my.f2cnt(th1, "ROLE_FAMILY", "ROLE_DEPTNAME")
th1$rf_rocd_cnt<-my.f2cnt(th1, "ROLE_FAMILY", "ROLE_CODE")

th1$rfd_rt_cnt<-my.f2cnt(th1, "ROLE_FAMILY_DESC", "ROLE_TITLE")
th1$rfd_rd_cnt<-my.f2cnt(th1, "ROLE_FAMILY_DESC", "ROLE_DEPTNAME")
th1$rfd_rocd_cnt<-my.f2cnt(th1, "ROLE_FAMILY_DESC", "ROLE_CODE")

th1$rt_rd_cnt<-my.f2cnt(th1, "ROLE_TITLE", "ROLE_DEPTNAME")
th1$rt_rocd_cnt<-my.f2cnt(th1, "ROLE_TITLE", "ROLE_CODE")

th1$rd_rocd_cnt<-my.f2cnt(th1, "ROLE_DEPTNAME", "ROLE_CODE")

th1$rd_rf_cnt<-my.f2cnt(th1, "ROLE_DEPTNAME", "ROLE_FAMILY")


#######################################################################################################################
#selected 3 way count
th1$res_mgr_rr2_cnt<-my.f3cnt(th1, "RESOURCE", "MGR_ID", "ROLE_ROLLUP_2")
th1$res_mgr_rd_cnt<-my.f3cnt(th1, "RESOURCE", "MGR_ID", "ROLE_DEPTNAME")
th1$res_mgr_rocd_cnt<-my.f3cnt(th1, "RESOURCE", "MGR_ID", "ROLE_CODE")
th1$res_mgr_rdf_cnt<-my.f3cnt(th1, "RESOURCE", "MGR_ID", "ROLE_FAMILY_DESC")
th1$res_mgr_rf_cnt<-my.f3cnt(th1, "RESOURCE", "MGR_ID", "ROLE_FAMILY")
th1$rf_mgr_rd_cnt<-my.f3cnt(th1, "ROLE_FAMILY", "MGR_ID", "ROLE_DEPTNAME")
th1$rf_mgr_rr2_cnt<-my.f3cnt(th1, "ROLE_FAMILY", "MGR_ID", "ROLE_ROLLUP_2")
th1$res_rd_rf_cnt<-my.f3cnt(th1, "RESOURCE", "ROLE_DEPTNAME", "ROLE_FAMILY")
th1$res_rd_rr2_cnt<-my.f3cnt(th1, "RESOURCE", "ROLE_DEPTNAME", "ROLE_ROLLUP_2")

#######################################################################################################################
#define rid -- combination of all "role" related variables
sum1<-sqldf("select role_rollup_1, role_rollup_2, role_title, role_deptname, role_family, role_family_desc, role_code, count(*) as cnt
            from th1 group by 1,2,3,4,5,6,7")

sum1$role_id<-c(1:dim(sum1)[1])

tmp<-sqldf("select role_id, b.cnt from th1 a join sum1 b on a.role_rollup_1=b.role_rollup_1 and a.role_rollup_2=b.role_rollup_2
           and a.role_title=b.role_title and a.role_deptname=b.role_deptname and a.role_family=b.role_family and a.role_family_desc=b.role_family_desc
           and a.role_code=b.role_code")
th1$rid<-tmp$role_id

th1$rid_res_cnt<-my.f2cnt(th1, "rid", "RESOURCE")
th1$rid_cnt<-my.f2cnt(th1, "rid", "dummy")
th1$all_but_res_cnt<-my.f2cnt(th1, "rid", "MGR_ID")

#######################################################################################################################
#role type id -- combination of some role related vars.
sum1<-sqldf("select role_rollup_1, role_rollup_2, role_title, role_deptname, role_family, count(*) as cnt
            from th1 group by 1,2,3,4,5")

sum1$roletype_id<-c(1:dim(sum1)[1])

tmp<-sqldf("select roletype_id, b.cnt from th1 a join sum1 b on a.role_rollup_1=b.role_rollup_1 and a.role_rollup_2=b.role_rollup_2
           and a.role_title=b.role_title and a.role_deptname=b.role_deptname and a.role_family=b.role_family")
th1$rtype_id<-tmp$roletype_id


th1$rtype_cnt<-my.f2cnt(th1, "rtype_id", "dummy")
th1$all_but_rfd_cnt<-my.f2cnt(th1, "mgr_res_f_id", "rtype_id")

#######################################################################################################################
#some eigen values for 2-way interactions
sum1<-sqldf("select rid, res_f_id, sum(1) as cnt from th1 group by 1, 2")
sm1<-sparseMatrix(i=sum1$res_f_id, j=sum1$rid, x=1)

svd1<-irlba(sm1, nu=5, nv=5, adjust=3)

th1$rid_ev1<-svd1$v[th1$rid,1]
th1$rid_ev2<-svd1$v[th1$rid,2]
th1$rid_ev3<-svd1$v[th1$rid,3]
th1$rid_ev4<-svd1$v[th1$rid,4]
th1$rid_ev5<-svd1$v[th1$rid,5]

th1$res_ev1<-svd1$u[th1$res_f_id,1]
th1$res_ev2<-svd1$u[th1$res_f_id,2]
th1$res_ev3<-svd1$u[th1$res_f_id,3]
th1$res_ev4<-svd1$u[th1$res_f_id,4]
th1$res_ev5<-svd1$u[th1$res_f_id,5]

sum1<-sqldf("select rid, mgr_f_id, sum(1) as cnt from th1 group by 1, 2")
sm1<-sparseMatrix(i=sum1$mgr_f_id, j=sum1$rid, x=1)

svd1<-irlba(sm1, nu=5, nv=5, adjust=3)

th1$rid_mgr_ev1<-svd1$v[th1$rid,1]
th1$rid_mgr_ev2<-svd1$v[th1$rid,2]
th1$rid_mgr_ev3<-svd1$v[th1$rid,3]
th1$rid_mgr_ev4<-svd1$v[th1$rid,4]
th1$rid_mgr_ev5<-svd1$v[th1$rid,5]

th1$mgr_rid_ev1<-svd1$u[th1$mgr_f_id,1]
th1$mgr_rid_ev2<-svd1$u[th1$mgr_f_id,2]
th1$mgr_rid_ev3<-svd1$u[th1$mgr_f_id,3]
th1$mgr_rid_ev4<-svd1$u[th1$mgr_f_id,4]
th1$mgr_rid_ev5<-svd1$u[th1$mgr_f_id,5]

sum1<-sqldf("select res_f_id, mgr_f_id, sum(1) as cnt from th1 group by 1, 2")
sm1<-sparseMatrix(i=sum1$mgr_f_id, j=sum1$res_f_id, x=1)

svd1<-irlba(sm1, nu=5, nv=5, adjust=3)

th1$res_mgr_ev1<-svd1$v[th1$res_f_id,1]
th1$res_mgr_ev2<-svd1$v[th1$res_f_id,2]
th1$res_mgr_ev3<-svd1$v[th1$res_f_id,3]
th1$res_mgr_ev4<-svd1$v[th1$res_f_id,4]
th1$res_mgr_ev5<-svd1$v[th1$res_f_id,5]

th1$mgr_res_ev1<-svd1$u[th1$mgr_f_id,1]
th1$mgr_res_ev2<-svd1$u[th1$mgr_f_id,2]
th1$mgr_res_ev3<-svd1$u[th1$mgr_f_id,3]
th1$mgr_res_ev4<-svd1$u[th1$mgr_f_id,4]
th1$mgr_res_ev5<-svd1$u[th1$mgr_f_id,5]
