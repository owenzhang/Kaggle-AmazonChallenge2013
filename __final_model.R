#######################################################################################################################
#model specification for tree based models (RF, GBM, extraTrees)
mdl_def_tree<-paste("RESOURCE+ROLE_ROLLUP_1+ROLE_ROLLUP_2+ROLE_DEPTNAME+ROLE_TITLE",
                    "+ROLE_FAMILY_DESC+ROLE_FAMILY+MGR_ID+ROLE_CODE",
                    
                    "+RESOURCE_cnt+ROLE_ROLLUP_1_cnt+ROLE_ROLLUP_2_cnt+ROLE_DEPTNAME_cnt+ROLE_TITLE_cnt",
                    "+ROLE_FAMILY_DESC_cnt+ROLE_FAMILY_cnt+MGR_ID_cnt+mgr_res_cnt+ROLE_CODE_cnt",
                    
                    "+res_rf_cnt+res_rfd_cnt+res_rt_cnt+res_rr1_cnt+res_rr2_cnt+res_rd_cnt",
                    "+mgr_rf_cnt+mgr_rfd_cnt+mgr_rt_cnt+mgr_rr1_cnt+mgr_rr2_cnt+mgr_rd_cnt",
                    "+res_mgr_rr2_cnt+res_mgr_rd_cnt+res_rd_rr2_cnt",
                    "+rr1_rr2_cnt+rr1_rf_cnt+rr1_rfd_cnt+rr1_rt_cnt+rr1_rd_cnt",
                    "+rr2_rf_cnt+rr2_rfd_cnt+rr2_rt_cnt+rr2_rd_cnt",
                    "+rf_rfd_cnt+rf_rt_cnt+rf_rd_cnt+rfd_rt_cnt+rfd_rd_cnt",
                    "+rt_rd_cnt+rid_res_cnt",
                    
                    "+exp_res+exp_mgr+exp_rfd+exp_res_rd + exp_res_rt + exp_res_rr2",
                    "+exp_mgr_res + exp_mgr_rt + exp_mgr_rfd +all_but_res_cnt +exp_rid",
                    "+exp_res_rid+ exp_all_but_res"
                    ,
                    
                    "+rid_ev1+rid_ev2+rid_ev3+res_ev1+res_ev2+res_ev3+rid_ev4+rid_ev5+res_ev4+res_ev5",
                    "+rid_mgr_ev1+rid_mgr_ev2+rid_mgr_ev3+mgr_rid_ev1+mgr_rid_ev2+mgr_rid_ev3+rid_mgr_ev4+rid_mgr_ev5+mgr_rid_ev4+mgr_rid_ev5",
                    "+mgr_res_ev1+mgr_res_ev2+mgr_res_ev3+res_mgr_ev1+res_mgr_ev2+res_mgr_ev3+mgr_res_ev4+mgr_res_ev5+res_mgr_ev4+res_mgr_ev5"
)

#######################################################################################################################
#model specification for glmnet
mdl_def_gnet<-paste("y~log(exp_all_but_res)+log(exp_res)+log(exp_rfd)+log(exp_res_rd)+log(exp_rd+exp_mgr)+log(exp_rid)",
                    "+log(exp_mgr_res)+log(exp_rr2)+log(exp_all_but_res)*log(exp_rid)",
                    "+log(exp_rid)+log(exp_rid)*log(exp_rid)+log(exp_res_rr2)+log(exp_res_rr2)*log(exp_res_rr2)",
                    "+log(exp_mgr_rt)+log(exp_mgr_rt)*log(exp_mgr_rt)+log(exp_res_rd)+log(exp_res_rd)*log(exp_res_rd)",
                    "+log(exp_mgr)+log(exp_mgr)*log(exp_mgr)+log(exp_mgr_rfd)+log(exp_mgr_rfd)*log(exp_mgr_rfd)",
                    "+log(exp_rf)+log(exp_rocd)+log(exp_res_rt)+log(exp_res_rid)+log(exp_res_rr2)+log(exp_res_rt)",
                    "+log(res_rd_cnt-0.5)+log(rid_res_cnt)+log(mgr_rt_cnt)",
                    "+log(exp_all_but_res/(1-exp_all_but_res))+log(exp_all_but_res)*log(exp_all_but_res)",
                    "+log(exp_res)*log(exp_res) + log(exp_res)*log(exp_all_but_res) +log(res_rr2_cnt)",
                    "+log(res_rd_cnt-0.5)*log(rid_res_cnt)",
                    "+log(RESOURCE_cnt)++log(RESOURCE_cnt)*+log(RESOURCE_cnt) +log(all_but_res_cnt)",
                    "+log(mgr_res_cnt)+log(mgr_res_cnt)*log(mgr_res_cnt)",
                    "+log(rt_rd_cnt)+log(rt_rd_cnt)*log(rt_rd_cnt)",
                    "+log(mgr_rd_cnt)+log(mgr_rd_cnt)*log(mgr_rd_cnt)",
                    "+log(ROLE_TITLE_cnt)+log(ROLE_TITLE_cnt)*log(ROLE_TITLE_cnt)",
                    "+log(MGR_ID_cnt)+log(MGR_ID_cnt)*log(MGR_ID_cnt)",
                    "+log(ROLE_DEPTNAME_cnt)+log(ROLE_DEPTNAME_cnt)*log(ROLE_DEPTNAME_cnt)",
                    "+log(ROLE_ROLLUP_2_cnt)+log(ROLE_FAMILY_cnt)+log(ROLE_FAMILY_DESC_cnt)+log(ROLE_ROLLUP_1_cnt)",
                    "+log(ROLE_CODE_cnt)+log(rr2_rd_cnt)",
                    "+log(res_mgr_rr2_cnt)+log(rf_rt_cnt)+log(res_rfd_cnt)+log(rf_rt_cnt)",
                    "+log(exp_rd_rr2)",
                    "+rid_ev1+rid_ev2+rid_ev3+res_ev1+res_ev2+res_ev3+rid_ev4+rid_ev5+res_ev4+res_ev5",
                    "+rid_mgr_ev1+rid_mgr_ev2+rid_mgr_ev3+mgr_rid_ev1+mgr_rid_ev2+mgr_rid_ev3+rid_mgr_ev4+rid_mgr_ev5+mgr_rid_ev4+mgr_rid_ev5",
                    "+mgr_res_ev1+mgr_res_ev2+mgr_res_ev3+res_mgr_ev1+res_mgr_ev2+res_mgr_ev3+mgr_res_ev4+mgr_res_ev5+res_mgr_ev4+res_mgr_ev5"
)

#######################################################################################################################
#model specification for tree based models without exp variables
mdl_def_tree_noexp<-paste("RESOURCE+ROLE_ROLLUP_1+ROLE_ROLLUP_2+ROLE_DEPTNAME+ROLE_TITLE",
                          "+ROLE_FAMILY_DESC+ROLE_FAMILY+MGR_ID+ROLE_CODE",
                          
                          "+RESOURCE_cnt+ROLE_ROLLUP_1_cnt+ROLE_ROLLUP_2_cnt+ROLE_DEPTNAME_cnt+ROLE_TITLE_cnt",
                          "+ROLE_FAMILY_DESC_cnt+ROLE_FAMILY_cnt+MGR_ID_cnt+mgr_res_cnt+ROLE_CODE_cnt",
                          
                          "+res_rf_cnt+res_rfd_cnt+res_rt_cnt+res_rr1_cnt+res_rr2_cnt+res_rd_cnt",
                          "+mgr_rf_cnt+mgr_rfd_cnt+mgr_rt_cnt+mgr_rr1_cnt+mgr_rr2_cnt+mgr_rd_cnt",
                          "+res_mgr_rr2_cnt+res_mgr_rd_cnt+res_rd_rr2_cnt",
                          "+rr1_rr2_cnt+rr1_rf_cnt+rr1_rfd_cnt+rr1_rt_cnt+rr1_rd_cnt",
                          "+rr2_rf_cnt+rr2_rfd_cnt+rr2_rt_cnt+rr2_rd_cnt",
                          "+rf_rfd_cnt+rf_rt_cnt+rf_rd_cnt+rfd_rt_cnt+rfd_rd_cnt",
                          "+rt_rd_cnt+rid_res_cnt",
                          "+rid_ev1+rid_ev2+rid_ev3+res_ev1+res_ev2+res_ev3+rid_ev4+rid_ev5+res_ev4+res_ev5",
                          "+rid_mgr_ev1+rid_mgr_ev2+rid_mgr_ev3+mgr_rid_ev1+mgr_rid_ev2+mgr_rid_ev3+rid_mgr_ev4+rid_mgr_ev5+mgr_rid_ev4+mgr_rid_ev5",
                          "+mgr_res_ev1+mgr_res_ev2+mgr_res_ev3+res_mgr_ev1+res_mgr_ev2+res_mgr_ev3+mgr_res_ev4+mgr_res_ev5+res_mgr_ev4+res_mgr_ev5"
)

#sort the dataset so that training obs appear first, necessary for gbm
th1<-th1[order(th1$split1),]

#######################################################################################################################
#create exp variables (leave one out average actual by categorical variable)
r_k<-0.2
th1<-tv_spec_trans(th1)


#######################################################################################################################
#Extreme Randomized Trees 
mt<-model.matrix(as.formula(paste("y~", mdl_def_tree)), data=th1)
ert1<-extraTrees(mt[th1$split1==0,], th1$y[th1$split1==0], mtry=4, nodesize=10, numTreads=4, ntree=2000, numRandomCuts=6)

#######################################################################################################################
#GLMNET 
m1<-model.matrix(as.formula(mdl_def_gnet), data=th1)
filter_t<-th1$split1==0
gnet1<-cv.glmnet(m1[filter_t,],th1$ACTION[filter_t],family="binomial", alpha=0.8, lambda.min.ratio=0.0001)

#######################################################################################################################
#GBM
t_frac<-with(th1, sum(split1==0)/length(split1))
g2<-gbm(as.formula(paste("y~", mdl_def_tree)),weights=th1$ws, #var.monotone=mono,
        data=th1, train.fraction=t_frac, n.trees=1000, interaction.depth=8, n.minobsinnode=20, distribution="bernoulli", 
        shrinkage=0.03, bag.fraction=0.5, keep.data=F, verbose=T)

#######################################################################################################################
#Random Forest
th1$y_factor<-factor(th1$y)
rf2<-randomForest(as.formula(paste("y_factor~", mdl_def_tree))
                  , data=th1, subset=(th1$split1==0), ntree=2000, mtry=3, nodesize=10, replace=F, 
                  , importance=TRUE, do.trace=10, maxnodes=500, sampsize=0.5*sum(th1$split1==0), 
                  xtest=NULL,ytest=NULL,classwt=NULL)

#######################################################################################################################
#noexp GBM@3000
g2_no_exp1<-gbm(as.formula(paste("y~", mdl_def_tree_noexp)), weights=th1$ws, 
           data=th1, train.fraction=t_frac, n.trees=3000, interaction.depth=8, n.minobsinnode=20, distribution="bernoulli", 
           shrinkage=0.03, bag.fraction=0.5, keep.data=F, verbose=T)


#######################################################################################################################
#noexp ert@1000
mt_noexp<-model.matrix(as.formula(paste("y~", mdl_def_tree_noexp)), data=th1)
ert_no_exp1<-extraTrees(mt_noexp[th1$split1==0,], th1$y[th1$split1==0], mtry=3, nodesize=10
                 , numTreads=4, ntree=1000, numRandomCuts=6, evenCuts=F)


#######################################################################################################################
#score all models
th1$pred_ert<-predict(ert1, newdata=mt)
th1$pred_gnet<-predict(gnet1, newx=m1, s="lambda.min", type="response")[,1]
th1$pred_gbm<-predict(g2, newdata=th1, type="response", n.trees=1000)
th1$pred_rf<-predict(rf2, newdata=th1, type="prob")[,2]
th1$pred_gbm_noexp<-1-1/(1+exp(g2_no_exp1$fit))
th1$pred_ert_noexp<-predict(ert_no_exp1, newdata=mt_noexp)

save(ert1, gnet1, g2, rf2, g2_no_exp1, ert_no_exp1, file="__final_models.RData")

#######################################################################################################################
#create ensemble -- weights determined by cross validation
th1$pred<-with(th1, 1-((pred_gbm*1+pred_ert*.5+pred_gnet*.3+pred_rf*.3)/2.1+0.15*pred_gbm_noexp+0.15*pred_ert_noexp)/1.3)
sub_final<-th1[th1$id<100000, c("id", "pred")]
names(sub_final)[2]<-"ACTION"
write.csv(sub_final, file="__final_sub.csv", row.names=F)
