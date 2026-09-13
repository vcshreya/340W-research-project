# #################### ----------------------------------------------------
# get.dfs -----------------------------------------------------------------

df.reliability = read_xlsx("0.Final.BDNF.Data.2024.xlsx", "Reliability")
save(df.reliability, file = "/Users/zacrobinson/Documents/BDNF Benitez (2024)/Data Frames/df.reliability.RData")

df.reliability.minus = df.reliability %>% filter(condition.rpe != "RPE 7-9+")
save(df.reliability.minus, file = "/Users/zacrobinson/Documents/BDNF Benitez (2024)/Data Frames/df.reliability.minus.RData")

df.resting.wide.time = read_xlsx("0.Final.BDNF.Data.2024.xlsx", "Resting Wide (Time)")
save(df.resting.wide.time, file = "/Users/zacrobinson/Documents/BDNF Benitez (2024)/Data Frames/df.resting.wide.time.RData")

df.resting.wide.time.minus = df.resting.wide.time %>% filter(condition.rpe != "RPE 7-9+")
save(df.resting.wide.time.minus, file = "/Users/zacrobinson/Documents/BDNF Benitez (2024)/Data Frames/df.resting.wide.time.minus.RData")

df.resting.long = read_xlsx("0.Final.BDNF.Data.2024.xlsx", "Resting Long")
save(df.resting.long, file = "/Users/zacrobinson/Documents/BDNF Benitez (2024)/Data Frames/df.resting.long.RData")

df.resting.long.minus = df.resting.long %>% filter(condition.rpe != "RPE 7-9+")
save(df.resting.long.minus, file = "/Users/zacrobinson/Documents/BDNF Benitez (2024)/Data Frames/df.resting.long.minus.RData")

df.resting.wide.well = read_xlsx("0.Final.BDNF.Data.2024.xlsx", "Resting Wide (Well)")
save(df.resting.wide.well, file = "/Users/zacrobinson/Documents/BDNF Benitez (2024)/Data Frames/df.resting.wide.well.RData")

df.resting.wide.well.minus = df.resting.wide.well %>% filter(condition.rpe != "RPE 7-9+")
save(df.resting.wide.well.minus, file = "/Users/zacrobinson/Documents/BDNF Benitez (2024)/Data Frames/df.resting.wide.well.minus.RData")

df.acute.wide.time = read_xlsx("0.Final.BDNF.Data.2024.xlsx", "Acute Wide (Time)")
save(df.acute.wide.time, file = "/Users/zacrobinson/Documents/BDNF Benitez (2024)/Data Frames/df.acute.wide.time.RData")

df.acute.wide.time.minus = df.acute.wide.time %>% filter(condition.rpe != "RPE 7-9+")
save(df.acute.wide.time.minus, file = "/Users/zacrobinson/Documents/BDNF Benitez (2024)/Data Frames/df.acute.wide.time.minus.RData")

df.acute.long = read_xlsx("0.Final.BDNF.Data.2024.xlsx", "Acute Long")
save(df.acute.long, file = "/Users/zacrobinson/Documents/BDNF Benitez (2024)/Data Frames/df.acute.long.RData")

df.acute.long.minus = df.acute.long %>% filter(condition.rpe != "RPE 7-9+")
save(df.acute.long.minus, file = "/Users/zacrobinson/Documents/BDNF Benitez (2024)/Data Frames/df.acute.long.minus.RData")

df.acute.wide.well = read_xlsx("0.Final.BDNF.Data.2024.xlsx", "Acute Wide (Well)")
save(df.acute.wide.well, file = "/Users/zacrobinson/Documents/BDNF Benitez (2024)/Data Frames/df.acute.wide.well.RData")

df.acute.wide.well.minus = df.acute.wide.well %>% filter(condition.rpe != "RPE 7-9+")
save(df.acute.wide.well.minus, file = "/Users/zacrobinson/Documents/BDNF Benitez (2024)/Data Frames/df.acute.wide.well.minus.RData")

# get.sem -----------------------------------------------------------------
get.sem = function(data.frame, marker) {
  ## load necessary packages if not loaded
  if(!require("SimplyAgree", character.only = TRUE))
  {library("SimplyAgree", character.only = TRUE)}
  
  ## create entries
  column.1 = paste0(marker,".od1")
  column.2 = paste0(marker,".od2")
  
  ## get sem
  sem=reli_aov(data = data.frame,
           wide = TRUE,
           col.names = c(column.1,
                         column.2))$SEM$estimate
  
  ## save for later use
  save(sem, file = paste0("RData/",marker,".sem.RData"))
  
  ## make data frame
  sem.df=data.frame(marker=marker,
                sem=sem)
  
  ## assign to enviroment
  assign(paste0(marker,".sem.df"), sem.df, envir = .GlobalEnv)
  
  ## return
  return(sem.df)
}
# save function
save(get.sem, file = "Functions/get.sem.RData")

# get.axis.limits ---------------------------------------------------------

get.axis.limits = function(data.frame,
                           y.column,
                           x.column = NULL,
                           y.only = TRUE) {
  
  if(y.only == TRUE){x.limits = NULL}else{
  x.limits = c(
    data.frame%>%select(all_of(x.column))%>%min(.,na.rm = TRUE),
    data.frame%>%select(all_of(x.column))%>%max(.,na.rm = TRUE)
  )
}

  y.limits = c(
    data.frame%>%select(all_of(y.column))%>%min(.,na.rm = TRUE),
    data.frame%>%select(all_of(y.column))%>%max(.,na.rm = TRUE)
  )
  
  if(y.only == TRUE){axis.limits = y.limits}else{
    axis.limits = c(
      min(c(x.limits,y.limits)),
      max(c(x.limits,y.limits))
    )
  }
  
  return(axis.limits)
  
}
save(get.axis.limits, file = "Functions/get.axis.limits.RData")

# plot.reliability --------------------------------------------------------

plot.reliability = function(data.frame, 
                            marker,
                            width = 5.25,
                            height = 5){
  
  ## load necessary packages if not loaded
  if(!require("tidyverse", character.only = TRUE))
  {library("tidyverse", character.only = TRUE)}
  
  ## load overall aesthetics
  load(file = "RData/THEME.RData")
  
  ## make column names
  column.1=paste0(marker,".od1")
  column.2=paste0(marker,".od2")
  
  x=column.1%>%as.name()
  y=column.2%>%as.name()
  
  ## get better axis limits
  limits=get.axis.limits(data.frame,
                         x.column = column.1,
                         y.column = column.2,
                         y.only = FALSE)

  
  if (marker == "bdnf") {
    ylab <-bquote(bold("BDNF Well 2 ng·ml"^"-1"))
  } else if (marker == "catb") {
    ylab <- bquote(bold("CatB Well 2 ng·ml"^"-1"))
  } else if (marker == "igf1") {
    ylab <- bquote(bold("IGF-1 Well 2 ng·ml"^"-1"))
  } else {
    ylab <- bquote(bold("IL-6 Well 2 pg·ml"^"-1"))
  }
  
  
  if (marker == "bdnf") {
    xlab <- bquote(bold("BDNF Well 1 ng·ml"^"-1"))
  } else if (marker == "catb") {
    xlab <- bquote(bold("CatB Well 1 ng·ml"^"-1"))
  } else if (marker == "igf1") {
    xlab <- bquote(bold("IGF-1 Well 1 ng·ml"^"-1"))
  } else {
    xlab <- bquote(bold("IL-6 Well 1 pg·ml"^"-1"))
  }
  
  if (marker == "bdnf") {
    title <- bquote(bold("BDNF Test-Retest Reliability"))
  } else if (marker == "catb") {
    title <- bquote(bold("CatB Test-Retest Reliability"))
  } else if (marker == "igf1") {
    title <- bquote(bold("IGF-1 Test-Retest Reliability"))
  } else {
    title <- bquote(bold("IL-6 Test-Retest Reliability"))
  }
  
  
  
  ## create plot
  plot=ggplot(data = data.frame,
              aes(x=!!sym(x),
                  y=!!sym(y)))+
    THEME+
    geom_abline(slope = 1,
                intercept = 0,
                alpha = 0.5)+
    geom_point(size = 3)+
    labs(x = xlab,
         y = ylab,
         title = title)+
    ylim(limits=limits*c(-1,1))+
    xlim(limits=limits*c(-1,1))
  
  ## save
  ggsave(plot,
         device = "pdf",
         filename = paste0("Plots/",marker,".reliability.plot.pdf"),
         width = width,
         height = height)
  
  save(plot,file = paste0("RData/",marker,".reliability.plot.RData"))
  
  ## assign to enviroment
  assign(paste0(marker,".reliability.plot"), plot, envir = .GlobalEnv)
  
  return(plot)
  
}
save(plot.reliability, file = "Functions/plot.reliability.RData")



# plot.linearity ----------------------------------------------------------

plot.linearity = function(data.frame, marker, type){

if(type == "acute"){  
  
  if (marker == "bdnf") {
    ylab <- bquote(bold("Raw Acute Change in BDNF ng·ml"^"-1"))
  } else if (marker == "catb") {
    ylab <- bquote(bold("Raw Acute Change in CatB ng·ml"^"-1"))
  } else if (marker == "igf1") {
    ylab <- bquote(bold("Raw Acute Change in IGF-1 ng·ml"^"-1"))
  } else {
    ylab <- bquote(bold("Raw Acute Change in IL-6 pg·ml"^"-1"))
  }

p=ggplot(data = data.frame,
       aes(x=avg.rpe,
           y=!!sym(paste0(marker,"_0_1"))-!!sym(paste0(marker,"_0_0")),
           color = condition.rir))+
  theme_classic()+
  geom_point()+
  geom_hline(yintercept = 0,linetype="dotted",alpha=0.5)+
  geom_smooth(aes(group = NA),
              se = FALSE,
              color = "blue")+
  geom_smooth(aes(group = NA),
              se = FALSE,
              color = "red",
              method = "lm")+
  labs(y = ylab,
       x = bquote(bold("RIR-Based-RPE")),
       color = bquote(bold("Condition")),
       title = bquote(bold("Week 1")))+
  
  ggplot(data = data.frame,
         aes(x=avg.rpe,
             y=!!sym(paste0(marker,"_1_1"))-!!sym(paste0(marker,"_1_0")),
             color = condition.rir))+
  theme_classic()+
  geom_point()+
  geom_hline(yintercept = 0,linetype="dotted",alpha=0.5)+
  geom_smooth(aes(group = NA),
              se = FALSE,
              color = "blue")+
  geom_smooth(aes(group = NA),
              se = FALSE,
              color = "red",
              method = "lm")+
  labs(y = ylab,
       x = bquote(bold("RIR-Based-RPE")),
       color = bquote(bold("Condition")),
       title = bquote(bold("Week 7")))+
  
  plot_layout(guides = "collect",
              axes = "collect")

return(p)
}
else{

  if (marker == "bdnf") {
    ylab <- bquote(bold("Raw Chronic Change in BDNF ng·ml"^"-1"))
  } else if (marker == "catb") {
    ylab <- bquote(bold("Raw Chronic Change in CatB ng·ml"^"-1"))
  } else if (marker == "igf1") {
    ylab <- bquote(bold("Raw Chronic Change in IGF-1 ng·ml"^"-1"))
  } else {
    ylab <- bquote(bold("Raw Chronic Change in IL-6 pg·ml"^"-1"))
  }
  
  p=ggplot(data = data.frame,
           aes(x=avg.rpe,
               y=!!sym(paste0(marker,"_1"))-!!sym(paste0(marker,"_0")),
               color = condition.rir))+
    theme_classic()+
    geom_point()+
    geom_hline(yintercept = 0,linetype="dotted",alpha=0.5)+
    geom_smooth(aes(group = NA),
                se = FALSE,
                color = "blue")+
    geom_smooth(aes(group = NA),
                se = FALSE,
                color = "red",
                method = "lm")+
    labs(y = ylab,
         x = bquote(bold("RIR-Based-RPE")),
         color = bquote(bold("Condition")),
         title = bquote(bold("Chronic Change")))
  
  return(p)  
  
}  
}

save(plot.linearity, file = "Functions/plot.linearity.RData")

# get.acute.effects -------------------------------------------------------

get.acute.effects = function(model, data.frame, marker, all.data = FALSE){

  ## session main effect
  session.preds=model%>%
    predictions(
      newdata = datagrid(
        session = 0:1,
        week = 0:1,
        avg.rpe = mean,
        id = NA
      ),
      by = "session",
      re_formula = NA,
      transform = function(x){(x*sd(model$data[,marker],na.rm=TRUE)+
                                 mean(model$data[,marker],na.rm=TRUE))})%>%
    suppressMessages()
  
  session.draws=session.preds%>%
    posterior_draws(shape = "DxP")%>%
    brms::hypothesis("b2-b1=0")%>%
    .$samples%>%
    rename(draw=H1)
  
  session.probs=session.draws%>%
    reframe(.null=ifelse(mean(draw)>0,mean(draw>0)*100,mean(draw<0)*100),
            .rope=ifelse(mean(draw)>0,mean(draw>get(paste0(marker,".sem.df"))$sem)*100,
                         mean(draw<(-get(paste0(marker,".sem.df"))$sem)*100)))%>%
    unique()
  
  session.est=session.draws%>%
    mode_hdci(.value = draw)%>%
    mutate(.sem = get(paste0(marker,".sem.df"))$sem,
           .null = session.probs$.null,
           .rope = session.probs$.rope,
           .marker = marker,
           .estimate = "Session")%>%
    relocate(.marker,.estimate) 
  
  ## week main effect
  week.preds=model%>%
    predictions(
      newdata = datagrid(
        session = 0:1,
        week = 0:1,
        avg.rpe = mean,
        id = NA
      ),
      by = "week",
      re_formula = NA,
      transform = function(x){(x*sd(model$data[,marker],na.rm=TRUE)+
                                 mean(model$data[,marker],na.rm=TRUE))}
    )%>%
    suppressMessages()
  
  week.draws=week.preds%>%
    posterior_draws(shape = "DxP")%>%
    brms::hypothesis("b2-b1=0")%>%
    .$samples%>%
    rename(draw=H1)
  
  week.probs=week.draws%>%
    reframe(.null=ifelse(mean(draw)>0,mean(draw>0)*100,mean(draw<0)*100),
            .rope=ifelse(mean(draw)>0,mean(draw>get(paste0(marker,".sem.df"))$sem)*100,
                         mean(draw<(-get(paste0(marker,".sem.df"))$sem)*100)))%>%
    unique()
  
  week.est=week.draws%>%
    mode_hdci(.value = draw)%>%
    mutate(.sem = get(paste0(marker,".sem.df"))$sem,
           .null = week.probs$.null,
           .rope = week.probs$.rope,
           .marker = marker,
           .estimate = "Week")%>%
    relocate(.marker,.estimate) 
  
  ## session x week interaction
  session.by.week.preds=model%>%
    predictions(
      newdata = datagrid(
        session = 0:1,
        week = 0:1,
        avg.rpe = mean,
        id = NA
      ),
      by = c("session","week"),
      re_formula = NA,
      transform = function(x){(x*sd(model$data[,marker],na.rm=TRUE)+
                                 mean(model$data[,marker],na.rm=TRUE))}
    )%>%
    suppressMessages()
  
  session.by.week.draws=session.by.week.preds%>%
    posterior_draws(shape = "DxP")%>%
    brms::hypothesis("(b4-b2)-(b3-b1)=0")%>%
    .$samples%>%
    rename(draw=H1)
  
  session.by.week.probs=session.by.week.draws%>%
    reframe(.null=ifelse(mean(draw)>0,mean(draw>0)*100,mean(draw<0)*100),
            .rope=ifelse(mean(draw)>0,mean(draw>get(paste0(marker,".sem.df"))$sem)*100,
                         mean(draw<(-get(paste0(marker,".sem.df"))$sem)*100)))%>%
    unique()
  
  session.by.week.est=session.by.week.draws%>%
    mode_hdci(.value = draw)%>%
    mutate(.sem = get(paste0(marker,".sem.df"))$sem,
           .null = session.by.week.probs$.null,
           .rope = session.by.week.probs$.rope,
           .marker = marker,
           .estimate = "Session x Week")%>%
    relocate(.marker,.estimate) 
  
  ## session x rpe interaction
  session.by.rpe.preds=model%>%
    predictions(
      newdata = datagrid(
        session = 0:1,
        week = 0:1,
        avg.rpe = c(mean(model$data[,"avg.rpe"],na.rm=TRUE)-0.5,
                    mean(model$data[,"avg.rpe"],na.rm=TRUE)+0.5),
        id = NA
      ),
      by = c("session","avg.rpe"),
      re_formula = NA,
      transform = function(x){(x*sd(model$data[,marker],na.rm=TRUE)+
                                 mean(model$data[,marker],na.rm=TRUE))}
    )%>%
    suppressMessages()
  
  session.by.rpe.draws=session.by.rpe.preds%>%
    posterior_draws(shape = "DxP")%>%
    brms::hypothesis("(b4-b2)-(b3-b1)=0")%>%
    .$samples%>%
    rename(draw=H1)
  
  session.by.rpe.probs=session.by.rpe.draws%>%
    reframe(.null=ifelse(mean(draw)>0,mean(draw>0)*100,mean(draw<0)*100),
            .rope=ifelse(mean(draw)>0,mean(draw>get(paste0(marker,".sem.df"))$sem)*100,
                         mean(draw<(-get(paste0(marker,".sem.df"))$sem)*100)))%>%
    unique()
  
  session.by.rpe.est=session.by.rpe.draws%>%
    mode_hdci(.value = draw)%>%
    mutate(.sem = get(paste0(marker,".sem.df"))$sem,
           .null = session.by.rpe.probs$.null,
           .rope = session.by.rpe.probs$.rope,
           .marker = marker,
           .estimate = "Session x RPE")%>%
    relocate(.marker,.estimate)
  
  ## week x rpe interaction
  week.by.rpe.preds=model%>%
    predictions(
      newdata = datagrid(
        session = 0:1,
        week = 0:1,
        avg.rpe = c(mean(model$data[,"avg.rpe"],na.rm=TRUE)-0.5,
                    mean(model$data[,"avg.rpe"],na.rm=TRUE)+0.5),
        id = NA
      ),
      by = c("week","avg.rpe"),
      re_formula = NA,
      transform = function(x){(x*sd(model$data[,marker],na.rm=TRUE)+
                                 mean(model$data[,marker],na.rm=TRUE))}
    )%>%
    suppressMessages()
  
  week.by.rpe.draws=week.by.rpe.preds%>%
    posterior_draws(shape = "DxP")%>%
    brms::hypothesis("(b4-b2)-(b3-b1)=0")%>%
    .$samples%>%
    rename(draw=H1)
  
  week.by.rpe.probs=week.by.rpe.draws%>%
    reframe(.null=ifelse(mean(draw)>0,mean(draw>0)*100,mean(draw<0)*100),
            .rope=ifelse(mean(draw)>0,mean(draw>get(paste0(marker,".sem.df"))$sem)*100,
                         mean(draw<(-get(paste0(marker,".sem.df"))$sem)*100)))%>%
    unique()
  
  week.by.rpe.est=week.by.rpe.draws%>%
    mode_hdci(.value = draw)%>%
    mutate(.sem = get(paste0(marker,".sem.df"))$sem,
           .null = week.by.rpe.probs$.null,
           .rope = week.by.rpe.probs$.rope,
           .marker = marker,
           .estimate = "Week x RPE")%>%
    relocate(.marker,.estimate)
  
  ## session x week x rpe interaction
  session.by.week.by.rpe.preds=model%>%
    predictions(
      newdata = datagrid(
        session = 0:1,
        week = 0:1,
        avg.rpe = c(mean(model$data[,"avg.rpe"],na.rm=TRUE)-0.5,
                    mean(model$data[,"avg.rpe"],na.rm=TRUE)+0.5),
        id = NA
      ),
      by = c("session","week","avg.rpe"),
      re_formula = NA,
      transform = function(x){(x*sd(model$data[,marker],na.rm=TRUE)+
                                 mean(model$data[,marker],na.rm=TRUE))}
    )%>%
    suppressMessages()
  
  session.by.week.by.rpe.draws=session.by.week.by.rpe.preds%>%
    posterior_draws(shape = "DxP")%>%
    brms::hypothesis("((b8-b4)-(b6-b2))-((b7-b3)-(b5-b1))=0")%>%
    .$samples%>%
    rename(draw=H1)
  
  session.by.week.by.rpe.probs=session.by.week.by.rpe.draws%>%
    reframe(.null=ifelse(mean(draw)>0,mean(draw>0)*100,mean(draw<0)*100),
            .rope=ifelse(mean(draw)>0,mean(draw>get(paste0(marker,".sem.df"))$sem)*100,
                         mean(draw<(-get(paste0(marker,".sem.df"))$sem)*100)))%>%
    unique()
  
  session.by.week.by.rpe.est=session.by.week.by.rpe.draws%>%
    mode_hdci(.value = draw)%>%
    mutate(.sem = get(paste0(marker,".sem.df"))$sem,
           .null = session.by.week.by.rpe.probs$.null,
           .rope = session.by.week.by.rpe.probs$.rope,
           .marker = marker,
           .estimate = "Session x Week x RPE")%>%
    relocate(.marker,.estimate)
  
  ## combine estimates
  estimates.df=rbind(
    session.est,
    week.est,
    session.by.week.est,
    session.by.rpe.est,
    week.by.rpe.est,
    session.by.week.by.rpe.est)
  
  assign(paste0(marker,".acute.estimates.df"),estimates.df,.GlobalEnv)
  save(estimates.df,file = paste0("RData/",marker,".acute.estimates.df.RData"))
  
  if(all.data == FALSE){
    
  rpe = data.frame%>%
      group_by(condition.rir)%>%
      summarise(rpe=mean(avg.rpe,na.rm=TRUE))%>%
      .$rpe%>%
      .[c(2,3,1)]  
    
  # estimates for plot
  plot.draws=model%>%
    predictions(
      newdata = datagrid(
        session = 0:1,
        week = 0:1,
        avg.rpe = rpe,
        id = NA
      ),
      by = c("session","week","avg.rpe"),
      re_formula = NA,
      transform = function(x){(x*sd(model$data[,marker],na.rm=TRUE)+
                                 mean(model$data[,marker],na.rm=TRUE))})%>%
    suppressMessages()%>%
    posterior_draws()%>%
    mutate(condition.rir=ifelse(avg.rpe==rpe[1],"4-6 RIR",
                            ifelse(avg.rpe==rpe[2],"1-3 RIR",
                                   "0 RIR")),
           week.label=ifelse(week==0,"Week 1","Week 7"),
           session.label=ifelse(session==0,"Pre","Post"))
  
  assign(paste0(marker,".acute.plot.draws"),plot.draws,.GlobalEnv)
  save(plot.draws,file = paste0("RData/",marker,".acute.plot.draws.RData"))
  
  return(estimates.df)
  
  }else{
    
    rpe = data.frame%>%
      group_by(condition.rir)%>%
      summarise(rpe=mean(avg.rpe,na.rm=TRUE))%>%
      .$rpe%>%
      .[c(2,3,4,1)]  
    
    plot.draws=model%>%
      predictions(
        newdata = datagrid(
          session = 0:1,
          week = 0:1,
          avg.rpe = rpe,
          id = NA
        ),
        by = c("session","week","avg.rpe"),
        re_formula = NA,
        transform = function(x){(x*sd(model$data[,marker],na.rm=TRUE)+
                                   mean(model$data[,marker],na.rm=TRUE))})%>%
      suppressMessages()%>%
      posterior_draws()%>%
      mutate(condition.rir=ifelse(avg.rpe==rpe[1],"4-6 RIR",
                              ifelse(avg.rpe==rpe[2],"1-3 RIR",
                                     ifelse(avg.rpe==rpe[3],"0-3 RIR",
                                     "0 RIR"))),
             week.label=ifelse(week==0,"Week 1","Week 7"),
             session.label=ifelse(session==0,"Pre","Post"))
    
    assign(paste0(marker,".acute.plot.draws.all.data"),plot.draws,.GlobalEnv)
    save(plot.draws,file = paste0("RData/",marker,".acute.plot.draws.all.data.RData"))
    
    return(estimates.df)
    
  }
  
  
}

save(get.acute.effects, file = "Functions/get.acute.effects.RData")

# plot.acute.effects ------------------------------------------------------

plot.acute.effects = function(draws,
                              data.frame,
                              marker,
                              width = 8,
                              height = 5.6,
                              all.data = FALSE){

  if (marker == "bdnf") {
    ylab <- bquote(bold("BDNF ng·ml"^"-1"))
  } else if (marker == "catb") {
    ylab <- bquote(bold("CatB ng·ml"^"-1"))
  } else if (marker == "igf1") {
    ylab <- bquote(bold("IGF-1 ng·ml"^"-1"))
  } else {
    ylab <- bquote(bold("IL-6 pg·ml"^"-1"))
  }
  
  if (marker == "bdnf") {
    title <- bquote(bold("Acute Effects for BDNF"))
  } else if (marker == "catb") {
    title <- bquote(bold("Acute Effects for CatB"))
  } else if (marker == "igf1") {
    title <- bquote(bold("Acute Effects for IGF-1"))
  } else {
    title <- bquote(bold("Acute Effects for IL-6"))
  }
  
  data.frame=data.frame%>%
    mutate(week.label=ifelse(week==0,"Week 1","Week 7"))
  
  if(all.data == FALSE){
  
  plot=ggplot(data = draws,
         aes(x=session,
             y=draw,
             color=condition.rir,
             fill=condition.rir))+
    facet_grid(week.label~fct_relevel(condition.rir,
                                      "4-6 RIR",
                                      "1-3 RIR",
                                      "0 RIR"))+
    THEME+
      geom_line(data = data.frame,
                aes(y=!!sym(marker),
                    x=session,
                    group=interaction(id,week)),
                stat = "summary",
                linewidth=0.5,
                alpha = 0.125,
                position = position_dodge2(.1))+
      geom_point(data = data.frame,
                 aes(y=!!sym(marker),
                     x=session,
                     group=interaction(id,week)),
                 stat = "summary",
                 alpha = 0.25,
                 shape = 16,
                 position = position_dodge2(.1))+
    stat_lineribbon(point_interval = "mode_hdci",
                    color="transparent",
                    alpha=0.25,
                    linewidth=1)+
    stat_lineribbon(point_interval = "mode_hdci",
                    fill="transparent",
                    linewidth=1)+
    scale_x_continuous(limits = c(-0.25,1.25),
                       breaks = c(0,1),
                       labels = unique(draws$session.label))+
    scale_color_manual(values = c("4-6 RIR"= "darkgreen",
                                  "1-3 RIR"= "goldenrod2",
                                  "0 RIR"= "red3"))+
    scale_fill_manual(values = c("4-6 RIR"= "darkgreen",
                                 "1-3 RIR"= "goldenrod2",
                                 "0 RIR"= "red3"))+
    labs(y = ylab,
         x = bquote(bold("Time Relative to Exercise")),
         title = title)
  
  ## save
  ggsave(plot,
         device = "pdf",
         filename = paste0("Plots/",marker,".acute.effects.plot.pdf"),
         width = width,
         height = height)
  
  save(plot,file = paste0("RData/",marker,".acute.effects.plot.RData"))
  
  ## assign to enviroment
  assign(paste0(marker,".acute.effects.plot"), plot, envir = .GlobalEnv)
  
  return(plot)
    
  }else{
    
    plot=ggplot(data = draws,
           aes(x=session,
               y=draw,
               color=condition.rir,
               fill=condition.rir))+
      facet_grid(week.label~fct_relevel(condition.rir,
                                        "4-6 RIR",
                                        "1-3 RIR",
                                        "0-3 RIR",
                                        "0 RIR"))+
      THEME+
      geom_line(data = data.frame,
                aes(y=!!sym(marker),
                    x=session,
                    group=interaction(id,week)),
                stat = "summary",
                linewidth=0.5,
                alpha = 0.125,
                position = position_dodge2(.1))+
      geom_point(data = data.frame,
                 aes(y=!!sym(marker),
                     x=session,
                     group=interaction(id,week)),
                 stat = "summary",
                 alpha = 0.25,
                 shape = 16,
                 position = position_dodge2(.1))+
      stat_lineribbon(point_interval = "mode_hdci",
                      color="transparent",
                      alpha=0.25,
                      linewidth=1)+
      stat_lineribbon(point_interval = "mode_hdci",
                      fill="transparent",
                      linewidth=1)+
      scale_x_continuous(limits = c(-0.25,1.25),
                         breaks = c(0,1),
                         labels = unique(draws$session.label))+
      scale_color_manual(values = c("4-6 RIR"= "darkgreen",
                                    "1-3 RIR"= "goldenrod2",
                                    "0-3 RIR"= "darkorange",
                                    "0 RIR"= "red3"))+
      scale_fill_manual(values = c("4-6 RIR"= "darkgreen",
                                   "1-3 RIR"= "goldenrod2",
                                   "0-3 RIR"= "darkorange",
                                   "0 RIR"= "red3"))+
      labs(y = ylab,
           x = bquote(bold("Time Relative to Exercise")),
           title = title)
    
    ## save
    ggsave(plot,
           device = "pdf",
           filename = paste0("Plots/",marker,".acute.effects.plot.all.data.pdf"),
           width = width,
           height = height)
    
    save(plot,file = paste0("RData/",marker,".acute.effects.plot.all.data.RData"))
    
    ## assign to environment
    assign(paste0(marker,".acute.effects.plot.all.data"), plot, envir = .GlobalEnv)
    
    return(plot)
    
  }    
    
  }
  save(plot.acute.effects, file = "Functions/plot.acute.effects.RData")
  
# get.chronic.effects -----------------------------------------------------

get.chronic.effects = function(model, data.frame, marker, all.data = FALSE){
    
    ## time main effect
    time.preds=model%>%
      predictions(
        newdata = datagrid(
          time = 0:1,
          avg.rpe = mean,
          id = NA
        ),
        by = "time",
        re_formula = NA,
        transform = function(x){(x*sd(model$data[,marker],na.rm=TRUE)+
                                   mean(model$data[,marker],na.rm=TRUE))})%>%
      suppressMessages()
    
    time.draws=time.preds%>%
      posterior_draws(shape = "DxP")%>%
      brms::hypothesis("b2-b1=0")%>%
      .$samples%>%
      rename(draw=H1)
    
    time.probs=time.draws%>%
      reframe(.null=ifelse(mean(draw)>0,mean(draw>0)*100,mean(draw<0)*100),
              .rope=ifelse(mean(draw)>0,mean(draw>get(paste0(marker,".sem.df"))$sem)*100,
                           mean(draw<(-get(paste0(marker,".sem.df"))$sem)*100)))%>%
      unique()
    
    time.est=time.draws%>%
      mode_hdci(.value = draw)%>%
      mutate(.sem = get(paste0(marker,".sem.df"))$sem,
             .null = time.probs$.null,
             .rope = time.probs$.rope,
             .marker = marker,
             .estimate = "Time")%>%
      relocate(.marker,.estimate) 
    
    
    ## time x rpe interaction
    time.by.rpe.preds=model%>%
      predictions(
        newdata = datagrid(
          time = 0:1,
          avg.rpe = c(mean(model$data[,"avg.rpe"],na.rm=TRUE)-0.5,
                      mean(model$data[,"avg.rpe"],na.rm=TRUE)+0.5),
          id = NA
        ),
        by = c("time","avg.rpe"),
        re_formula = NA,
        transform = function(x){(x*sd(model$data[,marker],na.rm=TRUE)+
                                   mean(model$data[,marker],na.rm=TRUE))}
      )%>%
      suppressMessages()
    
    time.by.rpe.draws=time.by.rpe.preds%>%
      posterior_draws(shape = "DxP")%>%
      brms::hypothesis("(b4-b2)-(b3-b1)=0")%>%
      .$samples%>%
      rename(draw=H1)
    
    time.by.rpe.probs=time.by.rpe.draws%>%
      reframe(.null=ifelse(mean(draw)>0,mean(draw>0)*100,mean(draw<0)*100),
              .rope=ifelse(mean(draw)>0,mean(draw>get(paste0(marker,".sem.df"))$sem)*100,
                           mean(draw<(-get(paste0(marker,".sem.df"))$sem)*100)))%>%
      unique()
    
    time.by.rpe.est=time.by.rpe.draws%>%
      mode_hdci(.value = draw)%>%
      mutate(.sem = get(paste0(marker,".sem.df"))$sem,
             .null = time.by.rpe.probs$.null,
             .rope = time.by.rpe.probs$.rope,
             .marker = marker,
             .estimate = "Time x RPE")%>%
      relocate(.marker,.estimate)
    
    ## combine estimates
    estimates.df=rbind(
      time.est,
      time.by.rpe.est)
    
    assign(paste0(marker,".chronic.estimates.df"),estimates.df,.GlobalEnv)
    save(estimates.df,file = paste0("RData/",marker,".chronic.estimates.df.RData"))
    
    if(all.data == FALSE){
      
      rpe = data.frame%>%
        group_by(condition.rir)%>%
        summarise(rpe=mean(avg.rpe,na.rm=TRUE))%>%
        .$rpe%>%
        .[c(2,3,1)]  
      
      # estimates for plot
      plot.draws=model%>%
        predictions(
          newdata = datagrid(
            time = 0:1,
            avg.rpe = rpe,
            id = NA
          ),
          by = c("time","avg.rpe"),
          re_formula = NA,
          transform = function(x){(x*sd(model$data[,marker],na.rm=TRUE)+
                                     mean(model$data[,marker],na.rm=TRUE))})%>%
        suppressMessages()%>%
        posterior_draws()%>%
        mutate(condition.rir=ifelse(avg.rpe==rpe[1],"4-6 RIR",
                                ifelse(avg.rpe==rpe[2],"1-3 RIR",
                                       "0 RIR")),
               time.label=ifelse(time==0,"Pre","Post"))
      
      assign(paste0(marker,".chronic.plot.draws"),plot.draws,.GlobalEnv)
      save(plot.draws,file = paste0("RData/",marker,".chronic.plot.draws.RData"))
      
      return(estimates.df)
      
    }else{
      
      rpe = data.frame%>%
        group_by(condition.rir)%>%
        summarise(rpe=mean(avg.rpe,na.rm=TRUE))%>%
        .$rpe%>%
        .[c(2,3,4,1)]  
      
      plot.draws=model%>%
        predictions(
          newdata = datagrid(
            time = 0:1,
            avg.rpe = rpe,
            id = NA
          ),
          by = c("time","avg.rpe"),
          re_formula = NA,
          transform = function(x){(x*sd(model$data[,marker],na.rm=TRUE)+
                                     mean(model$data[,marker],na.rm=TRUE))})%>%
        suppressMessages()%>%
        posterior_draws()%>%
        mutate(condition.rir=ifelse(avg.rpe==rpe[1],"4-6 RIR",
                                ifelse(avg.rpe==rpe[2],"1-3 RIR",
                                       ifelse(avg.rpe==rpe[3],"0-3 RIR",
                                              "0 RIR"))),
               time.label=ifelse(time==0,"Pre","Post"))
      
      assign(paste0(marker,".chronic.plot.draws.all.data"),plot.draws,.GlobalEnv)
      save(plot.draws,file = paste0("RData/",marker,".chronic.plot.draws.all.data.RData"))
      
      return(estimates.df)
      
    }
    
  }
  
save(get.chronic.effects, file = "Functions/get.chronic.effects.RData")
  
# plot.chronic.effects ----------------------------------------------------

plot.chronic.effects = function(draws,
                              data.frame,
                              marker,
                              width = 8.25,
                              height = 4,
                              all.data = FALSE){
  
  if (marker == "bdnf") {
    ylab <- bquote(bold("BDNF ng·ml"^"-1"))
  } else if (marker == "catb") {
    ylab <- bquote(bold("CatB ng·ml"^"-1"))
  } else if (marker == "igf1") {
    ylab <- bquote(bold("IGF-1 ng·ml"^"-1"))
  } else {
    ylab <- bquote(bold("IL-6 pg·ml"^"-1"))
  }
  
  if (marker == "bdnf") {
    title <- bquote(bold("Chronic Effects for BDNF"))
  } else if (marker == "catb") {
    title <- bquote(bold("Chronic Effects for CatB"))
  } else if (marker == "igf1") {
    title <- bquote(bold("Chronic Effects for IGF-1"))
  } else {
    title <- bquote(bold("Chronic Effects for IL-6"))
  }
  
  if(all.data == FALSE){
    
    plot=ggplot(data = draws,
                aes(x=time,
                    y=draw,
                    color=condition.rir,
                    fill=condition.rir))+
      facet_grid(~fct_relevel(condition.rir,
                                        "4-6 RIR",
                                        "1-3 RIR",
                                        "0 RIR"))+
      THEME+
      geom_line(data = data.frame,
                aes(y=!!sym(marker),
                    x=time,
                    group=id),
                stat = "summary",
                linewidth=0.5,
                alpha = 0.125,
                position = position_dodge2(.1))+
      geom_point(data = data.frame,
                 aes(y=!!sym(marker),
                     x=time,
                     group=id),
                 stat = "summary",
                 alpha = 0.25,
                 shape = 16,
                 position = position_dodge2(.1))+
      stat_lineribbon(point_interval = "mode_hdci",
                      color="transparent",
                      alpha=0.25,
                      linewidth=1)+
      stat_lineribbon(point_interval = "mode_hdci",
                      fill="transparent",
                      linewidth=1)+
      scale_x_continuous(limits = c(-0.25,1.25),
                         breaks = c(0,1),
                         labels = unique(draws$time.label))+
      scale_color_manual(values = c("4-6 RIR"= "darkgreen",
                                    "1-3 RIR"= "goldenrod2",
                                    "0 RIR"= "red3"))+
      scale_fill_manual(values = c("4-6 RIR"= "darkgreen",
                                   "1-3 RIR"= "goldenrod2",
                                   "0 RIR"= "red3"))+
      labs(y = ylab,
           x = bquote(bold("Time Relative to Training Intervention")),
           title = title)
    
    ## save
    ggsave(plot,
           device = "pdf",
           filename = paste0("Plots/",marker,".chronic.effects.plot.pdf"),
           width = width,
           height = height)
    
    save(plot,file = paste0("RData/",marker,".chronic.effects.plot.RData"))
    
    ## assign to enviroment
    assign(paste0(marker,".chronic.effects.plot"), plot, envir = .GlobalEnv)
    
    return(plot)
    
  }else{
    
    plot=ggplot(data = draws,
                aes(x=session,
                    y=draw,
                    color=condition.rir,
                    fill=condition.rir))+
      facet_grid(week.label~fct_relevel(condition.rir,
                                        "4-6 RIR",
                                        "1-3 RIR",
                                        "0-3 RIR",
                                        "0 RIR"))+
      THEME+
      geom_line(data = data.frame,
                aes(y=!!sym(marker),
                    x=time,
                    group=id),
                stat = "summary",
                linewidth=0.5,
                alpha = 0.125,
                position = position_dodge2(.1))+
      geom_point(data = data.frame,
                 aes(y=!!sym(marker),
                     x=time,
                     group=id),
                 stat = "summary",
                 alpha = 0.25,
                 shape = 16,
                 position = position_dodge2(.1))+
      stat_lineribbon(point_interval = "mode_hdci",
                      color="transparent",
                      alpha=0.25,
                      linewidth=1)+
      stat_lineribbon(point_interval = "mode_hdci",
                      fill="transparent",
                      linewidth=1)+
      scale_x_continuous(limits = c(-0.25,1.25),
                         breaks = c(0,1),
                         labels = unique(draws$time.label))+
      scale_color_manual(values = c("4-6 RIR"= "darkgreen",
                                    "1-3 RIR"= "goldenrod2",
                                    "0-3 RIR"= "darkorange",
                                    "0 RIR"= "red3"))+
      scale_fill_manual(values = c("4-6 RIR"= "darkgreen",
                                   "1-3 RIR"= "goldenrod2",
                                   "0-3 RIR"= "darkorange",
                                   "0 RIR"= "red3"))+
      labs(y = ylab,
           x = bquote(bold("Time Relative to Training Intervention")),
           title = title)
    
    ## save
    ggsave(plot,
           device = "pdf",
           filename = paste0("Plots/",marker,".chronic.effects.plot.all.data.pdf"),
           width = width,
           height = height)
    
    save(plot,file = paste0("RData/",marker,".chronic.effects.plot.all.data.RData"))
    
    ## assign to environment
    assign(paste0(marker,".chronic.effects.plot.all.data"), plot, envir = .GlobalEnv)
    
    return(plot)
    
  }    
  
}
save(plot.chronic.effects, file = "Functions/plot.chronic.effects.RData")

# plot.combined.effects ---------------------------------------------------

plot.combined.effects = function(draws.acute,
                                 draws.chronic,
                                 data.frame.acute,
                                 data.frame.chronic,
                                 marker,
                                 width =7.25,
                                 height =7, 
                                 all.data = FALSE){
  
  if (marker == "bdnf") {
    ylab <- bquote(bold("BDNF ng·ml"^"-1"))
  } else if (marker == "catb") {
    ylab <- bquote(bold("CatB ng·ml"^"-1"))
  } else if (marker == "igf1") {
    ylab <- bquote(bold("IGF-1 ng·ml"^"-1"))
  } else {
    ylab <- bquote(bold("IL-6 pg·ml"^"-1"))
  }
  
  if (marker == "bdnf") {
    title <- bquote(bold("Acute and Chronic Effects for BDNF"))
  } else if (marker == "catb") {
    title <- bquote(bold("Acute and Chronic Effects for CatB"))
  } else if (marker == "igf1") {
    title <- bquote(bold("Acute and Chronic Effects for IGF-1"))
  } else {
    title <- bquote(bold("Acute and Chronic Effects for IL-6"))
  }
  
  draws=rbind(draws.acute,
  draws.chronic%>%
    mutate(session.label=time.label,
           session=time,
           week=8,
           week.label="Chronic")%>%
    select(-time.label,
           -time))
  
  data.frame=rbind(data.frame.acute%>%
                     select(-timepoint)%>%
                     mutate(week.label=ifelse(week==0,"Week 1","Week 7")),
                   data.frame.chronic%>%
                     mutate(session=time,
                            week=8,
                            week.label="Chronic")%>%
                     select(-time))
  
  if(all.data == FALSE){
    
    plot=ggplot(data = draws,
                aes(x=session,
                    y=draw,
                    color=condition.rir,
                    fill=condition.rir))+
      facet_grid(fct_relevel(week.label,
                             "Week 1",
                             "Week 7",
                             "Chronic")~fct_relevel(condition.rir,
                                        "4-6 RIR",
                                        "1-3 RIR",
                                        "0 RIR"))+
      THEME+
      geom_line(data = data.frame,
                aes(y=!!sym(marker),
                    x=session,
                    group=interaction(id,week)),
                stat = "summary",
                linewidth=0.5,
                alpha = 0.125,
                position = position_dodge2(.1))+
      geom_point(data = data.frame,
                 aes(y=!!sym(marker),
                     x=session,
                     group=interaction(id,week)),
                 stat = "summary",
                 alpha = 0.25,
                 shape = 16,
                 position = position_dodge2(.1))+
      stat_lineribbon(point_interval = "mode_hdci",
                      color="transparent",
                      alpha=0.25,
                      linewidth=1)+
      stat_lineribbon(point_interval = "mode_hdci",
                      fill="transparent",
                      linewidth=1)+
      scale_x_continuous(limits = c(-0.25,1.25),
                         breaks = c(0,1),
                         labels = unique(draws$session.label))+
      scale_color_manual(values = c("4-6 RIR"= "darkgreen",
                                    "1-3 RIR"= "goldenrod2",
                                    "0 RIR"= "red3"))+
      scale_fill_manual(values = c("4-6 RIR"= "darkgreen",
                                   "1-3 RIR"= "goldenrod2",
                                   "0 RIR"= "red3"))+
      labs(y = ylab,
           x = bquote(bold("Time")),
           title = title)
    
    ## save
    ggsave(plot,
           device = "pdf",
           filename = paste0("Plots/",marker,".combined.effects.plot.pdf"),
           width = width,
           height = height)
    
    save(plot,file = paste0("RData/",marker,".combined.effects.plot.RData"))
    
    ## assign to enviroment
    assign(paste0(marker,".combined.effects.plot"), plot, envir = .GlobalEnv)
    
    return(plot)
    
  }else{
    
    plot=ggplot(data = draws,
                aes(x=session,
                    y=draw,
                    color=condition.rir,
                    fill=condition.rir))+
      facet_grid(fct_relevel(week.label,
                             "Week 1",
                             "Week 7",
                             "Chronic")~fct_relevel(condition.rir,
                                        "4-6 RIR",
                                        "1-3 RIR",
                                        "0-3 RIR",
                                        "0 RIR"))+
      THEME+
      geom_line(data = data.frame,
                aes(y=!!sym(marker),
                    x=session,
                    group=interaction(id,week)),
                stat = "summary",
                linewidth=0.5,
                alpha = 0.125,
                position = position_dodge2(.1))+
      geom_point(data = data.frame,
                 aes(y=!!sym(marker),
                     x=session,
                     group=interaction(id,week)),
                 stat = "summary",
                 alpha = 0.25,
                 shape = 16,
                 position = position_dodge2(.1))+
      stat_lineribbon(point_interval = "mode_hdci",
                      color="transparent",
                      alpha=0.25,
                      linewidth=1)+
      stat_lineribbon(point_interval = "mode_hdci",
                      fill="transparent",
                      linewidth=1)+
      scale_x_continuous(limits = c(-0.25,1.25),
                         breaks = c(0,1),
                         labels = unique(draws$session.label))+
      scale_color_manual(values = c("4-6 RIR"= "darkgreen",
                                    "1-3 RIR"= "goldenrod2",
                                    "0-3 RIR"= "darkorange",
                                    "0 RIR"= "red3"))+
      scale_fill_manual(values = c("4-6 RIR"= "darkgreen",
                                   "1-3 RIR"= "goldenrod2",
                                   "0-3 RIR"= "darkorange",
                                   "0 RIR"= "red3"))+
      labs(y = ylab,
           x = bquote(bold("Time")),
           title = title)
    
    ## save
    ggsave(plot,
           device = "pdf",
           filename = paste0("Plots/",marker,".combined.effects.plot.all.data.pdf"),
           width = width,
           height = height)
    
    save(plot,file = paste0("RData/",marker,".combined.effects.plot.all.data.RData"))
    
    ## assign to environment
    assign(paste0(marker,".combined.effects.plot.all.data"), plot, envir = .GlobalEnv)
    
    return(plot)
    
  }    
  
}
save(plot.combined.effects, file = "Functions/plot.combined.effects.RData")



# get.combined.effects.table ----------------------------------------------

get.combined.effects.table = function(acute.effects,
                                      chronic.effects,
                                      marker, 
                                      all.data = FALSE){

  if (marker == "bdnf") {
    lab <- "BDNF"
  } else if (marker == "catb") {
    lab <- "CatB"
  } else if (marker == "igf1") {
    lab <- "IGF-1"
  } else {
    lab <- "IL-6"
  }
  
  if (marker == "bdnf") {
    title <- "Combined Posterior Effect Estimates for BDNF"
  } else if (marker == "catb") {
    title <- "Combined Posterior Effect Estimates for CatB"
  } else if (marker == "igf1") {
    title <- "Combined Posterior Effect Estimates for IGF-1"
  } else {
    title <- "Combined Posterior Effect Estimates for IL-6"
  }
    
  effects=rbind(acute.effects%>%
                  mutate(.model="Acute"),
                chronic.effects%>%
                  mutate(.model="Chronic"))%>%
    mutate(.marker=lab)%>%
    select(-.width,
           -.point,
           -.interval,
           -.sem)%>%
    rename(Marker=.marker,
           Model=.model,
           Estimate=.estimate,
           Mode=.value,
           `Lower_HDI`=.lower,
           `Upper_HDI`=.upper,
           `P_Null`=.null,
           `P_ROPE`=.rope)%>%
    relocate(Model,.after = "Marker")
  
  x=tab_df(effects,
         title = title,
         filename = paste0("Tables/",marker,"combined.effects.table.png"))

  if(all.data==FALSE){
    
    x=tab_df(effects,
             title = title,
             file = paste0(marker,".combined.effects.table.doc"),
             )
  
  save(x,file = paste0("RData/",marker,".combined.effects.table.RData"))
  
  ## assign to environment
  assign(paste0(marker,".combined.effects.table"), x, envir = .GlobalEnv)
  
  return(x)
  
  }else{
    
    x=tab_df(effects,
             title = title,
             file = paste0("Tables/",marker,".combined.effects.table.all.data.doc"))
    
    save(x,file = paste0("RData/",marker,".combined.effects.table.all.data.RData"))
    
    ## assign to environment
    assign(paste0(marker,".combined.effects.table.all.data"), x, envir = .GlobalEnv)
    
    return(x)
    
  }
}
save(get.combined.effects.table, file = "Functions/get.combined.effects.table.RData")

# #################### ----------------------------------------------------
