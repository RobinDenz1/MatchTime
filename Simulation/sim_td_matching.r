
library(simDAG)
library(survival)
library(foreach)
library(parallel)
library(doSNOW)
library(doRNG)
library(ggplot2)
library(fastmatch)
library(data.table)
library(dplyr)

## compare standard cox with time-dependent matching
sim_comp <- function(dag, n_sim, n_repeats, max_t, n_cores, seed=runif(1),
                     ...) {

  # parallel processing
  cl <- makeCluster(n_cores, outfile="")
  doSNOW::registerDoSNOW(cl)
  pckgs <- c("data.table", "simDAG", "survival", "MatchTD")
  glob_funs <- ls(envir=.GlobalEnv)[sapply(ls(envir=.GlobalEnv), function(obj)
    "function"==class(eval(parse(text=obj)))[1])]

  # add progress bar
  pb <- txtProgressBar(max=n_repeats, style=3)
  progress <- function(n) setTxtProgressBar(pb, n)
  opts <- list(progress=progress)

  # start simulation
  set.seed(seed)
  out_final <- foreach::foreach(i=seq_len(n_repeats), .packages=pckgs,
                          .export=glob_funs, .options.snow=opts) %dorng% {

    # generate the data
    sim <- sim_discrete_time(dag, n_sim=n_sim, max_t=max_t)

    # get standard time-varying cox model based estimates
    data <- sim2data(sim, to="start_stop", target_event="influenza",
                     overlap=TRUE, keep_only_first=TRUE)

    mod <- coxph(Surv(start, stop, influenza) ~ vacc + mac + meds + vacc*mac,
                 data=data)

    out_1 <- as.data.frame(summary(mod)$conf.int)
    rownames(out_1) <- NULL
    out_1$name <- c("vacc", "mac", "meds", "vacc*mac")
    out_1$type <- "cox"

    # get time-dependent matching based estimates
    d_treat <- get_event_times(sim, "vacc")
    d_influ <- get_event_times(sim, "influenza")
    d_meds <- get_event_times(sim, "meds")
    d_baseline <- sim$data[, c(".id", "mac"), with=FALSE]

    dlist <- list(d_influ)
    names(dlist) <- c("influenza")
    d_inclusion <- MatchTD:::list2start_stop(dlist, n=nrow(d_baseline),
                                             max_t=max_t, durations=20)
    d_inclusion <- d_inclusion[influenza==FALSE]

    d_match <- td_matching(id=".id", time=".time", d_treat=d_treat,
                           d_event=d_influ, d_baseline=d_baseline,
                           keep_all_column=TRUE, d_inclusion=d_inclusion,
                           ...)
    d_match <- MatchTD:::add_previous_event_time(data=d_match, d_prev=d_meds,
                                       id=".id", time=".time", duration=Inf,
                                       name="meds")

    # remove cases without controls
    d_match[, n_per_pair := .N, by=pair_id]

    mod <- coxph(Surv(event_time, status) ~ .treat + mac + meds + .treat*mac,
                 data=subset(d_match, n_per_pair == 2))
    out_2 <- as.data.frame(summary(mod)$conf.int)
    rownames(out_2) <- NULL
    out_2$name <- c("vacc", "mac", "meds", "vacc*mac")
    out_2$type <- "matching"

    rbind(out_1, out_2)
  }
  close(pb)
  stopCluster(cl)

  return(rbindlist(out_final))
}

## extract all unique event times from a simDT object
get_event_times <- function(sim, name) {

  ldat <- sim$tte_past_events[[name]]
  out <- vector(mode="list", length=length(ldat))
  for (i in seq_len(length(ldat))) {
    if (length(ldat[[i]]) > 0) {
      out[[i]] <- data.table(.id=ldat[[i]], .time=i)
    }
  }
  d_prev <- rbindlist(out)
  setkeyv(d_prev, c(".id", ".time"))

  return(d_prev)
}

##### No confounding #####

prob_influ <- function(data, base_p, rr_mac, rr_vacc_mac, rr_vacc_nonmac) {
  base_p * rr_mac^(data$mac) * rr_vacc_mac^(data$mac & data$vacc_event) *
    rr_vacc_nonmac^(data$vacc_event & !data$mac)
}

dag <- empty_dag() +
  node("mac", type="rbernoulli", p=0.2) +
  node_td("vacc", type="time_to_event", prob_fun=0.001,
          event_duration=Inf) +
  node_td("influenza", type="time_to_event", prob_fun=prob_influ,
          event_duration=20, parents=c("mac", "vacc_event"),
          base_p=0.01, rr_mac=1.5, rr_vacc_mac=0.8, rr_vacc_nonmac=0.5)

set.seed(32457)
sim <- sim_discrete_time(dag, n_sim=100000, max_t=365)

dmod <- sim2data(sim, to="start_stop", target_event="influenza",
                 overlap=TRUE, keep_only_first=TRUE)

mod <- coxph(Surv(start, stop, influenza) ~ mac + vacc + mac*vacc, data=dmod)
summary(mod)

d_treat <- get_event_times(sim, "vacc")
d_event <- get_event_times(sim, "influenza")
d_baseline <- sim$data[, c(".id", "mac"), with=FALSE]
d_match <- td_matching(id=".id", time=".time", d_treat=d_treat,
                       d_baseline=d_baseline, d_event=d_event)

mod <- coxph(Surv(event_time, status) ~ mac + .treat + mac*.treat, data=d_match)
summary(mod)

#### With time-fixed confounder ####

prob_influ <- function(data, base_p, rr_mac, rr_vacc_mac, rr_vacc_nonmac,
                       rr_sex) {
  base_p * rr_mac^(data$mac) * rr_vacc_mac^(data$mac & data$vacc_event) *
    rr_vacc_nonmac^(data$vacc_event & !data$mac) * rr_sex^(data$sex)
}

prob_vacc <- function(data, base_p, rr_sex) {
  base_p * rr_sex^(data$sex)
}

dag <- empty_dag() +
  node("mac", type="rbernoulli", p=0.2) +
  node("sex", type="rbernoulli", p=0.5) +
  node_td("vacc", type="time_to_event", prob_fun=prob_vacc,
          event_duration=Inf, parents="sex", base_p=0.0005,
          rr_sex=5) +
  node_td("influenza", type="time_to_event", prob_fun=prob_influ,
          event_duration=20, parents=c("mac", "vacc_event", "sex"),
          base_p=0.01, rr_mac=1.5, rr_vacc_mac=0.8, rr_vacc_nonmac=0.5,
          rr_sex=4)

#### with time-varying confounder #####

prob_influ <- function(data, base_p, rr_mac, rr_vacc_mac, rr_vacc_nonmac,
                       rr_meds) {
  base_p * rr_mac^(data$mac) * rr_vacc_mac^(data$mac & data$vacc_event) *
    rr_vacc_nonmac^(data$vacc_event & !data$mac) * rr_meds^(data$meds_event)
}

prob_vacc <- function(data, base_p, rr_meds) {
  base_p * rr_meds^(data$meds_event)
}

dag <- empty_dag() +
  node("mac", type="rbernoulli", p=0.2) +
  node_td("meds", type="time_to_event", prob_fun=0.001, event_duration=Inf) +
  node_td("vacc", type="time_to_event", prob_fun=prob_vacc,
          event_duration=Inf, parents="meds_event", base_p=0.0005,
          rr_meds=5) +
  node_td("influenza", type="time_to_event", prob_fun=prob_influ,
          event_duration=20, parents=c("mac", "vacc_event", "meds_event"),
          base_p=0.001, rr_mac=1.5, rr_vacc_mac=0.8, rr_vacc_nonmac=0.5,
          rr_meds=4)

sim <- sim_discrete_time(dag, n_sim=100000, max_t=365)

# with standard cox
dmod <- sim2data(sim, to="start_stop", target_event="influenza",
                 overlap=TRUE, keep_only_first=TRUE)
mod <- coxph(Surv(start, stop, influenza) ~ vacc + mac + vacc*mac + meds,
             data=dmod)
summary(mod)

# with td matching
d_treat <- get_event_times(sim, "vacc")
d_influ <- get_event_times(sim, "influenza")
d_meds <- get_event_times(sim, "meds")
d_baseline <- sim$data[, c(".id", "mac"), with=FALSE]

dlist <- list(d_influ)
names(dlist) <- c("influenza")
d_inclusion <- list2start_stop(dlist, n=nrow(d_baseline), max_t=365,
                               durations=20)
d_inclusion <- d_inclusion[influenza==FALSE]

d_match <- td_matching(id=".id", time=".time", d_treat=d_treat,
                       d_event=d_influ, d_baseline=d_baseline,
                       keep_all_columns=TRUE, d_inclusion=d_inclusion,
                       replace_at_t=FALSE, verbose=TRUE)

mod <- coxph(Surv(event_time, status) ~ .treat + mac + .treat*mac,
             data=d_match)
summary(mod)

d_match <- add_previous_event_time(data=d_match, id=".id", time=".time",
                                d_prev=d_meds, duration=Inf, name="meds")
#data <- add_previous_event_time(data, id=".id", time=".time",
#                                d_prev=d_influ, duration=20, name="influ")

d_match[, n_per_pair := .N, by=pair_id]
d_match <- d_match[n_per_pair==2]

mod <- coxph(Surv(event_time, status) ~ .treat + mac + .treat*mac + meds,
             data=d_match, cluster=pair_id)
summary(mod)


############# run simulation

set.seed(2432454)
out <- sim_comp(dag=dag,
                n_sim=3000,
                n_repeats=500,
                max_t=365,
                n_cores=8,
                if_lt_n_at_t="warn")


true_rr <- data.table(name=c("vacc", "mac", "meds", "vacc*mac"),
                      rr=c(0.5, 1.5, 4, 1.6))
out <- merge(out, true_rr, by="name", all.x=TRUE)
out[, bias := `exp(coef)` - rr]

# bias
ggplot(out, aes(x=name, y=bias, fill=name)) +
  geom_boxplot() +
  facet_wrap(~type) +
  theme_bw() +
  theme(legend.position="none") +
  geom_hline(yintercept=0, linetype="dashed") #+
ylim(c(-1, 1))


out %>%
  group_by(type, name) %>%
  summarise(mean(bias, na.rm=T))
