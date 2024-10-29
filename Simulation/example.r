
## function to create random data.tables with ID and Date
get_random_id_date <- function(n, n_ids=10000, start_date, end_date, sort=TRUE,
                               multiple_per_ID=TRUE) {

  if (multiple_per_ID) {
    ID <- sample(1:n_ids, size=n, replace=TRUE)
  } else {
    ID <- seq_len(n)
  }

  data <- data.table(ID=ID,
                     Date=sample(seq(as.Date(start_date),
                                     as.Date(end_date), by="day"),
                                 size=n, replace=TRUE))
  data <- unique(data)

  if (sort) {
    setkeyv(data, c("ID", "Date"))
  }
  return(data)
}

set.seed(13243)

# vaccinations
d_vacc_pneumo <- get_random_id_date(n=10000, start_date="2015/01/01",
                               end_date="2021/01/31")
d_vacc_herpzo <- get_random_id_date(n=10000, start_date="2018/12/01",
                               end_date="2021/01/31")
d_vacc_influ <- get_random_id_date(n=10000, start_date="2015/01/01",
                              end_date="2021/01/31")

# outcomes
d_pneumo <- get_random_id_date(n=10000, start_date="2015/01/01",
                               end_date="2021/01/31")
d_herpzo <- get_random_id_date(n=10000, start_date="2018/12/01",
                               end_date="2021/01/31")
d_influ <- get_random_id_date(n=10000, start_date="2015/01/01",
                              end_date="2021/01/31")

# eligibility criteria
d_death <- get_random_id_date(n=10000, start_date="2013/01/01",
                              end_date="2021/12/31", multiple_per_ID=FALSE)

d_icd_excl <- get_random_id_date(n=200, n_ids=10000, start_date="2013/01/01",
                                 end_date="2021/12/31")

# confounder etc.
d_baseline <- data.table(ID=seq_len(10000),
                         sex=rbernoulli(n=10000, p=0.5),
                         GebJahr=rnorm(n=10000, mean=1950, sd=20),
                         Place=sample(1:50, size=10000, replace=TRUE))

d_HA_contacts <- get_random_id_date(n=10000, start_date="2013/01/01",
                                    end_date="2021/12/31")

d_A_contacts <- get_random_id_date(n=10000, start_date="2013/01/01",
                                    end_date="2021/12/31")

d_KH_visits <- get_random_id_date(n=10000, start_date="2013/01/01",
                                  end_date="2021/12/31")

d_chickenpox <- get_random_id_date(n=100, n_ids=10000, start_date="2013/01/01",
                                   end_date="2021/12/31")

# versicherungszeiten
dag <- empty_dag() +
  node_td("insured", type="time_to_event", prob_fun=0.01,
          event_duration=120)

d_vers <- sim2data(sim_discrete_time(dag, n_sim=10000, max_t=2000),
                   to="start_stop")



## Arbeitsschritte:
# 1.) Erstelle start-stop Datensatz pro Target Trial mit:
#     - Matching-variablen: Infos zu sex, GebJahr, Place, Disease
#     - Inclusion criteria: death, versicherungszeiten, ICD ausschlüsse

# 2.) Perform Matching with match_td.fit()
#     - hier können d_vacc und d_outcome direkt als input einfließen

# 3.) Add further adjustment variables
#     - von inclusion time zurückgucken:
#       - HA Kontakte in letzten zwei Jahren
#       - A Kontakte generell in letzten zwei Jahren
#       - Stationäre KH Aufenthalte
#       - Chickenpox ever (add_previous_event_time())
#       - medical devices (add_previous_event_time())
#       - CCI (ist mir noch nicht ganz klar)

# 4.) Modell berechnen
# 5.) G-Computation mit Modell

