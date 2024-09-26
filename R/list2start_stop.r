
## transforms a list of data.tables containing event times per person in
## the long format into a single data.table in the start-stop format,
## using the sim2start_stop.last() function from simDAG package
#' @importFrom data.table data.table
list2start_stop <- function(dlist, n, max_t, durations, ...) {

  names <- names(dlist)

  # construct fake tx_nodes object
  tx_nodes <- vector(mode="list", length=length(names))
  for (i in seq_len(length(names))) {
    tx_nodes[[i]] <- list(name=names[i], type_str="time_to_event",
                          event_duration=durations[i])
  }

  # construct fake tte_past_events object
  tte_past_events <- vector(mode="list", length=length(names))
  for (i in seq_len(length(names))) {

    # split by .time
    splitted <- split(dlist[[i]], by=".time", sorted=TRUE)

    out_ij <- vector(mode="list", length=max_t)
    names(out_ij) <- paste(seq_len(max_t))
    for (j in seq_len(max_t)) {
      t_name <- as.character(j)

      if (t_name %fin% names(splitted)) {
        out_ij[[t_name]] <- splitted[[t_name]]$.id
      }
    }
    tte_past_events[[i]] <- out_ij
  }
  names(tte_past_events) <- names

  # put it into a fake simDT object
  sim_fake <- list(data=data.table(.id=seq_len(n)),
                   max_t=max_t,
                   tx_nodes=tx_nodes,
                   tte_past_events=tte_past_events)

  # transform to start-stop format
  out <- simDAG:::sim2start_stop.last(sim=sim_fake, ...)
  return(out)
}
