gini <- function(y,s){
  suppressMessages(
    imp <- tibble(y=y,s=s) %>%
      #select(where(~!all(is.na(.)))) %>%
      group_by(.data$s) %>%
      summarize(imp = 1-sum((table(.data$y)/length(.data$s))^2),
                p=length(.data$s)#/nrow(.data)
      ) %>% ungroup() %>%
      summarize(
        imp = sum(.data$imp*.data$p)/sum(.data$p)
      ) %>% as.numeric()
  )
  return(imp)
}
