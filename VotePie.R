# Set up: we need the names of the parties and types of votes, a. to correctly
# map colours, and b. to ensure that our graph is drawn in the correct sequence

source('Settings.R')
source('Colours.R')


listVoteTypes <- names(colVotes)
listFractions <- names(colFractions)

helperFactors <-
  expand.grid(party = listFractions,
              vote = listVoteTypes,
              stringsAsFactors = FALSE)
helperFactors$level <-
  paste(helperFactors$vote, helperFactors$party)
helperColours <-
  rbind(
    data.frame(name = listVoteTypes, colours = colVotes),
    data.frame(name = listFractions, colours = colFractions),
    data.frame(
      name = helperFactors$level,
      colours = rep(colFractions, times = length(listVoteTypes))
    )
  )


plotPie <-
  function(df,
           inner,
           outer = NA,
           coloursInner = NA,
           coloursOuter = NA,
           pThreshold = 0.05) {
    assert(df, not_na, as.symbol(inner))
    coloursInner %>% verify(!is.na(.))
    
    hasInnerAndOuter <- all(!(is.na(outer) | is.na(coloursOuter)))
    
    if (hasInnerAndOuter) {
      assert(df, not_na, as.symbol(outer))
      
      helperFactors <- expand.grid(
        cOuter = names(coloursOuter),
        cInner = names(coloursInner),
        stringsAsFactors = FALSE
      )
      helperFactors$level <-
        paste(helperFactors$cInner, helperFactors$cOuter)
      
      helperColours <- rbind(
        data.frame(name = names(coloursInner),
                   colour = coloursInner),
        data.frame(
          name = helperFactors$level,
          colour = rep(coloursOuter, times =
                         length(coloursInner))
        )
      )
    } else {
      helperColours <- data.frame(name = names(coloursInner),
                                  colour = coloursInner)
    }
    
    # Create a summary for the inner ring
    dfinner <- df %>%
      group_by(cInner = eval(as.symbol(inner))) %>%
      dplyr::summarise(cInner = first(cInner), size = n())
    
    # Make sure the graph is drawn in the sequence we want by turning the
    # relevant column into a factor
    dfinner$cInner <-
      factor(dfinner$cInner, levels = helperColours$name)
    
    # ... and then calculate the x an y position for the text labels
    dfinner <- dfinner %>%
      arrange(desc(cInner)) %>% mutate(x = 1.1, y = cumsum(size) - 0.5 * size)
    
    if (hasInnerAndOuter) {
      # We do the same for the the outer ring of our graph
      dfinnerByOuter <- df %>%
        group_by(cInner = eval(as.symbol(inner)),
                 cOuter = eval(as.symbol(outer))) %>%
        summarise(
          cInner = first(cInner),
          cOuter = first(cOuter),
          size = n()
        )
      dfinnerByOuter$cInnerByOuter <- paste(dfinnerByOuter$cInner,
                                            dfinnerByOuter$cOuter)
      dfinnerByOuter$cInnerByOuter <-
        factor(dfinnerByOuter$cInnerByOuter,
               levels = helperColours$name)
      
      # As it turns out, ggplot2 changes plot direction for the second bar
      # (ring), therefore we need to reverse the y positions for our text labels
      dfinnerByOuter <- dfinnerByOuter %>%
        ungroup() %>%
        arrange(desc(cInnerByOuter)) %>%
        mutate(x = 2, y = cumsum(size) - 0.5 * size)
    }
    
    # Now we draw a two-column bar chart. We dont want to squeeze labels into
    # spaces that are too small, hence a threshold
    sumCInner <- nrow(df)
    
    pie <- ggplot() +
      geom_bar(
        data = dfinner,
        aes(x = 1, y = size, fill = cInner),
        stat = 'identity',
        color = 'white'
      ) +
      geom_text(aes(
        x = dfinner$x,
        y = dfinner$y,
        label = ifelse(
          dfinner$size / sumCInner >= pThreshold,
          as.character(dfinner$cInner),
          ''
        )
      ),
      color = "white",
      size = 3)
    
    if (hasInnerAndOuter) {
      pie <- pie +
        geom_bar(
          data = dfinnerByOuter,
          aes(x = 2,
              y = size,
              fill = cInnerByOuter),
          stat = 'identity',
          color = 'white'
        ) +
        geom_text(
          aes(
            x = dfinnerByOuter$x,
            y = dfinnerByOuter$y,
            label = ifelse(
              dfinnerByOuter$size / sumCInner >= pThreshold,
              as.character(dfinnerByOuter$cOuter),
              ''
            )
          ),
          color = "white",
          size = 4
        )
    }
    
    # We give it the correct colours for parties and eval(as.symbol(inner))s
    pie <- pie + scale_fill_manual(values = deframe(helperColours))
    
    # We turn it into a pie chart
    pie <- pie + coord_polar(theta = 'y', direction = -1)
    
    # ... and tidy up the presentation
    pie <- pie + theme(
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      axis.title.x = element_blank(),
      axis.text.x = element_blank(),
      axis.ticks.x = element_blank(),
      axis.title.y = element_blank(),
      axis.text.y = element_blank(),
      axis.ticks.y = element_blank(),
      legend.position = 'none'
    )
    pie <- pie + labs(x = "", y = "")
    
    return(pie)
  }


testPie <- function() {
  load(file = paste0(folderRData, 'skVotes.RData'))
  load(file = paste0(folderRData, 'skMPs.RData'))
  load(file = paste0(folderRData, 'fractions.RData'))
  
  delme <- skVotes %>%
    filter(skVotes$poll.id == 4034) %>%
    left_join(skMPs, by = c('mandate.id' = 'id')) %>%
    left_join(fractions, by = c('fraction_membership.fraction.id' = 'id')) %>%
    select('poll.id',
           'mandate.id',
           'politician.label',
           'short_name',
           'vote')
  
  delme <- delme  %>% filter(!is.na(delme$short_name))
  p1 <- plotPie2(
    delme,
    inner = 'vote',
    outer = 'short_name',
    coloursInner = colVotes,
    coloursOuter = colFractions
  )
  ggsave(plot = p1,
         dpi = 'print',
         filename = 'VotePie Test Image 1.jpg')
  
  p2 <- plotPie2(delme,
                 inner = 'vote',
                 coloursInner = colVotes)
  ggsave(plot = p2,
         dpi = 'print',
         filename = 'VotePie Test Image 2.jpg')
  
  p3 <- plotPie2(
    delme,
    inner = 'short_name',
    outer = 'vote',
    coloursInner = colFractions,
    coloursOuter = colVotes
  )
  ggsave(plot = p3,
         dpi = 'print',
         filename = 'VotePie Test Image 3.jpg')
}

#testPie()
