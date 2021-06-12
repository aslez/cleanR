dat_sel <- function(type, year, vars = NULL) {
  rloc <- path.expand(paste0('~', R.home()))
  home <- stringr::str_extract(rloc, '/Users/(.*?)/')
  xwlk <- subset(ctyXwalk, YEAR == year)
  xwlk[xwlk$ID_NUM == 6010, 'VOTE'] <- 32133000
  xwlk[xwlk$ID_NUM == 6012, 'VOTE'] <- 32135000
  if (year == 1890) {
    xwlk[xwlk$ID_NUM == 10465, 'REL'] <- 38083000
    xwlk[xwlk$ID_NUM == 13489, 'REL'] <- 46107199    
  }
  if (!type %in% c('pop', 'rep', 'dem')) {
    dpath <- paste0(home, '/Box Sync/data/haines/')
    dnum <- clu[which(clu$TYPE == type & clu$YEAR == year), 'FILE']
    fpath <- paste0(dpath, 'DS00', dnum)
    fname <- paste0('/02896-00', dnum, '-Data.dta')
    d <- foreign::read.dta(paste0(fpath, fname))
    d[, toupper(type)] <- d$fips * 1000
    if (year == 1890 & type == 'rel') {
      d$REL[d$REL == 38053000 & d$name == 'SHERIDAN'] <- 38083000
      d$REL[d$state == 37 & d$name == 'PRESHO'] <- 46107199    
    }
  }
  else {
    dpath <- paste0(home, '/Box Sync/data/icpsr_voting/')
    vlu_sub <- subset(vlu, YEAR == year & TYPE == type)
    d_lst <- lapply(1:NROW(vlu_sub), function(x) {
      v <- ifelse(vlu_sub[x, ]$STATE %in% c("NC", "KS", "NE"), "V", "v")
      dnum <- vlu_sub$FILE[x]
      vnum <- vlu_sub[x, 5:NCOL(vlu_sub)]
      vnum_sub <- vnum[which(!is.na(vnum))]
      fpath <- paste0(dpath, 'DS0', sprintf('%03d', vlu_sub[x, 'FILE']))
      fname <- list.files(fpath, '.dta')
      d <- foreign::read.dta(paste0(fpath, '/', fname))
      d[, "VOTE"] <- d[, paste0(v, 1)] * 1e+06 + d[, paste0(v, 3)] * 100
      d$VOTE[d$VOTE == 32135000 & d[, paste0(v, 2)] == 'NEOSHO/DORN'] <- 32133000
      d$VOTE[d$VOTE == 32137000 & d[, paste0(v, 2)] == 'NESS'] <- 32135000
      d[, names(vnum)] <- NA
      d[, names(vnum_sub)] <- d[, paste0(v, vnum_sub)]
      
      #add district endorsement information
      flu_sub <- flu[flu$STATE == vlu_sub[x, "STATE"] & flu$YEAR == year, ]
      d$CDIST_0_1[d[, paste0(v, 1)] %in% 36:37] <- 1
      d$CDIST_0_1[d$VOTE == 35015000] <- 6 #Boyd, NE
      d$CDIST_0_1[d$VOTE == 35071000] <- 7 #Garfield, KS
      d$CDIST_0_1[d$VOTE == 33135000] <- 7 #Roseau, MN
      d$CD_STAT <- flu_sub$STATUS[match(d$CDIST_0_1, flu_sub$DISTRICT)]
      
      #replace zeros with NA for MN districts 1 and 4 in 1890
      if (year == 1890 & type == 'pop') {
        d$CONV_1_1[d[, paste0(v, 1)] == 33 & d$CDIST_0_1 %in% c(1, 4)] <- NA
      }
      
      #fix bad presidential voting data with info from Leip
      if (year == 1892) {
        d$PRST_1_1[d$VOTE == 32053000] <- 2213
        d$PRST_1_1[d$VOTE == 32189000] <- 270
        if (type == 'pop') {
          d$PRSV_1_1[d$VOTE == 32189000] <- 185
        }
      }
      
      d[, c('VOTE', 'CD_STAT', names(vnum))]
    })
    d <- do.call('rbind', d_lst)
  }
  d$YEAR <- year
  result <- dplyr::left_join(xwlk, d, na_matches = 'never')
  if (!is.null(vars)) {
    result <- result[, c('ID_NUM', 'FIPS', 'NAME', 'STATE_TERR', vars)]
  }
  result
}
