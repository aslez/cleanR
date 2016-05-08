dat_sel <- function(type, year, vars = NULL) {
  rloc <- path.expand(paste0('~', R.home()))
  home <- stringr::str_extract(rloc, '/Users/(.*?)/')
  xwlk <- subset(ctyXwalk, YEAR == year)
  if (year == 1890) {
    xwlk[xwlk$ID_NUM == 10465, 'REL'] <- 38083000
    xwlk[xwlk$ID_NUM == 13489, 'REL'] <- 46107199    
  }
  if (!type %in% c('pop', 'rep')) {
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
      v <- ifelse(vlu_sub[x, ]$STATE == "NC", "V", "v")
      dnum <- vlu_sub$FILE[x]
      vnum <- vlu_sub[x, 5:NCOL(vlu_sub)]
      vnum_sub <- vnum[which(!is.na(vnum))]
      fpath <- paste0(dpath, 'DS0', sprintf('%03d', vlu_sub[x, 'FILE']))
      fname <- list.files(fpath, '.dta')
      d <- foreign::read.dta(paste0(fpath, '/', fname))
      d[, "VOTE"] <- d[, paste0(v, 1)] * 1e+06 + d[, paste0(v, 3)] * 100
      d[, names(vnum)] <- NA
      d[, names(vnum_sub)] <- d[, paste0(v, vnum_sub)]
      d[, c('VOTE', names(vnum))]
    })
    d <- do.call('rbind', d_lst)
  }
  d$YEAR <- year
  result <- dplyr::left_join(xwlk, d)
  if (!is.null(vars)) {
    result <- result[, c('ID_NUM', 'FIPS', 'NAME', 'STATE_TERR', vars)]
  }
  result
}
