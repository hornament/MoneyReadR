# Converter functions
# 
# Author: Flavian Imlig <flavian#imlig.info>
# Date: 19.05.2019
###############################################################################

readCSV_moneyok <- function(file)
{
	file_raw <- read.delim(file, as.is=T, strip.white = T, blank.lines.skip = T, encoding = 'UTF-8')
	
	records <- file_raw %>%
			select('Date' := matches('^Date'), 'Amount' := matches('^Amount'), 'Cat1' := matches('^Category.group'), 'Cat2' := matches('^Category$'), 'Note' := matches('^Note')) %>%
			slice(-1) %>%
			filter(!is.na(.data$Amount)) %>%
			mutate_at(vars('Date'), funs(lubridate::ymd)) %>%
			mutate_at(vars('Note'), funs(formatNote)) %>%
			mutate('Cat' := combineCategories(.data$Cat1, .data$Cat2))
	return(records)
	
}

readCSV_szkb <- function(file)
{
	# get start row of data, read file
	args_read <- list('file' = file, 'as.is' = T, 'strip.white' = T, 'blank.lines.skip' = T, 'encoding' = 'Latin-1')
	file_raw <- do.call('read.csv2', args_read)
	args_read$skip <- str_which(file_raw[,1], '^Datum')[1]
	file_raw <- do.call('read.csv2', args_read)
	
	# prepare
	records <- file_raw %>%
			transmute('Date1' := lubridate::dmy(.data$Datum),
					'Date2' := lubridate::dmy(.data$Valuta),
					'Caption' := stri_trans_nfc(.data$Buchungstext),
					'Amount' := as.numeric(.data$Betrag))
	return(records)
}

formatNote <- function(notes)
{
	parts <- str_split(stri_trans_nfc(notes), ',', n= 2, simplify = T)
	
	note <- purrr::pmap_chr(tbl_df(parts), function(V1, V2) {
				x <- str_trim(V1)
				y <- str_trim(V2)
				
				if(nchar(x) < 1) x <- '?'
				x <- sprintf('[%s]', str_to_lower(str_replace_all(x, ' ', '')))
				
				return(str_trim(str_c(x, y, sep=' ')))
			})	
	return(note)
}

formatNote_szkb <- function(notes)
{
	notes_out <- notes %>%
			str_replace('^Maestro-Einkauf \\d{2}:\\d{2} (.+) Kartennummer: \\d+$', '\\1,') %>%
			str_replace('^Bancomat \\d{2}:\\d{2} (.+) Kartennummer: \\d+$', 'kasse, Bancomat \\1') %>% 
			str_replace('^([\\w+]+).+(Strategiefonds)', 'szkb, \\1 \\2') %>%
			str_replace('^((Depotgeb.hren).+)', 'szkb, \\1') %>%
			str_replace(regex('^(.*(auftrag|gutschrift|rechnung|eingang).*)$', ignore_case = T), ',\\1') %>%
			formatNote() %>%
			str_replace('migrosm.*\\]', 'migros]')
}

combineCategories <- function(cat1, cat2)
{
	cat_comb <- str_c(str_trim(cat1), str_trim(cat2), sep=' ')
	return(str_trim(str_replace(cat_comb, ' +', ' ')))
}

prepareCSV_moneyok <- function(table, duedate)
{
	contrecat <- 'Portemonnaie'
	specialcat <- 'Food'
	
	records <- table %>%
			filter(year(.data$Date) == year(duedate) & month(.data$Date) == month(duedate)) %>%
			transmute('cat' := .data$Cat,
					'mode' := case_when(.data$Amount < 0 ~ -1, TRUE ~ 1),
					'amount' := .data$Amount,
					'Datum' := format(.data$Date, '%d.%m.%Y'),
					'Beschreibung' := .data$Note)
					
	specials <- records %>%
			filter(str_detect(.data$cat, '^Special')) %>%
			group_by(.data$cat, .data$mode) %>%
			summarise('amount' := sum(.data$amount)) %>% ungroup %>%
			mutate('Datum' := format(duedate, '%d.%m.%Y'),
					'Beschreibung' := str_c('[div]', str_replace(.data$cat, 'Special ?', ''), sep=' '),
					'cat' := specialcat)

	buddi <- records %>% 
			filter(!str_detect(.data$cat, '^Special')) %>%
			bind_rows(specials) %>%
			mutate('Betrag' := .data$amount * .data$mode,
					'Von' := case_when(.data$mode == -1 ~ contrecat, .data$mode == 1 ~ .data$cat),
					'An' :=  case_when(.data$mode == -1 ~ .data$cat, .data$mode == 1 ~ contrecat)) %>%
			arrange(.data$Datum) %>%
			select('Datum', 'Beschreibung', 'Betrag', 'Von', 'An')
	
	return(buddi)
}

prepareCSV_szkb <- function(table, duedate)
{
	cat1 <- 'SZKB Privatkonto'
	cat2 <- 'todo'
	
	records <- table %>%
			filter(year(.data$Date2) == year(duedate) & month(.data$Date2) == month(duedate)) %>%
			arrange(.data$Date2) %>%
			transmute('Datum' := format(.data$Date2, '%d.%m.%Y'),
					'Beschreibung' := formatNote_szkb(.data$Caption),
					'mode' := .data$Amount > 0,
					'Betrag' := abs(.data$Amount),
					'Von' := ifelse(.data$mode, cat2, cat1),
					'An' := ifelse(.data$mode, cat1, cat2)) %>%
			select(-.data$mode)
	
	records$An[str_which(records$Beschreibung, '^\\[kasse\\]')] <- 'Portemonnaie'
	records$An[str_which(records$Beschreibung, '^\\[swica\\]')] <- 'Versicherungen'
	
	return(records)
}

processAll <- function(yearmonth, type = 'MoneyOK')
{
	# parse arguments
	duedate <- ymd(str_c(first(yearmonth), '-01', collapse='')) %m+% months(1) - 1
	assert_that(is.Date(duedate))
	assert_that(is.string(type))
	assert_that(type %in% c('MoneyOK', 'SZKB'))
	
	# get paths
	directory <- 'C:/Users/horna/Documents/Various/Finance/Reports'
	patterns_in <- list('MoneyOK' = regex('^MoneyOK'),
			'SZKB' = regex('^Auszug'))
	patterns_out <- list('MoneyOK' = 'buddi_%Y-%m.csv', 'SZKB' = 'szkb_%Y-%m.csv')
	
	paths <- list('input' = last(list.files(directory, patterns_in[[type]])),
					'output' =  format(duedate, patterns_out[[type]])) %>%
			purrr::map(~file.path(directory, .x))
	
	# read files
	read_func <- str_c('readCSV', tolower(type), sep = '_')
	assert_that(exists(read_func, 'package:MoneyReadR', mode = 'function'), msg = sprintf('MoneyReadR::%s(): No read function for type %s in package.', 'processAll', type))
	records <- do.call(read_func, list('file' = paths$input))
#	return(records)
	
	# prepare table
	prepare_func <- str_c('prepareCSV', tolower(type), sep = '_')
	assert_that(exists(prepare_func, 'package:MoneyReadR', mode = 'function'), msg = sprintf('MoneyReadR::%s(): No prepare function for type %s in package.', 'processAll', type))
	buddy <- do.call(prepare_func, list('table' = records, 'duedate' = duedate))
#	return(buddy)
	
	# output
	fc <- file(description = paths$output, open = 'w', encoding = 'UTF-8')
	buddy %>% 
#			mutate('Beschreibung' := stri_conv(buddy$Beschreibung, from = 'UTF-8', to = 'Latin-1')) %>%
			write.csv2(fc, row.names = F)
#	write.csv2(buddy %>% mutate(, paths$output, row.names = F, fileEncoding = 'ANSI')
	close(fc)
	return(nrow(buddy))
}

#MoneyReadR::processAll('2018-07')