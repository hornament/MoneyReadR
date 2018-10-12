# Converter functions
# 
# Author: Flavian Imlig <flavian#imlig.info>
# Date: 16.07.2018
###############################################################################

readMoneyCSV <- function(file)
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

formatNote <- function(notes)
{
	parts <- str_split(stri_trans_nfc(notes), ',', n= 2, simplify = T)
	
	note <- purrr::pmap_chr(tbl_df(parts), function(V1, V2) {
				x <- str_trim(V1)
				y <- str_trim(V2)
				
				if(nchar(x) < 1) x <- '?'
				x <- sprintf('[%s]', str_to_lower(str_replace(x, ' ', '')))
				
				return(str_trim(str_c(x, y, sep=' ')))
			})	
	return(note)
}

combineCategories <- function(cat1, cat2)
{
	cat_comb <- str_c(str_trim(cat1), str_trim(cat2), sep=' ')
	return(str_trim(str_replace(cat_comb, ' +', ' ')))
}

prepareBuddiCSV <- function(table, yearmonth)
{
	duedate <- ymd(str_c(first(yearmonth), '-01', collapse='')) %m+% months(1) - 1 
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

processAll <- function(yearmonth)
{
	directory <- 'C:\\Users\\horna\\Documents\\Various\\Finance\\Reports\\'
	file <- last(list.files(directory, '^MoneyOK'))
	
	records <- readMoneyCSV(str_c(directory, file, sep=''))
	buddy <- prepareBuddiCSV(records, yearmonth)
	
	write.csv2(buddy, 
			str_c(directory, 'buddi_', format(dmy(max(buddy$Datum)), '%Y-%m'), '.csv', sep=''),
			row.names = F)
}

#MoneyReadR::processAll('2018-06')