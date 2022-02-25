library(stringi)

a <- brio::read_file(fp('2020', '4')) # Read file as single string
abid <- stri_split_fixed(a, '\n\n')[[1]] # Split on each passport

fields <- c('byr', 'iyr', 'eyr', 'hgt', 'hcl', 'ecl', 'pid') # Fields to find
b <- lapply(fields, stri_detect_fixed, str=abid) |> Reduce(f=`&`) # Find all fields
sum(b) # Part 1 (228): Passports with all fields present

patterns <- c(
  "byr:19[2-9].|200[0-2]",
  "iyr:201.|2020",
  "eyr:202.|2030",
  "hgt:(((1[5-8].|19[0-3])cm)|((59|6[0-9]|7[0-6])in))",
  "hcl:#[a-f0-9]{6}",
  "ecl:(amb|blu|brn|gry|grn|hzl|oth)",
  "pid:\\d{9}"
) # Rules on each field
patterns <- paste0(patterns, '(\n| |$)')
matchs <- lapply(patterns, grepl, x=abid[b], perl=T) |> Reduce(f=`&`) # Apply regex on each field
sum(matchs) # Part 2 (175): Passports with all fields corrects
