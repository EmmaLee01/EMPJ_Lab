## Yards gained mixture model
## Lab 7

# football_data2014 <- readRDS("pbp2014-2024.rds")

## Decision tree for downs 1-3

## Red zone vs other part of field

## Run vs pass

# Run - fumble (fumble_lost 148, fumble_forced 141, fumble 164) vs not (fumble_not_forced 142)
# rush_attempt - col 152
# Pass - incomplete (col 128), complete (complete_pass 165), interception (col 130)
# pass_attempt - col 151

# For complete passes and run, use inferred models 