# advent-haskell

Solutions for Advent of Code problems in Haskell.

## Running Solutions
Solutions can be run by executing `stack run -- [ARGUMENT(s)]`

```
Usage: advent-haskell-exe [YEAR] [DAY] [(-s|--stdin) | (-f|--file ARG)]

Available options:
  YEAR                     puzzle year (default: <year of most recent puzzle>)
  DAY                      puzzle day (default: <day of most recent puzzle>)
  -s,--stdin               use stdin as input
  -f,--file ARG            get input from file
  -h,--help                Show this help text
```

If no optional flags are given, will attempt to download user input from the Advent of Code website, using the session cookie value set at `$AOC_SESSION`, caching inputs at the optionally set `$AOC_CACHE`.