# nblscrapeR 0.0.3

The NBL changed their endpoints and data structures - likely because of a new data provider.

### New Functions

* `get_season_matches_df()` gets a data frame of all matches in the 25-26 season
* `get_each_match()` gets all the required data for `nblR` for a particular match ID and returns all that data (including nested data frames) in a data frame

***

# nblscrapeR 0.0.2.4000

### Bug

* API request for `get_matches()` was not getting all columns, so reverted back to not hard coding selected fields in query

***

# nblscrapeR 0.0.2.3000

### Bug

* API request for `get_matches()` was only getting regular season matches

***

# nblscrapeR 0.0.2.2000

### Bug

* API for matches changes, causing the change to `get_matches()`

***

# nblscrapeR 0.0.2.1000

### Bug

* API for matches changes, causing the change to `get_matches()`

***

# nblscrapeR 0.0.2

### Improvements

* Vectorised all functions to accept full game lists
* Output of each parsing functions now includes season column

### New Functions

* `parse_match_shot()` to parse shooting coordinate and type data

***

# nblscrapeR 0.0.1

* First release

***
