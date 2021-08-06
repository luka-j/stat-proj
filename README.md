# stat-proj

An interactive dashboard for [Upis data](github.com/luka-j/UpisScraper). 

## Setup
R: execute `app/setup.R`

Launching the server: `./setup.sh`

## Requirements
A running PostgreSQL database containing a subset of Upis data (data dump available in [release assets](https://github.com/luka-j/stat-proj/releases/tag/1.0). Host, user and password are configured by `PG_HOST`, `PG_USER` and `PG_PASS` environment variables.
