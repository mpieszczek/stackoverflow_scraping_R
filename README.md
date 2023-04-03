# StackOverflow scraping

Main logic is contained in `Stack.Rmd` - scraping and saving data.

Default setting downloads data from whole Stack history.
The process might take a long time and Stack might temporarily block your requests.

For tests one might make a change in line:
```
stack_quest <- strony[1:ostatnia] %>% map_dfr(zbieraj)
```
and set custom range of pages e.g.
```
# Only reads 10 pages.
stack_quest <- strony[1:10] %>% map_dfr(zbieraj)
```

This is university project, so conclusions are not exhaustive due to time restrictions of semester.

You can check `Stack.html` to see results for whole history.

The `app.R` is a ShinyApp with some interactive time-series build on scrapped data.