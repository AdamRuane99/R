head(flights)
summary(flights)


head(filter(flights, month==11, day ==3, carrier == 'AA'))


slice(flights, 1:10)

head(arrange(flights, year, month, day, air_time, desc(dep_time)))



head(select(flights, carrier, arr_time))

head(rename(flights, airline_carrier= carrier))

distinct(select(flights, carrier))

##mutate ##

head(transmute(flights, new_col = arr_delay-dep_delay))
