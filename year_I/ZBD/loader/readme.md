## Data loading team

No queries from me, I was loading data with Paweł.

Main script is data_model.py. It contains data model
in python, using sqlalchemy library. It is capable
of printing accurate create view statements and loading data
from specified files to database.
It also has help messages.

Loading data is slow. It takes 4 hours. Mostly due to
issuing insert statements to the database.
It is possible to speed it up, should another loading be
necessary (to ~10 minutes).

It is possible to speed it up. First - by running it
on pypy3, to use JIT, to reduce python overhead.
Reducing postgres insert time could be done by
creating copy files. It could be done in natural way in
current version of
script, by injecting logic to Loader.flush_objects, so it
would print to files and not insert to db.
Simple experiment shows that it should take roughly on my laptop
7,5 minute + time for writing to files 2GB in python, so 10 minutes
is possible to achieve.

The workflow was to issue

\i create_tables.sql (in psql)
load data using data_model.py (inbash)
\i create_constraints.sql (in psql)

pg_dump --data-only (in bash on database)
