Yarno znikł, ale już napiszę po angielsku; nie zaszkodzi to mojemu angielskiemu.

Table I propose is very simple. Mainly because Cassandra's
documentation suggests there is no point in doing modelling
other than having specific query in mind. Our query seems to need
following constraints:

- clustering on time of game
- primary key on playerTag, to use group by (it should be avialable since 3.10 and we have 3.11)
- primary key has to be unique, so it should be (playerTag, utcTime).

Other than that, I have decided to store data about each battle separately for
every player. This way, query will be very simple (it has to be, Cassandra is picky).
I have added other fields from clashroyale_battle_sample.json, which we were given
some time ago. Although I don't think there are lots of queries that could be
run on such table - any questions invloving data from more than one player
(even - did they play against each other) is impossible to answer.
Query would be:

SELECT player_tag, my_aggregate(crown = opponent_crowns) AS draw
FROM clashroyale_battles
WHERE utc_time <= '2012-01-01' AND utc_time <= '2012-01-31'
GROUP BY player_tag
ALLOW FILTERING;

Where aggregate functions would have to be defined in java by programmer (me)
(we need something like count(boolean) - I have checked it is possible to
create aggreagate functions) and should be very efficient - API
is to pass java code via plaintext which works with state of current group,
so it could simply add one to state if condition holds for a row.

It is useful to understand, that Cassandra is just a map from partition key
to columns sorted by the clustering part of the primary key. It tries hard
to touch get only rows which user will get in result - if it does not protest
to a query, it has linear complexity wrt to output size.

I have attached sample query above, because it easy to make a mistake with so
many (new!) rules
which restrict what can be done. Above query needs ALLOW FILTERING - cassandra
says it does not know if it will only touch needed rows. It is a bit of a chicken
and egg problem - we have time range as an input, so first part of key should
be timestamp. But then, we cannot do GROUP BY player_tag. SQL does not care
what we do GROUP BY on, so battles can be sorted by time and then group by
can be done on the player_tag, but Cassandra needs a prefix of columns from
primary key. And without group by, subqueries or join it is tough to do
anything to aggregate the data - or maybe I just don't see a way.

So above data model will go through every player, even if he did not play
int the time range. Other than that, it should (?) fetch only real battles,
and do GROUP BY on them - calling simple java function for each row. So this
should be fine, when a big proprotion of players played in some time range.
My appropach should be unreasonably slow for short time spans.

Other than that, one could think of dummy (eg. always 1) partition key and then sorting by time.
This way, Cassandra could fetch only needed battles, but then it seems it cannot do any
processing server side with this. After checking with docs - one could think of query which would
create materialized view with key as in my table, but only using data filtered by time range.
Then it would query as I plan - it should waste no time on nonactive players - and 
then deleted the view. But it seems complicated a bit, and I hope we will be doing larger
ranges and then I should be fine.

As I understand, limitations of group by steem from the fact that partioning key should
divide data between computers, and Cassandra wants to perform aggregation on a single node.
So it seems much better suited for situations we one needs a very fine grained access to the
data, and have some serious amount of it. Calculating statistics seems not to fit very nicely,
but again - I can be missing something.



And now some small notes.

Information about deck is a bit problematic. It would be nice to include
it in some form. I have decied to use list of tuples.
I have dropped names, since they seem not useful.
