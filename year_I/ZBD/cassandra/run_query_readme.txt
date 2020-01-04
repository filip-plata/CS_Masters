This will be the lowest quality homework of all I have done.

I planned on creating my own aggregate function.

User defined aggregates are disabled in config of our Cassandra! It would
be great if people next year could experiment with this also, aggregate
functions seem to be per keyspace.

So I inspired by other people, I have done win/loss/draw mask. Also I have
enlarged PRIMARY KEY, since there are duplicate (player_tag, utc_time) pairs
with artificial random UUID as a last component.

I have loaded the data using load_data.py into keyspace fp371335. The query casts
data to bigint, otherwise there is overflow in summing numbers.

Cassandra turns out to be slow. Very slow. Some of the time ranges are from first email. I have measured using time utility,
since cassandra makes it tough otherwise. So a bit of time is taken by process creation and piping output to a file. But it does not matter,
cassandra on one node is sluggish.
I did checksum validation andit was fine.

2018-03-01 to 2018-04-01  : 6.447, 6.671, 6.537 so 6.55 +- 0.11 seconds
2018-03-01 to 2018-06-01 : it is the same data underneath, so times are the same
2018-05-25 to 2018-09-02: 390 seconds

After this I didn't even bother trying/waiting. It is extremely slow. Maybe the fault is in my  data model. But then, most of the models had player_tag
as partitioning key, so there won't be any magic here (but Monday will tell).

Or maybe Cassandra is not a tool for this job. Internet suggests it is for write heavy systems, which need to be queried on parts of data
for analytics - like network requests and so forth. Cassandra's webpage says Apple has 75000 nodes and 10PB of data. Maybe on this
scale Cassandra seems useful.
