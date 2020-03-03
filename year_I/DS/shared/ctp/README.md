# High level description

As we have discussed, this is a copy of collect.c, adapted
for the task. I attach a diff between aggregate.c and collect.c, to
further clarify what changes were done. Apart from header, there is only
one source file aggregate.c.

Connection keeps information about currently used aggregate function,
and a list of values from children in the tree. It also keeps a cache
of last sent value it tried to send. This way, traffic is generated
only when there indeed is a new value. Also there is a list of received
values from children.

Changes of the aggregate function are announced via announcement
mechanism, the same that collect code used. Nodes responds to
aggregate function changes from their parents.

Information about changes is propagated at the time of receiving
a packet and after calling `aggregate_send`. There is also a timer,
which checks whether node should send value again. My assumption is that
tree will not change during execution. I have seen it occure, and it results
for sum in obtaining higher result, because of counting value that switched
parent twice. This seems not to be possible to allieviate without
either informing previous parent - but it might went down - or creating
some second information of how much to subtract from values in case of
sum, count and average. Furthermore, to implement correctly, node would have to know what values have it sended to any other node. Thus, I assume
the tree is static. This is also based on the example, which allows some
time for network to settle.

## Changes to the header

I have added enum to header and struct aggregate_value. Additional
struct definition is there to have `struct aggregate_conn` definition shorter.
Enum is as we discussed, it describe aggregation which is currently used.

## Changes to collect code

Major changes are in node_packet_received - after receiving a packet.
Some `define` declarations are removed, along with the code protected
by them. Also there are some new methods for the new functionality.
Like converting string <-> enum, calulating and updating values of
nodes. Information about value sent to parent is set only on
sucessfult transmission. When parent is changed, we drop info
about last value sended for safety.
