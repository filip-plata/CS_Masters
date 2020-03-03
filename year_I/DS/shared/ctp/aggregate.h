/**
 * \file
 *         Header file for hop-by-hop in-tree reliable data aggregation
 * \author
 *         Mateusz Banaszek
 */

/**
 * \addtogroup rime
 * @{
 */

/**
 * \defgroup rimeaggregate Tree-based hop-by-hop in-tree reliable data aggregation
 * @{
 *
 * The aggregation module implements a hop-by-hop in-tree reliable data
 *   aggregation mechanism.
 *
 * NOTE: This header is heavily inspired by collect.h. Only differences are
 *   documented here. Everything not discribed here follows RIME's collect.
 */

#ifndef AGGREGATE_H_
#define AGGREGATE_H_

#include "net/rime/announcement.h"
#include "net/rime/runicast.h"
#include "net/rime/neighbor-discovery.h"
#include "net/rime/collect-neighbor.h"
#include "net/rime/packetqueue.h"
#include "sys/ctimer.h"
#include "lib/list.h"

struct aggregate_callbacks {
  /**
   * Callback announcing to the sink the aggregated result.
   *
   * The callback is called only if the mote is the sink.
   * The callback is called only when the value of the aggregated result changes.
   *
   * @param aggregate Current aggregate function. It is the same as an argument
   *   to the last `aggregate_set_aggregate()` call.
   * @param result Current aggregated result.
   */
  void (* recv)(const char *aggregate, int32_t result);
};

enum Aggregate_fun {
  SUM,
  COUNT,
  AVG,
  MIN,
  MAX
};

struct aggregate_value {
  int32_t value;
  uint16_t count;
  uint8_t method;
};


struct aggregate_conn {
  struct unicast_conn unicast_conn;
  struct announcement announcement;
  struct announcement announcement_aggr_fun;
  struct ctimer transmit_after_scan_timer;

  const struct aggregate_callbacks *cb;
  struct ctimer retransmission_timer;
  LIST_STRUCT(send_queue_list);
  struct packetqueue send_queue;
  LIST_STRUCT(aggregate_values_list);
  struct collect_neighbor_list neighbor_list;

  struct ctimer keepalive_timer;
  clock_time_t keepalive_period;

  struct ctimer proactive_probing_timer;

  linkaddr_t parent, current_parent;
  uint16_t rtmetric;
  uint8_t seqno;
  uint8_t sending, transmissions, max_rexmits;
  uint8_t eseqno;
  uint8_t is_router;
  struct aggregate_value *last_agg_sent, current_agg_val, agg_val_cache;
  struct ctimer aggregate_timer;

  clock_time_t send_time;

  enum Aggregate_fun agg_f;
};

enum {
  AGGREGATE_NO_ROUTER,
  AGGREGATE_ROUTER,
};

// is_router == AGGREGATE_NO_ROUTER doesn't have to be supported in the assignment.
void aggregate_open(struct aggregate_conn *c, uint16_t channels,
                  uint8_t is_router,
                  const struct aggregate_callbacks *callbacks);

void aggregate_close(struct aggregate_conn *c);

/**
 * Sends a value.
 *
 * @param c Aggregate connection.
 * @param value The value to be sent.
 * @result Status.
 */
int aggregate_send(struct aggregate_conn *c, int32_t value);

void aggregate_set_sink(struct aggregate_conn *c, int should_be_sink);

const linkaddr_t *aggregate_parent(struct aggregate_conn *c);

/**
 * Sets aggregate function.
 *
 * This function is called only by the sink.
 * When the aggregate function is changed, the network discards past values
 *   and starts aggregating follwing values with the new aggregate function.
 *
 * @param c Aggregate connection.
 * @param aggregate The aggregate function. Allowed functions:
 *   "sum": Sums values sent by motes.
 *          The value is wrapped around int32_t in the standard way.
 *   "count": Counts values sent by motes.
 *            The values are de facto ignored.
 *   "avg": Calculates average of values sent by motes.
 *          The result should be as exact as it is possible.
 *          A nonintegral result is casted to int32_t.
 *   "min": Calculates minimum of values sent by motes.
 *   "max": Calculates maximum of values sent by motes.
 *
 * "sum" is the default – it is applied after the connection is opened
 *    and `aggregate_set_aggregate()` is not called yet.
 */
void aggregate_set_aggregate(struct aggregate_conn *c, const char *aggregate);

#endif /* AGGREGATE_H_ */
/** @} */
/** @} */
