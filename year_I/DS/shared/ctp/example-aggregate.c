/**
 * \file
 *         Example of how the aggregate primitive works.
 * \author
 *         Mateusz Banaszek
 */

#include "contiki.h"
#include "lib/random.h"
#include "net/rime/rime.h"
#include "aggregate.h"
#include "net/netstack.h"

#include <stdio.h>

static struct aggregate_conn tc;

/*---------------------------------------------------------------------------*/
PROCESS(example_aggregate_process, "Test aggregate process");
AUTOSTART_PROCESSES(&example_aggregate_process);
/*---------------------------------------------------------------------------*/
static void recv(const char *aggregate, int32_t result)
{
  printf("Current %s: %ld\n", aggregate, result);
}
/*---------------------------------------------------------------------------*/
static const struct aggregate_callbacks callbacks = { recv };
/*---------------------------------------------------------------------------*/
PROCESS_THREAD(example_aggregate_process, ev, data)
{
  static struct etimer periodic;
  static struct etimer et;

  PROCESS_BEGIN();

  aggregate_open(&tc, 130, AGGREGATE_ROUTER, &callbacks);

  if(linkaddr_node_addr.u8[0] == 1 &&
     linkaddr_node_addr.u8[1] == 0) {
    printf("I am sink\n");
    aggregate_set_sink(&tc, 1);
  }

  /* Allow some time for the network to settle. */
  etimer_set(&et, 120 * CLOCK_SECOND);
  PROCESS_WAIT_UNTIL(etimer_expired(&et));

  //aggregate_set_aggregate(&tc, "max"); // "sum" is the default.


  while(1) {
    static uint8_t ctr = 0;
    /* Send a packet every 30 seconds. */
    if(etimer_expired(&periodic)) {
      etimer_set(&periodic, CLOCK_SECOND * 30);
      etimer_set(&et, random_rand() % (CLOCK_SECOND * 30));

      switch (ctr / 4) {
        case 0:
          aggregate_set_aggregate(&tc, "sum");
          break;
        case 1:
          aggregate_set_aggregate(&tc, "max");
          break;
        case 2:
          aggregate_set_aggregate(&tc, "count");
          break;
        case 3:
          aggregate_set_aggregate(&tc, "avg");
          break;
        case 4:
          aggregate_set_aggregate(&tc, "min");
          break;
        default:
          ctr = 0;
          break;
      }
      ctr++;
    }

    PROCESS_WAIT_EVENT();


    if(etimer_expired(&et)) {
      static linkaddr_t oldparent;
      const linkaddr_t *parent;
      int32_t value;


      // Let's send a part of my address as my value:
      value = (int32_t)linkaddr_node_addr.u8[0];
      aggregate_send(&tc, value);

      parent = aggregate_parent(&tc);
      if(!linkaddr_cmp(parent, &oldparent)) {
        if(!linkaddr_cmp(&oldparent, &linkaddr_null)) {
          printf("#L %d 0\n", oldparent.u8[0]);
        }
        if(!linkaddr_cmp(parent, &linkaddr_null)) {
          printf("#L %d 1\n", parent->u8[0]);
        }
        linkaddr_copy(&oldparent, parent);
      }
    }

  }

  PROCESS_END();
}
/*---------------------------------------------------------------------------*/
