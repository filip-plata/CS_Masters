/**
 * Read interface to query for this mote router.
 */
#include "Radio.h"

interface ReadRouter {
  /**
   * Returns next hop for a given target
   *
   * @return SUCCESS if there is a neighbour
   */
  command error_t next_hop(sensor_readings_t *sensor, uint16_t *);

  command error_t self_cords(uint32_t *lat, uint32_t *longi);
}
