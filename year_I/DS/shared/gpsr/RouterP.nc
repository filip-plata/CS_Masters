#include <stdint.h>

#define MAX_MOTES_ 1000


module RouterP {
  provides interface ReadRouter;
  provides interface UpdateRouter;

  uses {
    interface Boot;

    interface ReadLocation;

    interface Timer<TMilli> as TimerLocationRead;
  }
}

implementation {
  const uint32_t BEACON_VALID_THR = 4000;

  typedef struct coords {
    int64_t longitude;
    int64_t latitude;
  } coords;

  struct location {
     uint32_t beacon_dt;
     uint32_t longitude;
     uint32_t latitude;
     uint8_t valid;
  };

  struct point {
    uint32_t longitude;
    uint32_t latitude;
    uint16_t id;
    uint8_t valid;
  };

  typedef struct location location;
  typedef struct point point;

  static location neighbours[MAX_MOTES_];
  static point planarized[MAX_MOTES_];
  static uint32_t planarized_count = 0;

  location self_loc;
  bool initialized = FALSE;

  void initialize();
  uint64_t distance(uint32_t x1_, uint32_t y1_, uint32_t x2_, uint32_t y2_);
  bool first_counterclokwise(coords origin, coords reference, uint16_t*);
  void planarize_neighbours();
  bool check_for_intersection(sensor_readings_t *sensor, uint16_t next_mote, uint64_t *dist);

  coords loc_to_coords(struct location *l) {
    coords res;
    res.longitude = l->longitude;
    res.latitude = l->latitude;
    return res;
  }

  coords point_to_coords(struct point *p) {
    coords res;
    res.longitude = p->longitude;
    res.latitude = p->latitude;
    return res;
  }

  command error_t ReadRouter.self_cords(uint32_t *lat, uint32_t *longi) {
    if (initialized) {
      *lat = self_loc.latitude;
      *longi = self_loc.longitude;
      return SUCCESS;
    } else {
      return EBUSY;
    }
  }

  command error_t ReadRouter.next_hop(sensor_readings_t *sensor, uint16_t *next_mote) {
    uint32_t longitude = sensor->longitude, latitude = sensor->latitude, ct = call TimerLocationRead.getNow();
    uint64_t smallest_distance, dist;
    uint16_t i;
    uint8_t header = sensor->header;
    coords origin, reference;
    bool found_next = FALSE, should_planarize = FALSE;

    if (!initialized) {
      return EOFF;
    }

    switch (header) {
      case GREEDY_FORWARDING:
        smallest_distance = distance(latitude, longitude, self_loc.latitude, self_loc.longitude);
        break;
      case PERIMETER_ROUTING:
        smallest_distance = distance(sensor->p_latitude, sensor->p_longitude, latitude, longitude);
        break;
      default:
        return FAIL;
    }

    for (i = 0; i < MAX_MOTES_; i++) {
      if (ct > neighbours[i].beacon_dt) {
        if (neighbours[i].valid) {
          dbg("DEBUG", "Mote %u disappeared\n", i);
          neighbours[i].valid = FALSE;
          should_planarize  = TRUE;
        }
        continue;
      }

      dist = distance(latitude, longitude, neighbours[i].latitude, neighbours[i].longitude);
      //dbg("DEBUG", "Distance of neigh %u is %llu\n", i, dist);

      if (dist < smallest_distance) {
        //dbg("DEBUG", "Found greedy %u\n", i);
        found_next = TRUE;
        *next_mote = i;
        smallest_distance = dist;
      }
    }

    if (found_next) {
      //dbg("DEBUG", "Sending greedily package to %lu\n", *next_mote);
      sensor->header = GREEDY_FORWARDING;
      sensor->senderID = TOS_NODE_ID;
      return SUCCESS;
    }

    /* Switching to PERIMETER_ROUTING */

    if (should_planarize)
      planarize_neighbours();

    origin = loc_to_coords(&self_loc);

    switch (sensor->header) {
      case GREEDY_FORWARDING:
        sensor->p_latitude = self_loc.latitude;
        sensor->p_longitude = self_loc.longitude;
        sensor->header = PERIMETER_ROUTING;
        reference.latitude = sensor->latitude;
        reference.longitude = sensor->longitude;
        found_next = first_counterclokwise(origin, reference, next_mote);
        sensor->f_dist_sq = distance(self_loc.longitude, self_loc.latitude,
                              reference.longitude, reference.latitude);
        sensor->e_0_senderID = TOS_NODE_ID;
        sensor->e_0_receiverID = *next_mote;
        break;
      case PERIMETER_ROUTING:
        dbg("DEBUG", "Received perimeter routing from %u\n", sensor->senderID);
        if (!neighbours[sensor->senderID].valid)
          return FAIL;
        reference = loc_to_coords(neighbours + sensor->senderID);
        found_next = first_counterclokwise(origin, reference, next_mote);
        dbg("DEBUG", "Next counterclokwise is %u\n", *next_mote);

        if (found_next &&
            sensor->e_0_senderID == TOS_NODE_ID &&
            sensor->e_0_receiverID == *next_mote)
            return FAIL;

        while (found_next && check_for_intersection(sensor, *next_mote, &smallest_distance)) {
          dbg("DEBUG", "Edges intersecting with dist: %llu\n", smallest_distance);
          reference = loc_to_coords(neighbours + *next_mote);
          found_next = first_counterclokwise(origin, reference, next_mote);

          if (smallest_distance < sensor->f_dist_sq) {
            sensor->f_dist_sq = smallest_distance;

            if (found_next) {
              sensor->e_0_senderID = TOS_NODE_ID;
              sensor->e_0_receiverID = *next_mote;
            }
          }
        }
        break;
      default:
        return FAIL;
    }

    if (found_next) {
      dbg("DEBUG", "Perimeter routing to %u from %u\n", *next_mote, TOS_NODE_ID);
      sensor->header = PERIMETER_ROUTING;
      sensor->senderID = TOS_NODE_ID;
      return SUCCESS;
    } else {
      return FAIL;
    }
  }

  command void UpdateRouter.update_beacon(uint16_t moteId, uint32_t longitude, uint32_t latitude) {
    neighbours[moteId].longitude = longitude;
    neighbours[moteId].latitude = latitude;
    neighbours[moteId].beacon_dt = call TimerLocationRead.getNow() + BEACON_VALID_THR;

    if (!neighbours[moteId].valid) {
      neighbours[moteId].valid = TRUE;
      planarize_neighbours();
    }
  }

  event void Boot.booted() {
    initialize();
  }

  event void TimerLocationRead.fired() {
    initialize();
  }

  event void ReadLocation.readDone(error_t result, uint32_t longitude, uint32_t latitude) {
    if (result == SUCCESS) {
      self_loc.longitude = longitude;
      self_loc.latitude = latitude;
      initialized = TRUE;
    } else {
      initialize();
    }
  }

  void initialize() {
    error_t res = call ReadLocation.read();

    if (res != SUCCESS) {
      call TimerLocationRead.startOneShot(1000);
    }
  }

  uint64_t distance(uint32_t x1_, uint32_t y1_, uint32_t x2_, uint32_t y2_) {
    uint64_t dx = x1_ > x2_ ? (x1_ - x2_) : (x2_ - x1_);
    uint64_t dy = y1_ > y2_ ? (y1_ - y2_) : (y2_ - y1_);

    return dx * dx + dy * dy;
  }

  void planarize_RNG() {
    uint16_t iter, v, w;
    point p;
    uint64_t d_u_w, d_v_w, d_u_v;

    planarized_count = 0;

    for (iter = 0; iter < MAX_MOTES_; iter++) {
      if (neighbours[iter].valid) {
        p.latitude = neighbours[iter].latitude;
        p.longitude = neighbours[iter].longitude;
        p.id = iter;
        p.valid = TRUE;

        planarized[planarized_count++] = p;
      }
    }

    for (v = 0; v < planarized_count; v++) {
      for (w = 0; w < planarized_count; w++) {
        if (w == v)
          continue;
        else {
          d_v_w = distance(planarized[v].latitude, planarized[v].longitude,
                           planarized[w].latitude, planarized[w].longitude);
          d_u_w = distance(self_loc.latitude, self_loc.longitude,
                           planarized[w].latitude, planarized[w].longitude);
          d_u_v = distance(self_loc.latitude, self_loc.longitude,
                           planarized[v].latitude, planarized[v].longitude);

          if (d_u_v > d_u_w && d_u_v > d_v_w) {
            planarized[v].valid = FALSE;
            break;
          }
        }
      }
    }

    v = 0;
    while (v < planarized_count) {
      if (!planarized[v].valid) {
        planarized[v] = planarized[planarized_count - 1];
        planarized_count--;
      } else {
        v++;
      }
    }
  }

  void planarize_neighbours() {
    //dbg("DEBUG", "Planarizing neighbours\n");
    planarize_RNG();
  }

  bool coords_equal(coords a, coords b) {
    return a.latitude == b.latitude && a.longitude == b.longitude;
  }

  int64_t cross_product(coords a, coords b) {
    return a.latitude * b.longitude - a.longitude * b.latitude;
  }

  coords diff_coords(coords a, coords b) {
    coords res;
    res.latitude = a.latitude - b.latitude;
    res.longitude = a.longitude - b.longitude;
    return res;
  }

  bool cmp_coords(coords origin, coords reference, coords a, coords b) {
    coords da = diff_coords(a, origin), db = diff_coords(b, origin),
           dreference = diff_coords(reference, origin);
    int64_t detb = cross_product(dreference, db), deta = cross_product(dreference, da);

    if (detb == 0 &&
        (db.longitude * dreference.longitude +
         db.latitude * dreference.latitude) >= 0)
      return FALSE;

    if (deta == 0 &&
        (da.longitude * dreference.longitude +
         da.latitude * dreference.latitude) >= 0)
      return TRUE;

    if (deta * detb >= 0)
      return cross_product(da, db);

    return deta > 0;
  }

  bool first_counterclokwise(coords origin, coords reference, uint16_t *mote_id) {
    uint16_t iter;
    point p;
    bool reference_is_planar_neighbour = FALSE;

    /* Use reference point as next only if there is no other edge */
    for (iter = 0; iter < planarized_count; iter++) {
      //dbg("DEBUG", "Planarized neigh %u\n", planarized[iter].id);
      if (coords_equal(point_to_coords(&planarized[iter]), reference)) {
        if (planarized_count == 1) {
          /* Sending packet back, nothing else to do */
          *mote_id = planarized[iter].id;
          return TRUE;
        }

        p = planarized[iter];
        planarized[iter] = planarized[planarized_count - 1];
        planarized[planarized_count - 1] = p;

        reference_is_planar_neighbour = TRUE;
        planarized_count--;
        break;
      }
    }

    dbg("DEBUG", "Planarized count: %d\n", planarized_count);
    p = planarized[0];

    for (iter = 0; iter < planarized_count; iter++) {
      if (cmp_coords(origin, reference, point_to_coords(&p), point_to_coords(&planarized[iter]))) {
        dbg("DEBUG", "Found better point %u\n", planarized[iter].id);
        p = planarized[iter];
      }
    }

    *mote_id = p.id;

    if (reference_is_planar_neighbour)
      planarized_count++;

    return TRUE;
  }

  bool get_line_intersection(double p0_x, double p0_y, double p1_x, double p1_y,
    double p2_x, double p2_y, double p3_x, double p3_y, double *i_x, double *i_y)
  {
      double s1_x, s1_y, s2_x, s2_y, s, t;
      s1_x = p1_x - p0_x;     s1_y = p1_y - p0_y;
      s2_x = p3_x - p2_x;     s2_y = p3_y - p2_y;

      s = (-s1_y * (p0_x - p2_x) + s1_x * (p0_y - p2_y)) / (-s2_x * s1_y + s1_x * s2_y);
      t = ( s2_x * (p0_y - p2_y) - s2_y * (p0_x - p2_x)) / (-s2_x * s1_y + s1_x * s2_y);

      if (s > 0 && s < 1 && t > 0 && t < 1)
      {
          // Collision detected
          if (i_x != NULL)
              *i_x = p0_x + (t * s1_x);
          if (i_y != NULL)
              *i_y = p0_y + (t * s1_y);
          return TRUE;
      }

      return FALSE; // No collision
  }

  bool check_for_intersection(sensor_readings_t *sensor, uint16_t next_mote, uint64_t *dist) {
    double p0_x = sensor->p_longitude, p0_y = sensor->p_latitude;
    double p1_x = sensor->longitude, p1_y = sensor->latitude;
    double p2_x = self_loc.longitude, p2_y = self_loc.latitude;
    double p3_x = neighbours[next_mote].longitude, p3_y = neighbours[next_mote].latitude;

    double i_x, i_y, d_x, d_y;

    if (get_line_intersection(p0_x, p0_y, p1_x, p1_y, p2_x, p2_y, p3_x, p3_y, &i_x, &i_y)) {
      d_x = p1_x - i_x;
      d_y = p1_y - i_y;

      *dist = (uint64_t) d_x * d_x + d_y * d_y;
      return TRUE;
    } else
        return FALSE;
  }
}
