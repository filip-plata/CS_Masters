#include <stdio.h>
#include "net/rime/rime.h"
#include "net/netstack.h"

#include "dev/leds.h"
#include "cc2420.h"
#include "cc2420_const.h"
#include "dev/spi.h"
#include "cfs/cfs.h"
#include "cfs/cfs-coffee.h"

#include "contiki.h"

#include "flash-radio.h"

#define MSMTS_FILENAME "rssi"
#define CHANNEL_FILENAME "ch"

#define MAX_READING -10
#define MIN_READING -100
#define BUF_SIZE (MAX_READING - MIN_READING + 1)
#define MIN_CHANNEL 11
#define MAX_CHANNEL 26
#define RSSI_RECORD_SIZE 16

/*---------------------------------------------------------------------------*/
PROCESS(radio_process, "R");
/*---------------------------------------------------------------------------*/

typedef struct record {
  char rssis[RSSI_RECORD_SIZE];
} record;

static uint32_t stats_buf[BUF_SIZE];

static int fd_msm, fd_channel;
static unsigned char channel;

void read_channel_info() {
  record rec;
  int r;

  printf("channel: %d\n", channel);
  cfs_seek(fd_msm, 0, CFS_SEEK_SET);

  for (r = 0; r < BUF_SIZE; r++)
    stats_buf[r] = 0;

  leds_on(LEDS_GREEN);
  while (1) {
    r = cfs_read(fd_msm, &rec, sizeof(record));
    if(r == 0) {
      break;
    } else if(r < sizeof(record)) {
      return;
    }

    for (r = 0; r < RSSI_RECORD_SIZE; r++)
      stats_buf[rec.rssis[r] - MIN_READING]++;
  }
  leds_off(LEDS_GREEN);

  for (r = 0; r < BUF_SIZE; r++)
      printf("%d: %lu\n", r + MIN_READING, stats_buf[r]);

  cfs_close(fd_msm);
  cfs_remove(MSMTS_FILENAME);
  fd_msm = cfs_open(MSMTS_FILENAME, CFS_WRITE | CFS_APPEND | CFS_READ);

  channel++;
  channel %= (MAX_CHANNEL + 1);
  if (channel < MIN_CHANNEL)
    channel = MIN_CHANNEL;

  cfs_seek(fd_channel, 0, CFS_SEEK_SET);
  cfs_write(fd_channel, &channel, sizeof(channel));
  cc2420_set_channel(channel);
}

static int write_to_flash(int fd, const void *data, size_t size) {
  int res;
  leds_on(LEDS_BLUE);
  res = cfs_write(fd, data, size);
  leds_off(LEDS_BLUE);

  return res;
}

/*---------------------------------------------------------------------------*/
PROCESS_THREAD(radio_process, ev, data)
{
  static radio_value_t rssi;
  static int record_idx = 0, r;
  static record curr_record;

  PROCESS_BEGIN();
  fd_msm = cfs_open(MSMTS_FILENAME, CFS_WRITE | CFS_APPEND | CFS_READ);
  fd_channel = cfs_open(CHANNEL_FILENAME, CFS_WRITE | CFS_APPEND | CFS_READ);

  cfs_seek(fd_channel, 0, CFS_SEEK_SET);
  r = cfs_read(fd_channel, &channel, sizeof(channel));
  if (r < 0) {
    printf("%d shdsgjgfskdfdksgfsdsfdk\n", r);
    channel = MIN_CHANNEL;
    cfs_write(fd_channel, &channel, sizeof(channel));
  }
  NETSTACK_MAC.off(0);
  cc2420_on();
  cc2420_set_channel(channel);

  while(1) {
    rssi = cc2420_rssi();

    if (MIN_READING <= rssi && rssi <= MAX_READING)
      curr_record.rssis[record_idx++] = (char) rssi;

    if (record_idx == RSSI_RECORD_SIZE) {
      write_to_flash(fd_msm, &curr_record, sizeof(record));
      record_idx = 0;
    }

    PROCESS_PAUSE();
  }

  PROCESS_END();
}
