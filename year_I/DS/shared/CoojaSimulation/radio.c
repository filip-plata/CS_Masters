#include <stdio.h>
#include <math.h>

#include "contiki.h"
#include "net/rime/rime.h"

#include "dev/serial-line.h"
#include "dev/uart1.h"

#include "readings-message.h"

#define MAX_RETRANSMISSIONS 4
#define DEVICE_ID 18

static linkaddr_t target;

/*---------------------------------------------------------------------------*/
PROCESS(radio_process, "radio process");
/*---------------------------------------------------------------------------*/

void print_message(readings_message_t *msg) {
  printf("\n %02x\n", msg->studentID);
  printf("\nStudent %lu, device %u, readingId %u\n", msg->studentID, msg->deviceID, msg->readingsID);
  printf("Temperature=%d.%02u C\n", msg->tempInt, msg->tempDec);
  printf("Humidity=%d.%02u %%\n", msg->humidInt, msg->humidDec);
  printf("Light=%d.%02u lux\n", msg->lightInt, msg->lightDec);
}

static void
recv_uc(struct unicast_conn *c, const linkaddr_t *from)
{
  print_message(packetbuf_dataptr());
  printf("unicast message received from %d.%d\n",
	 from->u8[0], from->u8[1]);
}

static void
sent_uc(struct unicast_conn *c, int status, int num_tx)
{
  const linkaddr_t *dest = packetbuf_addr(PACKETBUF_ADDR_RECEIVER);
  if(linkaddr_cmp(dest, &linkaddr_null)) {
    return;
  }
  printf("unicast message sent to %d.%d: status %d num_tx %d\n",
    dest->u8[0], dest->u8[1], status, num_tx);
}


static const struct unicast_callbacks unicast_callbacks = {recv_uc, sent_uc,};
static struct unicast_conn uc;
/*---------------------------------------------------------------------------*/
PROCESS_THREAD(radio_process, ev, data)
{
  static uint8_t READINGS_ID = 0;
  static readings_message_t msg;
  PROCESS_EXITHANDLER(unicast_close(&uc);)
  //target = linkaddr_node_addr;

  PROCESS_BEGIN();

  msg.studentID = 371335;
  msg.deviceID = DEVICE_ID;

  unicast_open(&uc, RIME_CHANNEL, &unicast_callbacks);

  while(1) {
    static struct etimer et;

    etimer_set(&et, 5*CLOCK_SECOND);
    PROCESS_WAIT_EVENT_UNTIL(etimer_expired(&et));

    msg.tempInt = 23;
    msg.tempDec = 50;

    msg.humidInt = 17;
    msg.humidDec = 2;

    msg.lightInt = 100;
    msg.lightDec = 25;

    packetbuf_copyfrom(&msg, sizeof(readings_message_t));

    printf("%u.%u: sending unicast to address %u.%u\n",
     linkaddr_node_addr.u8[0],
     linkaddr_node_addr.u8[1],
     target.u8[0],
     target.u8[1]);
    msg.readingsID = READINGS_ID++;

    if (!linkaddr_cmp(&target, &linkaddr_node_addr)) {
      if (unicast_send(&uc, &target) != 0) {
        printf("Packet sent OK\n");
      } else {
        printf("Packet sent FAILED\n");
      }
    }
  }

  PROCESS_END();
}

int hex_digit_to_int(char c) {
  if ('0' <= c && c <= '9')
    return c - '0';
  if ('a' <= c && c <= 'f')
    return c - 'a' + 10;
  if ('A' <= c && c <= 'F')
    return c - 'A' + 10;
  return -1;
}


PROCESS(stdin, "Stdin");

PROCESS_THREAD(stdin, ev, data)
{
  PROCESS_BEGIN();

  uart1_set_input(serial_line_input_byte);
  serial_line_init();

  while(1) {
    static uint16_t num;
    static char *d;
    PROCESS_WAIT_EVENT_UNTIL(ev == serial_line_event_message);

    d = (char *)data;
    num = 0;
    printf("Received line: %s\n", d);

    while (*d) {
      //printf("%d\n", num);
      if (hex_digit_to_int(*d) == -1)
        break;
      num *= 16;
      num += hex_digit_to_int(*d);
      d++;
    }

    if (*d == '\0') {
      target.u8[0] = (num & 0xFF00) >> 8;
      target.u8[1] = num & 0xFF;
      printf("new: %u.%u\n", target.u8[0], target.u8[1]);
    }
  }

  PROCESS_END();
}
