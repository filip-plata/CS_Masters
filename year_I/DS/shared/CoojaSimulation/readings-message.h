#ifndef READINGS_MESSAGE_H_
#define READINGS_MESSAGE_H_

typedef struct {
  uint32_t studentID;
  uint8_t deviceID;   // From label "IWANICKI <X>"
  uint8_t readingsID; // Counter for readings
  int8_t tempInt;
  uint8_t tempDec;
  uint8_t humidInt;
  uint8_t humidDec;
  uint8_t lightInt;
  uint8_t lightDec;
} readings_message_t;

enum {
  RIME_CHANNEL = 146, // For unicast_open()
};

#endif /* READINGS_MESSAGE_H_ */
