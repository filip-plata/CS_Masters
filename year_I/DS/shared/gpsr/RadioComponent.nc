interface RadioComponent {
  /**
   * New radio message
   *
   * @return SUCCESS if there is a neighbour
   */
  command error_t send_value(uint16_t value, uint16_t targetId, uint32_t latitude, uint32_t longitude);
}
