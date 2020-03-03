/**
 * The RadioComponent interface provides to other modules functionalities
 * handled by the RadioComponentP.
 */
interface RadioComponent {
  /**
   * Initiates a broadcast of measured temperature and humidity.
   *
   * @param temp the temperature as elsius degrees.
   * @param humid the humidity as percentage.
   */
  command void sendReadings(float temp, float humid);
}
