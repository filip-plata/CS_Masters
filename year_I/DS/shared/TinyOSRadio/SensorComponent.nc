/**
 * The SensorComponent interface provides to other modules functionalities
 * handled by the SensorComponentP.
 */
interface SensorComponent {
  /**
   * Initiates periodic measurements and announcements
   * of temperature and humidity.
   */
  command void startSensing();
}
