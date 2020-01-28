#[link(name = "config")]
extern "C" {
    pub(crate) fn configure_bluetooth();
    pub(crate) fn configure_dma();
    pub(crate) fn configure_leds();
    pub(crate) fn configure_rotational_encoder();
    pub(crate) fn awaiting_stable_encoder();
    pub(crate) fn encoder_stable();
}