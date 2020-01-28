use core::cell::{RefCell};

use cortex_m::interrupt::{free, Mutex, CriticalSection};

use core::ops::DerefMut;
use crate::leds::do_led_command;

#[derive(Copy, Clone)]
pub(crate) struct CommandParser {
    parser_state: CommandParserState,
    led_number: u8,
    led_command: CommandLEDType,
}


pub(crate) struct CommandLED {
    pub(crate) command_type: CommandLEDType,
    pub(crate) led_number: u8,
}

#[derive(Copy, Clone, PartialEq)]
enum CommandParserState {
    START,
    NUM,
    TYPE,
    DONE,
}

#[derive(Copy, Clone)]
pub(crate) enum CommandLEDType {
    On,
    Off,
    Toggle,
}

static COMMAND_PARSER: Mutex<RefCell<Option<CommandParser> >> =
    Mutex::new(RefCell::new(None));

impl CommandParser {
    pub(crate) fn new() -> CommandParser {
        CommandParser {
            parser_state: CommandParserState::START,
            led_number: 0,
            led_command: CommandLEDType::On
        }
    }

    pub(crate) fn initialize() {
        free(|cs| {
            COMMAND_PARSER.borrow(cs).replace(Some (CommandParser::new()) );
        });
    }

    pub(crate) fn advance(cs: &CriticalSection, c: u8)  {
        if let Some(ref mut command_parser) =  COMMAND_PARSER.borrow(cs).borrow_mut().deref_mut() {
            let initial_state = command_parser.parser_state;

            match (initial_state, c) {
                (CommandParserState::START, b'\n') => command_parser.parser_state = CommandParserState::START,

                (CommandParserState::START, b'l') => command_parser.parser_state = CommandParserState::NUM,

                (CommandParserState::NUM, n @ b'0'..=b'3') => {
                    command_parser.parser_state = CommandParserState::TYPE;
                    command_parser.led_number = n - b'0';
                },

                (CommandParserState::TYPE, b'o') => {
                    command_parser.parser_state = CommandParserState::DONE;
                    command_parser.led_command = CommandLEDType::On;
                },

                (CommandParserState::TYPE, b'f') => {
                    command_parser.parser_state = CommandParserState::DONE;
                    command_parser.led_command = CommandLEDType::Off;
                },

                (CommandParserState::TYPE, b't') => {
                    command_parser.parser_state = CommandParserState::DONE;
                    command_parser.led_command = CommandLEDType::Toggle;
                },

                _ => {
                    command_parser.parser_state = CommandParserState::START;
                    return;
                }
            }

            if command_parser.parser_state == CommandParserState::DONE {
                command_parser.parser_state = CommandParserState::START;
                do_led_command(cs, command_parser.build_command());
            }
        }
    }

    fn build_command(&self) -> CommandLED {
        CommandLED {
            command_type: self.led_command,
            led_number: self.led_number,
        }
    }
}
