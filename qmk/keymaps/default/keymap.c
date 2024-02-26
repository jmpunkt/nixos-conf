#include QMK_KEYBOARD_H
#include "keymap_us_international.h"

// Defines names for use in layer keycodes and the keymap
enum layer_names {
    _COLEMAK,
    _QWERTY,
    _LOWER,
    _RAISE,
    _ADJUST,
    _FUNC,
};

// custom keycodes
enum custom_keycodes {
  C_FN = SAFE_RANGE,
  C_BLK,
};

#define LOWER MO(_LOWER)
#define RAISE MO(_RAISE)
#define FUNC MO(_FUNC)


const uint16_t PROGMEM keymaps[][MATRIX_ROWS][MATRIX_COLS] = {
/* Colemak
 *
 * ,----------------------------------.           ,----------------------------------.
 * |   Q  |   W  |   F  |   P  |   B  |           |   J  |   L  |   U  |   Y  |   ;  |
 * |------+------+------+------+------|           |------+------+------+------+------|
 * |   A  |   R  |   S  |   T  |   G  |           |   M  |   N  |   E  |   I  |   O  |
 * |------+------+------+------+------|           |------+------+------+------+------|
 * |   Z  |   X  |   C  |   D  |   V  |           |   K  |   H  |   ,  |   .  |   /  |
 * `-------------+--------------------'           `----------------------------------'
 *                  ,--------------------.    ,--------------------.
 *                  | Ctrl  |backsp|R/Spa|    |L/Ente|Func  | Alt  |
 *                  `--------------------'    `--------------------.
 */

// Default config uses home row mods. So hold each of the keys on the home row to use ctrl, gui, alt, or shift
[_COLEMAK] = LAYOUT_split_3x5_3(
  KC_Q,         KC_W,         KC_F,         KC_P,         KC_B,         KC_J,    KC_L,         KC_U,         KC_Y,         KC_SCLN,
  KC_A,         KC_R,         KC_S,         KC_T,         KC_G,         KC_M,    KC_N,         KC_E,         KC_I,         KC_O,
  KC_Z,         KC_X,         KC_C,         KC_D,         KC_V,         KC_K,    KC_H,         KC_COMMA,     KC_DOT,       KC_SLASH,
                              LCTL_T(KC_DEL),  KC_BSPC,      LT(RAISE,KC_SPACE),           LT(LOWER,KC_ENTER), LT(FUNC, KC_MINUS),   KC_UNDS
),

/* QWERTY
 *
 * ,----------------------------------.           ,----------------------------------.
 * |   Q  |   W  |   E  |   R  |   T  |           |   Y  |   U  |   I  |   O  |   P  |
 * |------+------+------+------+------|           |------+------+------+------+------|
 * |   A  |   S  |   D  |   F  |   G  |           |   H  |   J  |   K  |   L  |   ;  |
 * |------+------+------+------+------|           |------+------+------+------+------|
 * |   Z  |   X  |   C  |   V  |   B  |           |   N  |   M  |   ,  |   .  |   /  |
 * `----------------------------------'           `----------------------------------'
 *                  ,--------------------.    ,--------------------.
 *                  | Ctrl  |backsp|R/Spa|    |L/Ente|Func  | Alt  |
 *                  `--------------------'    `--------------------.
 */

[_QWERTY] = LAYOUT_split_3x5_3(
  KC_Q,         KC_W,         KC_E,         KC_R,         KC_T,         KC_Y,    KC_U,         KC_I,         KC_O,         KC_P,
  KC_A,         KC_S,         KC_D,         KC_F,         KC_G,         KC_H,    KC_J,         KC_K,         KC_L,         KC_SCLN,
  KC_Z,         KC_X,         KC_C,         KC_V,         KC_B,         KC_N,    KC_M,         KC_COMMA,     KC_DOT,       KC_SLASH,
                              LCTL_T(KC_DEL),  KC_BSPC,      LT(RAISE,KC_SPACE),           LT(LOWER,KC_ENTER), LT(FUNC, KC_MINUS),   KC_UNDS
),
/* Raise
 *
 * ,----------------------------------.           ,----------------------------------.
 * |   ^  |   !  |   <  |   >  | altT |           |   ({ |   => |   -  |   \  |   ~  |
 * |------+------+------+------+------|           |------+------+------+------+------|
 * |  |   |   ;  |   {  |   }  |   '  |           |  *   |   (  |   )  |   /  |   #  |
 * |------+------+------+------+------|           |------+------+------+------+------|
 * |  &   |   :  |  [   |  ]   |   `  |           |  +   |   =  |   ,  |   .  |   ?  |
 * `----------------------------------'           `----------------------------------'
 *                  ,--------------------.    ,--------------------.
 *                  |      |      |      |    |      | CapsW|  _   |
 *                  `--------------------'    `--------------------.
 */
[_RAISE] = LAYOUT_split_3x5_3(
  LSFT(KC_6),     KC_EXLM,      KC_LT,       KC_GT,       KC_TAB,             C_BLK,            C_FN,       KC_MINUS,   KC_NUBS,    KC_TILD,
  KC_BSPC,        KC_SCLN,      KC_LCBR,     KC_RCBR,     KC_QUOTE,           LSFT(KC_8),       KC_LPRN,    KC_RPRN,    KC_SLASH,   LSFT(KC_3),
  KC_AMPR,        KC_COLN,      KC_LBRC,     KC_RBRC,     KC_GRAVE,           LSFT(KC_EQUAL),   KC_EQUAL,   KC_COMMA,   KC_DOT,     KC_SLASH,
                                _______,     _______,     _______,            _______,          CW_TOGG,    KC_LALT
),

/* Lower
 *
 * ,----------------------------------.           ,----------------------------------.
 * | esc  |   1  |   2  |   3  |   .  |           |   F1 |  F2  |  F3  |  F4  | Home |
 * |------+------+------+------+------|           |------+------+------+------+------|
 * | shift|   4  |   5  |   6  |   ,  |           |   F5 |  F6  |  F7  |  F8  | PgUp |
 * |------+------+------+------+------|           |------+------+------+------+------|
 * | ctrl |   7  |   8  |   9  |   0  |           |   F9 |  F10 |  F11 |  F12 | PgDn |
 * `----------------------------------'           `----------------------------------'
 *                  ,--------------------.    ,--------------------.
 *                  | alt  | win |       |    |     |       |      |
 *                  `--------------------'    `--------------------.
 */
[_LOWER] = LAYOUT_split_3x5_3(
  KC_ESCAPE,  KC_1, KC_2,    KC_3,    KC_DOT,       KC_F1, KC_F2, KC_F3, KC_F4, KC_HOME,
  KC_LSFT,    KC_4, KC_5,    KC_6,    KC_COMMA,     KC_F5, KC_F6, KC_F7, KC_F8, KC_PGUP,
  KC_LCTL,    KC_7, KC_8,    KC_9,    KC_0,         KC_F9, KC_F10, KC_F11, KC_F12,   KC_PGDN,
                    KC_LALT, KC_LGUI, _______,      _______,  _______, _______
),

/* Adjust (Lower + Raise)
 *
 * ,----------------------------------.           ,----------------------------------.
 * |      |      |      |      |COLEMK|           |      |loud- |mute  |loud+ |      |
 * |------+------+------+------+------|           |------+------+------+------+------|
 * |      |      |      |      |QWERTY|           |      |prev  |Play  |next  |      |
 * |------+------+------+------+------|           |------+------+------+------+------|
 * |      |  cut |copy  |pase  |      |           |      |      |      |      | Reset|
 * `----------------------------------'           `----------------------------------'
 *                  ,--------------------.    ,--------------------.
 *                  |      |      |      |    |      |      |      |
 *                  `--------------------'    `--------------------.
 */
[_ADJUST] =  LAYOUT_split_3x5_3(
  _______, _______, _______, _______, TO(_COLEMAK),       _______,  KC_VOLD,  KC_MUTE,   KC_VOLU,    _______,
  _______, _______,  _______, _______, TO(_QWERTY),       _______,  KC_MPRV,  KC_MPLY,   KC_MNXT,    _______,
  _______, KC_CUT  ,  KC_COPY, KC_PASTE, _______,         _______,  KC_BRID,  _______,   KC_BRIU,   QK_BOOT,
                     _______, _______, _______,           _______, _______,   _______
),

/* Function layer 
 *
 * ,----------------------------------.           ,----------------------------------.
 * |      |      |      |c+s+P |      |           |      |      |   UP |      |      |
 * |------+------+------+------+------|           |------+------+------+------+------|
 * |      |      |  c+S |c+s+F |      |           |      | LEFT | DOWN | RIGHT|      |
 * |------+------+------+------+------|           |------+------+------+------+------|
 * |      |      |      | c+P  |      |           |      |      |      |      |      |
 * `----------------------------------'           `----------------------------------'
 *                  ,--------------------.    ,--------------------.
 *                  |      |      |      |    |      |      |      |
 *                  `--------------------'    `--------------------.
 */
[_FUNC] = LAYOUT_split_3x5_3(
  _______, _______, A(S(KC_F)), C(S(KC_P)), _______,      _______, _______, KC_UP, _______, _______,
  _______, _______, C(KC_S), C(S(KC_F)), _______,      _______, KC_LEFT, KC_DOWN, KC_RIGHT, _______,
  MEH(KC_C), MEH(KC_F), C(KC_SLASH), C(KC_P), _______,      _______, G(S(KC_S)), G(KC_E), _______, _______,
                    KC_LCTL, KC_LGUI, _______,      _______, _______, _______
),
};


layer_state_t layer_state_set_user(layer_state_t state) {
    return update_tri_layer_state(state, _LOWER, _RAISE, _ADJUST);
}

// fn to hit these two keys after each other = and >
bool process_record_user(uint16_t keycode, keyrecord_t *record) {
  if (record->event.pressed) {
    switch (keycode) {
      case C_FN:
        SEND_STRING("=>");
        return false;
      case C_BLK:
        SEND_STRING("({");
        return false;
    }
  }
  return true;
}

// OLED
#ifdef OLED_ENABLE

bool render_layer_status(void) {
    oled_clear();
    // Host Keyboard Layer Status
    switch (get_highest_layer(layer_state)) {
        case _COLEMAK:
          if (is_keyboard_master()) {
              // colemak is default write some funny words each 4 caracters long here
              oled_write_ln_P(PSTR("if"), false);
              oled_write_ln_P(PSTR("you"), false);
              oled_write_ln_P(PSTR("read"), false);
              oled_write_ln_P(PSTR("this"), false);
              oled_write_ln_P(PSTR("you"), false);
              oled_write_ln_P(PSTR("are"), false);
              oled_write_ln_P(PSTR("a"), false);
              oled_write_ln_P(PSTR("nerd"), false);
              oled_write_ln_P(PSTR("lol"), false);
          } else {
              oled_write_ln_P(PSTR("this"), false);
              oled_write_ln_P(PSTR("is"), false);
              oled_write_ln_P(PSTR("the"), false);
              oled_write_ln_P(PSTR("right"), false);
              oled_write_ln_P(PSTR("side"), false);
              oled_write_ln_P(PSTR("or"), false);
              oled_write_ln_P(PSTR("you"), false);
              oled_write_ln_P(PSTR("plug"), false);
              oled_write_ln_P(PSTR("in"), false);
              oled_write_ln_P(PSTR("wrong"), false);
          }
        break;
        case _LOWER:
            oled_write_ln_P(PSTR("|"), false);
            oled_write_ln_P(PSTR("|"), false);
            oled_write_ln_P(PSTR("|"), false);
            oled_write_ln_P(PSTR("|"), false);
            oled_write_ln_P(PSTR("----"), false);
            oled_write_ln_P(PSTR(""), false);
            oled_write_ln_P(PSTR("----"), false);
            oled_write_ln_P(PSTR("|  |"), false);
            oled_write_ln_P(PSTR("|  |"), false);
            oled_write_ln_P(PSTR("|  |"), false);
            oled_write_ln_P(PSTR("----"), false);
            break;
        case _RAISE:
            oled_write_ln_P(PSTR("----"), false);
            oled_write_ln_P(PSTR("|  |"), false);
            oled_write_ln_P(PSTR("|--/"), false);
            oled_write_ln_P(PSTR("| \\"), false);
            oled_write_ln_P(PSTR("|  \\"), false);
            oled_write_ln_P(PSTR(""), false);
            oled_write_ln_P(PSTR(" /\\"), false);
            oled_write_ln_P(PSTR("/  \\"), false);
            oled_write_ln_P(PSTR("|--|"), false);
            oled_write_ln_P(PSTR("|  |"), false);
            oled_write_ln_P(PSTR("|  |"), false);
            break;
        case _FUNC:
            oled_write_ln_P(PSTR("----"), false);
            oled_write_ln_P(PSTR("|   "), false);
            oled_write_ln_P(PSTR("|---"), false);
            oled_write_ln_P(PSTR("|   "), false);
            oled_write_ln_P(PSTR("|   "), false);
            oled_write_ln_P(PSTR(""), false);
            oled_write_ln_P(PSTR("|  |"), false);
            oled_write_ln_P(PSTR("|\\ |"), false);
            oled_write_ln_P(PSTR("| \\|"), false);
            oled_write_ln_P(PSTR("|  \\"), false);
            oled_write_ln_P(PSTR("|  |"), false);
            break;
        case _ADJUST:
            oled_write_ln_P(PSTR(" /\\"), false);
            oled_write_ln_P(PSTR("/  \\"), false);
            oled_write_ln_P(PSTR("|--|"), false);
            oled_write_ln_P(PSTR("|  |"), false);
            oled_write_ln_P(PSTR("|  |"), false);
            oled_write_ln_P(PSTR(""), false);
            oled_write_ln_P(PSTR("|-\\"), false);
            oled_write_ln_P(PSTR("|  \\"), false);
            oled_write_ln_P(PSTR("|  |"), false);
            oled_write_ln_P(PSTR("|  /"), false);
            oled_write_ln_P(PSTR("|-/"), false);
            break;
        default:
    }
    return false;
}

oled_rotation_t oled_init_user(oled_rotation_t rotation) {
    return OLED_ROTATION_270;
}

bool oled_task_user(void) {
  render_layer_status();
  return false;
}

void oled_render_boot(bool bootloader) {
    oled_clear();
    if (bootloader) {
        oled_write_P(PSTR("Flashing"), false);
    } else {
        oled_write_P(PSTR("Rebooting"), false);
    }
}

bool shutdown_user(bool jump_to_bootloader) {
    oled_render_boot(jump_to_bootloader);
    return true;
}

#endif
