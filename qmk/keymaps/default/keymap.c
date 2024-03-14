#include QMK_KEYBOARD_H
#include "keymap_us_international.h"
#include "../../lib.h"

// Defines names for use in layer keycodes and the keymap
enum layer_names {
    _QWERTY,
    _GAME,
    _LOWER,
    _RAISE,
    _FUNC,
};

#define LOWER MO(_LOWER)
#define RAISE MO(_RAISE)

enum {
  KX_DQUOTE = SAFE_RANGE,
  KX_QUOTE,
  KX_GRAVE,
  KX_CIRCUMFLEX,
  KX_TILDE,
  KX_CTRL_SPACE,
};

enum {
  TP_ABK = 0,
  TP_BRC,
  TP_PRN,
  TP_CBR,
  TP_CTRL_SPACE,
};

tap_dance_action_t tap_dance_actions[] = {
  [TP_ABK]  = ACTION_TAP_DANCE_TRIPLE_AUTO(KC_LABK, KC_RABK),
  [TP_BRC]  = ACTION_TAP_DANCE_TRIPLE_AUTO(KC_LBRC, KC_RBRC),
  [TP_PRN]  = ACTION_TAP_DANCE_TRIPLE_AUTO(KC_LPRN, KC_RPRN),
  [TP_CBR]  = ACTION_TAP_DANCE_TRIPLE_AUTO(KC_LCBR, KC_RCBR),
  [TP_CTRL_SPACE] = ACTION_TAP_DANCE_HOLD_TAP(KC_LCTL, LCTL(KC_SPACE)),
};

const uint16_t PROGMEM keymaps[][MATRIX_ROWS][MATRIX_COLS] = {
/* QWERTY
 *
 * ,----------------------------------.           ,----------------------------------.
 * |   Q  |   W  |   E  |   R  |   T  |           |   Y  |   U  |   I  |   O  |   _  |
 * |------+------+------+------+------|           |------+------+------+------+------|
 * |   A  |   S  |   D  |   F  |   G  |           |   H  |   J  |   K  |   L  |   P  |
 * |------+------+------+------+------|           |------+------+------+------+------|
 * |   Z  |   X  |   C  |   V  |   B  |           |   N  |   M  |   .  |   ,  |   ;  |
 * `----------------------------------'           `----------------------------------'
 *                  ,--------------------.    ,--------------------.
 *                  | Ctrl|backsp|L/Space|    |R/RET  |Shift|Meta  |
 *                  `--------------------'    `--------------------'
 */

[_QWERTY] = LAYOUT_split_3x5_3(
  KC_Q,         KC_W,         KC_E,            KC_R,         KC_T,         KC_Y,    KC_U,         KC_I,         KC_O,         KC_UNDERSCORE,
  KC_A,         KC_S,         KC_D,            KC_F,         KC_G,         KC_H,    KC_J,         KC_K,         KC_L,         KC_P,
  KC_Z,         KC_X,         KC_C,            KC_V,         KC_B,         KC_N,    KC_M,         KC_DOT,       KC_COMMA,     KC_SCLN,
                          KC_LCTL,  KC_BSPC,    LT(LOWER,KC_SPACE),        LT(RAISE,KC_ENTER), KC_LSFT,  KC_LALT
),

/* Raise
 *
 * ,----------------------------------.           ,----------------------------------.
 * |      |      |   `  |   '  |   "  |           |  -   |  $   |  ~   |  @   |  :   |
 * |------+------+------+------+------|           |------+------+------+------+------|
 * |  {   |  <   |   [  |   (  |   /  |           |  ?   |   &  |   =  |   +  |  *   |
 * |------+------+------+------+------|           |------+------+------+------+------|
 * |      |      |      |      |   \  |           |  !   |   |  |  #   |  ^   |  %   |
 * `----------------------------------'           `----------------------------------'
 *                  ,--------------------.
 *                  | S+C  |      | \_/  |
 *                  `--------------------'
 */
[_RAISE] = LAYOUT_split_3x5_3(
  _______,      _______,    KX_GRAVE,    KX_QUOTE,       KX_DQUOTE,      KC_MINUS,        KC_DOLLAR,  KX_TILDE,  KC_AT,    KC_COLON,
  TD(TP_CBR),   TD(TP_ABK), TD(TP_BRC),  TD(TP_PRN),     KC_SLASH,       KC_QUESTION,     KC_AMPR,    KC_EQUAL,  KC_PLUS,  KC_ASTR,
  _______,      _______,    _______,     _______,        KC_NUBS,        KC_EXLM,         KC_PIPE,    KC_HASH,   KX_CIRCUMFLEX,  KC_PERC,
                           LSFT(KC_LCTL),    _______,   _______,         _______,         _______,    _______
),

/* Lower
 *
 * ,----------------------------------.           ,----------------------------------.
 * |  1   |   2  |   3  |   4  |  5   |           | ESC  | tab  | del  | pos1 | end  |
 * |------+------+------+------+------|           |------+------+------+------+------|
 * |  6   |   7  |   8  |   9  |  0   |           | left |down  |up    | right|      |
 * |------+------+------+------+------|           |------+------+------+------+------|
 * |      | ae   |  ss  |  ue  |  oe  |           |      |      |      |      |      |
 * `----------------------------------'           `----------------------------------'
 *                                             ,--------------------.
 *                                             |  \_/  | win  | C+M |
 *                                             `--------------------'
 */
[_LOWER] = LAYOUT_split_3x5_3(
  KC_1,    KC_2,    KC_3,    KC_4,    KC_5,                   KC_ESC, KC_TAB, KC_DEL, KC_HOME, KC_END,
  KC_6,    KC_7,    KC_8,    KC_9,    KC_0,                   KC_LEFT, KC_DOWN, KC_UP,   KC_RIGHT,_______,
  _______, RALT(KC_Q), RALT(KC_S), RALT(KC_Y), RALT(KC_P),    _______, _______, _______, _______, _______,
                    _______, _______, _______,                _______, KC_LGUI, LSFT(KC_LALT)
),

/* Function layer
 *
 * ,----------------------------------.           ,----------------------------------.
 * | NKRO |      |      |      | Reset|           |  f1  |  f2  |  f3  |  f4  |home  |
 * |------+------+------+------+------|           |------+------+------+------+------|
 * |      |      |      |      |      |           |  f5  |  f6  |  f7  |  f8  |pgup  |
 * |------+------+------+------+------|           |------+------+------+------+------|
 * |      |      |      |      | Game |           |  f9  | f10  | f11  |  f12 |pgdwn |
 * `----------------------------------'           `----------------------------------'
 */
[_FUNC] = LAYOUT_split_3x5_3(
  MAGIC_TOGGLE_NKRO, _______, _______, _______, QK_BOOT,        KC_F1, KC_F2,  KC_F3,  KC_F4,  KC_HOME,
  _______, _______, _______, _______, _______,        KC_F5, KC_F6,  KC_F7,  KC_F8,  KC_PGUP,
  _______, _______, _______, _______, DF(_GAME),      KC_F9, KC_F10, KC_F11, KC_F12, KC_PGDN,
                    _______, _______, _______,        _______, _______, _______
),
/* Game layer
 *
 * ,----------------------------------.           ,----------------------------------.
 * |  T   |   Q  |   W  |   E  |   R  |           |      |      |      |      |      |
 * |------+------+------+------+------|           |------+------+------+------+------|
 * |  G   |   A  |   S  |   D  |   F  |           |      |      |      |      |      |
 * |------+------+------+------+------|           |------+------+------+------+------|
 * |  B   |   Z  |   X  |   C  |   V  |           |      |      |      |      |      |
 * `----------------------------------'           `----------------------------------'
 *                  ,--------------------.    ,--------------------.
 *                  | Ctrl|Shift |L/Space|    |QWERTY|     |       |
 *                  `--------------------'    `--------------------'
*/
[_GAME] = LAYOUT_split_3x5_3(
         KC_T,  KC_Q,  KC_W,  KC_E,  KC_R,         KX_CTRL_SPACE,   _______,   _______,   _______,  _______,
         KC_G,  KC_A,  KC_S,  KC_D,  KC_F,         _______,   _______,   _______,   _______,  _______,
         KC_B,  KC_Z,  KC_X,  KC_C,  KC_V,         _______,   _______,   _______,   _______,  _______,
         TD(TP_CTRL_SPACE), KC_LSFT, LT(LOWER,KC_SPACE),     DF(_QWERTY), _______,  _______
),
};

layer_state_t layer_state_set_user(layer_state_t state) {
   return update_tri_layer_state(state, _LOWER, _RAISE, _FUNC);
}

bool process_record_user(uint16_t keycode, keyrecord_t *record) {
  switch (keycode) {
  case KX_QUOTE:
    if (record->event.pressed) {
      tap_code(KC_QUOTE);
      tap_code(KC_SPACE);
    }
    return false;
  case KX_DQUOTE:
    if (record->event.pressed) {
      tap_code16(KC_DQUO);
      tap_code(KC_SPACE);
    }
    return false;
  case KX_GRAVE:
    if (record->event.pressed) {
      tap_code(KC_GRAVE);
      tap_code(KC_SPACE);
    }
    return false;
  case KX_TILDE:
    if (record->event.pressed) {
      tap_code16(KC_TILDE);
      tap_code(KC_SPACE);
    }
    return false;
  case KX_CTRL_SPACE:
    if (record->event.pressed) {
      register_code(KC_LCTL);
      tap_code(KC_SPACE);
      unregister_code(KC_LCTL);
    }
    return false;
  case KX_CIRCUMFLEX:
    if (record->event.pressed) {
      tap_code16(KC_CIRCUMFLEX);
      tap_code(KC_SPACE);
    }
    return false;
  default:
    return true; // Process all other keycodes normally
  }
}
