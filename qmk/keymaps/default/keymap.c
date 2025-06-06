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
  TP_CTRL_B,
};

tap_dance_action_t tap_dance_actions[] = {
  [TP_CTRL_B] = ACTION_TAP_DANCE_HOLD_TAP(KC_LCTL, KC_B),
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
 * |  €   |      |   `  |   '  |   "  |           |  -   |  $   |  ~   |  @   |  :   |
 * |------+------+------+------+------|           |------+------+------+------+------|
 * |  {   |  <   |   [  |   (  |   /  |           |  ?   |   &  |   =  |   +  |  *   |
 * |------+------+------+------+------|           |------+------+------+------+------|
 * |  }   |  >   |  ]   |   )  |   \  |           |  !   |   |  |  #   |  ^   |  %   |
 * `----------------------------------'           `----------------------------------'
 *                  ,--------------------.
 *                  | R Ctl|      | \_/  |
 *                  `--------------------'
 */
[_RAISE] = LAYOUT_split_3x5_3(
  US_EURO,      _______,    KX_GRAVE,    KX_QUOTE,       KX_DQUOTE,      KC_MINUS,        KC_DOLLAR,  KX_TILDE,  KC_AT,    KC_COLON,
  KC_LCBR,      KC_LABK,    KC_LBRC,     KC_LPRN,        KC_SLASH,       KC_QUESTION,     KC_AMPR,    KC_EQUAL,  KC_PLUS,  KC_ASTR,
  KC_RCBR,      KC_RABK,    KC_RBRC,     KC_RPRN,        KC_NUBS,        KC_EXLM,         KC_PIPE,    KC_HASH,   KX_CIRCUMFLEX,  KC_PERC,
                           KC_RCTL,    _______,   _______,         _______,         _______,    _______
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
 *                                             |  \_/  | win  | S+M |
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
 * |      |      |      |      | Reset|           |  f1  |  f2  |  f3  |  f4  |home  |
 * |------+------+------+------+------|           |------+------+------+------+------|
 * |      |      |      |      |      |           |  f5  |  f6  |  f7  |  f8  |pgup  |
 * |------+------+------+------+------|           |------+------+------+------+------|
 * | C+M+D|      |      |      | Game |           |  f9  | f10  | f11  |  f12 |pgdwn |
 * `----------------------------------'           `----------------------------------'
 */
[_FUNC] = LAYOUT_split_3x5_3(
  _______, _______, _______, _______, QK_BOOT,        KC_F1, KC_F2,  KC_F3,  KC_F4,  KC_HOME,
  _______, _______, _______, _______, _______,        KC_F5, KC_F6,  KC_F7,  KC_F8,  KC_PGUP,
  LCA(KC_DEL), _______, _______, _______, DF(_GAME),      KC_F9, KC_F10, KC_F11, KC_F12, KC_PGDN,
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
         KC_T,  KC_Q,  KC_W,  KC_E,  KC_R,               _______,   _______,   _______,   _______,  _______,
         KC_G,  KC_A,  KC_S,  KC_D,  KC_F,               _______,   _______,   _______,   _______,  _______,
         TD(TP_CTRL_B),  KC_Z,  KC_X,  KC_C,  KC_V,      _______,   _______,   _______,   _______,  _______,
         KX_CTRL_SPACE, KC_LSFT, LT(LOWER,KC_SPACE),     DF(_QWERTY), _______,  _______
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
      return false;
    }
    break;
  case KX_DQUOTE:
    if (record->event.pressed) {
      tap_code16(KC_DQUO);
      tap_code(KC_SPACE);
      return false;
    }
    break;
  case KX_GRAVE:
    if (record->event.pressed) {
      tap_code(KC_GRAVE);
      tap_code(KC_SPACE);
      return false;
    }
    break;
  case KX_TILDE:
    if (record->event.pressed) {
      tap_code16(KC_TILDE);
      tap_code(KC_SPACE);
      return false;
    }
    break;
  case KX_CTRL_SPACE:
    if (record->event.pressed) {
      register_code(KC_LCTL);
      wait_ms(100);
      register_code(KC_SPACE);
    } else {
      unregister_code(KC_SPACE);
      unregister_code(KC_LCTL);
    }
    return false;
  case KX_CIRCUMFLEX:
    if (record->event.pressed) {
      tap_code16(KC_CIRCUMFLEX);
      tap_code(KC_SPACE);
      return false;
    }
    break;
  }
  return true; // Process all other keycodes normally
}
