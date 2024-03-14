#include QMK_KEYBOARD_H

// TRIPLE TAP SETUP
typedef struct {
    uint16_t open;
    uint16_t close;
} tap_dance_tap_triple_t;

void tap_dance_tap_hold_fn(tap_dance_state_t *state, void *user_data) {
  tap_dance_tap_triple_t *tap_triple = (tap_dance_tap_triple_t *)user_data;

  if (state->count == 1) {
    tap_code16(tap_triple->open);
  } else if (state->count == 2) {
    tap_code16(tap_triple->close);
  }  else {
    tap_code16(tap_triple->open);
    tap_code16(tap_triple->close);
  }
}

#define ACTION_TAP_DANCE_TRIPLE_AUTO(OPEN, CLOSE) \
  { .fn = {NULL, tap_dance_tap_hold_fn, NULL, NULL}, .user_data = (void *)&((tap_dance_tap_triple_t){OPEN, CLOSE}), }

typedef struct {
    bool is_press_action;
    uint8_t step;
} tap;

// HOLD AND TAP
typedef struct {
  uint16_t hold;
  uint16_t tap;
} tap_dance_tap_hold_t;

enum {
  SINGLE_TAP = 1,
  SINGLE_HOLD,
  MORE_TAPS
};

static tap dance_state[1];

uint8_t dance_step(tap_dance_state_t *state) {
  if (state->interrupted || !state->pressed) return SINGLE_TAP;
  else return SINGLE_HOLD;
}

void on_dance_0(tap_dance_state_t *state, void *user_data) {
  tap_dance_tap_hold_t *tap_hold = (tap_dance_tap_hold_t *)user_data;
  if(state->count == 3) {
    tap_code16(tap_hold->tap);
    tap_code16(tap_hold->tap);
    tap_code16(tap_hold->tap);
  }
  if(state->count > 3) {
    tap_code16(tap_hold->tap);
  }
}

void dance_0_finished(tap_dance_state_t *state, void *user_data) {
  tap_dance_tap_hold_t *tap_hold = (tap_dance_tap_hold_t *)user_data;

  dance_state[0].step = dance_step(state);
  switch (dance_state[0].step) {
  case SINGLE_TAP: register_code16(tap_hold->tap); break;
  case SINGLE_HOLD: register_code16(tap_hold->hold); break;
  }
}

void dance_0_reset(tap_dance_state_t *state, void *user_data) {
  tap_dance_tap_hold_t *tap_hold = (tap_dance_tap_hold_t *)user_data;
  wait_ms(10);
  switch (dance_state[0].step) {
  case SINGLE_TAP: unregister_code16(tap_hold->tap); break;
  case SINGLE_HOLD: unregister_code16(tap_hold->hold); break;
  }
  dance_state[0].step = 0;
}

#define ACTION_TAP_DANCE_HOLD_TAP(HOLD, TAP)                            \
  { .fn = {on_dance_0, dance_0_finished, dance_0_reset, NULL}, .user_data = (void *)&((tap_dance_tap_hold_t){HOLD, TAP}), }

