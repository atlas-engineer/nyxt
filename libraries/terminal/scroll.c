typedef struct GhosttyTerminalImpl *GhosttyTerminal;

typedef union {
  long delta;
  unsigned long row;
  long delta_prompt;
  unsigned long long padding[2];
} nyxt_scroll_value;

typedef struct {
  int tag;
  nyxt_scroll_value value;
} nyxt_scroll_viewport;

typedef char nyxt_scroll_viewport_size[(sizeof(nyxt_scroll_viewport) == 24) ? 1 : -1];

extern int ghostty_terminal_scroll_viewport(GhosttyTerminal terminal,
                                            nyxt_scroll_viewport behavior);

extern int ghostty_terminal_set(GhosttyTerminal terminal,
                                int option,
                                const void *value);

int nyxt_terminal_scroll(GhosttyTerminal terminal, long delta) {
  nyxt_scroll_viewport behavior = {0};
  behavior.tag = 2;
  behavior.value.delta = delta;
  return ghostty_terminal_scroll_viewport(terminal, behavior);
}

int nyxt_terminal_scroll_bottom(GhosttyTerminal terminal) {
  nyxt_scroll_viewport behavior = {0};
  behavior.tag = 1;
  return ghostty_terminal_scroll_viewport(terminal, behavior);
}

int nyxt_terminal_scrollback(GhosttyTerminal terminal,
                             unsigned long lines,
                             unsigned long bytes) {
  int lines_status = ghostty_terminal_set(terminal, 29, &lines);
  int bytes_status = ghostty_terminal_set(terminal, 28, &bytes);
  if (lines_status != 0)
    return lines_status;
  return bytes_status;
}
