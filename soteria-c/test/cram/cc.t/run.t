  $ soteria-c exec-main array_add.c --no-ignore-parse-failures --no-ignore-duplicate-symbols
  Symex terminated with the following outcomes:
    [Ok: (0,
          { heap =
            [(V|1|, Freed);
             (V|2|, [Uninit {offset = 0; len = 24}; (Bound 24)]);
             (V|3|, Freed); (V|4|, Freed); (V|5|, Freed);
             (V|6|,
              [TypedVal {offset = 0; ty = size_t; v = 10};
               TypedVal {offset = 8; ty = size_t; v = 16};
               TypedVal {offset = 16; ty = void**; v = &(V|32|, 0)};
               (Bound 24)]);
             (V|7|, Freed); (V|8|, Freed); (V|9|, Freed); (V|10|, Freed);
             (V|11|, Freed); (V|12|, Freed); (V|13|, Freed); (V|14|, Freed);
             (V|15|, Freed); (V|16|, Freed); (V|17|, Freed); (V|18|, Freed);
             (V|19|, Freed); (V|20|, Freed); (V|21|, Freed); (V|22|, Freed);
             (V|23|, Freed); (V|24|, Freed); (V|25|, Freed); (V|26|, Freed);
             (V|27|, Freed); (V|28|, Freed); (V|29|, Freed); (V|30|, Freed);
             (V|31|, Freed);
             (V|32|,
              [TypedVal {offset = 0; ty = void*; v = &(0, 0)};
               TypedVal {offset = 8; ty = void*; v = &(0, 0)};
               TypedVal {offset = 16; ty = void*; v = &(0, 0)};
               TypedVal {offset = 24; ty = void*; v = &(0, 0)};
               TypedVal {offset = 32; ty = void*; v = &(0, 0)};
               TypedVal {offset = 40; ty = void*; v = &(0, 0)};
               TypedVal {offset = 48; ty = void*; v = &(0, 0)};
               TypedVal {offset = 56; ty = void*; v = &(0, 0)};
               TypedVal {offset = 64; ty = void*; v = &(0, 0)};
               TypedVal {offset = 72; ty = void*; v = &(0, 0)};
               Uninit {offset = 80; len = 48}; (Bound 128)]);
             (V|33|, Freed); (V|34|, Freed)];
            globs = [] });
     Error: Buffer overflow or underflow with trace
            [• Called from here: array_add.c:127:7-31;
             • Triggering memory operation: array_add.c:79:3-33 (cursor: 79:24)];
     Ok: (1,
          { heap =
            [(V|1|, Freed);
             (V|2|, [Uninit {offset = 0; len = 24}; (Bound 24)]);
             (V|3|, Freed); (V|4|, Freed); (V|5|, Freed); (V|6|, Freed);
             (V|7|, Freed)];
            globs = [] });
     Ok: (1,
          { heap =
            [(V|1|, Freed);
             (V|2|, [Uninit {offset = 0; len = 24}; (Bound 24)]);
             (V|3|, Freed); (V|4|, Freed); (V|5|, Freed)];
            globs = [] });
     Ok: (1, { heap = [(V|1|, Freed)]; globs = [] })]
  Executed 163 statements
