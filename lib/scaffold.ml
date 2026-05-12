let build_meow name =
  Printf.sprintf
    {|project %s

toolchain {
  cc     cc
  cflags -O2 -Wall -Wextra -std=c17
}

check header stdio.h
check func sqrt in m
check sizeof void*

config_header include/%s_config.h

lib %s_core {
  srcs    src/core/*.c
  include include
  ldflags -lm
}

bin %s {
  srcs    src/main.c
  include include
  use     %s_core
  install bin
}

test %s_test {
  srcs    tests/*.c
  include include
  use     %s_core
}
|}
    name name name name name name name

let core_c name =
  Printf.sprintf
    {|#include "%s.h"
#include <math.h>

double %s_hypot(double a, double b) { return sqrt(a * a + b * b); }
|}
    name name

let core_h name =
  Printf.sprintf
    {|#ifndef %s_H
#define %s_H

double %s_hypot(double a, double b);

#endif
|}
    (String.uppercase_ascii name) (String.uppercase_ascii name) name

let main_c name =
  Printf.sprintf
    {|#include "%s.h"
#include "%s_config.h"
#include <stdio.h>

int main(void) {
    printf("%%s\n", %s_NAME);
    printf("hypot(3,4) = %%.1f\n", %s_hypot(3.0, 4.0));
    return 0;
}
|}
    name name (String.uppercase_ascii name) name

let test_c name =
  Printf.sprintf
    {|#include "%s.h"
#include <math.h>
#include <stdio.h>

int main(void) {
    if (fabs(%s_hypot(3.0, 4.0) - 5.0) > 1e-9) {
        fprintf(stderr, "hypot is wrong\n");
        return 1;
    }
    printf("ok\n");
    return 0;
}
|}
    name name

let create name =
  let w path text = if Sys.file_exists path then Diag.error "%s already exists" path else Fs.write path text in
  w "build.meow" (build_meow name);
  w (Printf.sprintf "include/%s.h" name) (core_h name);
  w (Printf.sprintf "src/core/%s.c" name) (core_c name);
  w "src/main.c" (main_c name);
  w (Printf.sprintf "tests/%s_test.c" name) (test_c name);
  w ".gitignore" "build/\n";
  [ "build.meow"; "include/" ^ name ^ ".h"; "src/core/" ^ name ^ ".c"; "src/main.c";
    "tests/" ^ name ^ "_test.c"; ".gitignore" ]
