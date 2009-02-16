/*
 * This is the `tmp/tmp.c' file of GNU eev.
 * This file is in the Public Domain.
 * Author and version: Eduardo Ochs, 2005jan07
 * `eegcc' prepends this to C code given on stdin and compiles the result.
 * (find-eevrc ".zshrc" "eegcc")
 * (find-node "(cpp)Stringification")
 * (find-node "(cpp)Computed Includes")
 * (find-node "(gcc)Preprocessor Options" "`-D NAME=DEFINITION'")
 * (find-node "(cpp)Diagnostics")
 */
#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
