#include <stdio.h>
#include <getopt.h>
#include <stdlib.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <fcntl.h>
#include <unistd.h>
#include <strings.h>

#define BUFFER_SIZE 0x50000

static int length = -1;
static int start = 0;
static int frames = 79;

int max (int a, int b) {
  return (a > b? a: b);
}

int min (int a, int b) {
  return (a < b? a: b);
}


void summarize (char *file) {
  short *buf;
  struct stat statbuf;
  int size = 0;
  int end = 0;
  int frame_size = 0;
  int in, nframe = 0, pos, read_len, buffer_pos, frame_pos, file_pos;
  double sample;
  
  if (! (buf = (short *) malloc (BUFFER_SIZE * sizeof(short)))) {
    perror ("summarize");
    exit (-1);
  }
  
  if (! (in = open (file, O_RDONLY))) {
    perror ("summarize");
    exit (-1);
  }

  if (fstat (in, &statbuf) < 0) {
    perror ("summarize");
    exit (-1);
  }

  size = statbuf.st_size;
  size /= sizeof(short);
  if (length == -1)
    length = size;
  
  if (start + length > size)
    end = size;
  else
    end = start + length;

  if (start != 0)
    lseek(in, start*sizeof(short), SEEK_SET);

  file_pos = start;
  frame_size = length / frames;

  printf ("(((length %d) (start %d) (end %d) (frames %d) (frame-size %d))\n", 
	  length, start, end, frames, frame_size);

  while (nframe < frames) {
    pos = 0;
    read_len = 0;
    buffer_pos = 0;
    frame_pos = 0;
    sample = 0;
    while (pos < frame_size) {
      if (buffer_pos * sizeof(short) >= read_len) {
	read_len = read (in, buf, min(BUFFER_SIZE, frame_size - frame_pos)
			 * sizeof(short));
	buffer_pos = 0;
      }
      sample += abs(buf[buffer_pos++]);
      pos++;
      frame_pos++;
      file_pos++;
    }
    printf("((position %d) (value %d))\n", file_pos, (int)(sample/frame_size));
    nframe++;
  }
  printf(")\n");
}

int main (int argc, char **argv) {
  int c;
  char *file;
  
  while (1) {
    int option_index = 0;
    static struct option long_options[] = {
      {"help", 1, 0, 'h'},
      {"length", 1, 0, 'l'},
      {"start", 1, 0, 's'},
      {0, 0, 0, 0}
    };

    c = getopt_long (argc, argv, "hl:s:", long_options, &option_index);
    if (c == -1)
      break;

    switch (c) {
    case 'h':
      printf ("\
Usage: summarize [--help] [--length <length>] [--start <start>] <file>\n");
      break;

    case 's':
      start = atoi (optarg);
      break;

    case 'l':
      length = atof (optarg);
      break;

    }
  }

  if (! argv[optind]) {
    printf ("Usage: summarize <file>\n");
    exit (-1);
  } else {
    file = argv[optind];
  }

  summarize (file);
  
  return (0);
}
  
