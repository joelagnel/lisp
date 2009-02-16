#include <stdio.h>
#include <getopt.h>
#include <stdlib.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <fcntl.h>
#include <unistd.h>
#include <strings.h>
#include <string.h>

#define BUFFER_SIZE 0x5000

static char *name = "split-%02d.raw";

int max (int a, int b) {
  return (a > b? a: b);
}

int min (int a, int b) {
  return (a < b? a: b);
}

void sp_write (int fd, void *buf, int count) {
  int l, start = 0, block;

  while (count > 0) {
    block = BUFFER_SIZE;
    if (block > count) 
      block = count;
    if ((l = write (fd, (char*)(buf + start), block)) < 0) {
      perror ("bsplit write");
      printf ("%x %x %x\n", fd, (unsigned int)buf, count);
      exit (-1);
    }
    start += l;
    count -= l;
  }
}						   


void bsplit (char *file, char *split_spec) {
  char *ibuf, *obuf;
  struct stat statbuf;
  int size = 0;
  signed int in = 0, out = 0;
  int pos = 0, file_part = 1, opos = 0, i = 0, ipos = 0, read_len = 0;
  char ofilename[1024];
  int positions[1024];
  char *split;
  
  if (! (ibuf = (char *) malloc (BUFFER_SIZE))) {
    perror ("bsplit");
    exit (-1);
  }
      
  if (! (obuf = (char *) malloc (BUFFER_SIZE))) {
    perror ("bsplit");
    exit (-1);
  }
  
  if (! (in = open (file, O_RDONLY))) {
    perror ("bsplit");
    exit (-1);
  }

  if (fstat (in, &statbuf) < 0) {
    perror ("bsplit");
    exit (-1);
  }

  size = statbuf.st_size;

  positions[i++] = 0;
  split = (char*)strtok(split_spec, ":");
  positions[i++] = atoi(split);
  while ((split = strtok(NULL, ":")) != NULL) 
    positions[i++] = atoi(split);

  i = 0;
  
  while (pos < size) {
    if (pos == positions[i]) {
      if (out) {
	sp_write(out, obuf, opos);
	close(out);
      }
      sprintf(ofilename, name, file_part++);
      if ((out = open(ofilename, O_CREAT|O_WRONLY|O_TRUNC, 0644)) < 0) {
	perror("bsplit");
	exit;
      }
      i++;
      opos = 0;
    }

    if (ipos == read_len) {
      read_len = read (in, ibuf, min(BUFFER_SIZE, size - pos));
      ipos = 0;
    }

    if (opos == BUFFER_SIZE) {
      sp_write(out, obuf, BUFFER_SIZE);
      opos = 0;
    }
    
    obuf[opos++] = ibuf[ipos++];
    pos++;
  }

  if (out) {
    sp_write(out, obuf, opos);
    close(out);
  }
    
}

      
int main (int argc, char **argv) {
  int c;
  char *file, *split_spec;
  
  while (1) {
    int option_index = 0;
    static struct option long_options[] = {
      {"help", 1, 0, 'h'},
      {0, 0, 0, 0}
    };

    c = getopt_long (argc, argv, "hn:", long_options, &option_index);
    if (c == -1)
      break;

    switch (c) {
    case 'h':
      printf ("\
Usage: bsplit [--name <format>] <file> <split-spec>\n");
      break;

    case 'n':
      name = (char*) malloc(strlen(optarg) + 1);
      strcpy(name, optarg);
      break;

    }
  }

  if (! argv[optind]) {
    printf ("Usage: bsplit <file>\n");
    exit (-1);
  } else {
    file = argv[optind++];
    if (! argv[optind]) {
      printf ("Usage: bsplit <file>\n");
      exit (-1);
    } else {
      split_spec = (char*) malloc(strlen(argv[optind]) + 1);
      strcpy(split_spec, argv[optind]);
    }
  }

  bsplit (file, split_spec);
  
  return (0);
}
  
