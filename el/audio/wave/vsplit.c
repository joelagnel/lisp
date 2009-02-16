#include <stdio.h>
#include <getopt.h>
#include <stdlib.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <fcntl.h>
#include <unistd.h>
#include <strings.h>

#define BUFFER_SIZE 0x50000

int limit = 8;
char *output = "output";
int avglen = 44100;
int verbose = 0;
int spread = 2;

int max (int a, int b) {
  return (a > b? a: b);
}

int min (int a, int b) {
  return (a < b? a: b);
}

void sprint (char *type, int count, int part) {
  int c = count / (44100*2*2);
  fprintf (stderr, "%02d:%02d %02d %s\n", c / 60, c % 60, part, type);
}

void sp_write (int fd, void *buf, int count) {
  int l, start = 0, block;

  while (count > 0) {
    block = BUFFER_SIZE;
    if (block > count) 
      block = count;
    if ((l = write (fd, (char*)(buf + start), block)) < 0) {
      perror ("vsplit write");
      printf ("%x %x %x\n", fd, (unsigned int)buf, count);
      exit (-1);
    }
    start += l;
    count -= l;
  }
}						   

void split (char *file) {
  short *buf, *avgbuf;
  int avgb = 1;
  int in, len, in_song = 0, b;
  int highest = 0;
  int high_val = 0;
  unsigned int avg = 0;
  int count = 0, close_pos = 0, end_pos = 0, split_pos = 0, start_pos = 0;
  int gracelen = 44100 * 2 * 2 * 1.5;
  int seq = -1, below = 0;
  int stop[1000], start[1000];
  int part = 1;
  char *sbuf = (char*)malloc(1000);
  int block_start = 0, out = 0, write_start = 0;
  unsigned int limit_minus_spread = (limit - spread) * avglen,
    limit_plus_spread = (limit + spread) * avglen,
    proper_limit = limit * avglen;
  
  if (! (buf = (short *) malloc (BUFFER_SIZE * sizeof(short)))) {
    perror ("vsplit");
    exit (-1);
  }
  avgbuf = (short *) malloc (avglen * sizeof(short));
  bzero (avgbuf, avglen * sizeof(short));
  
  fprintf (stderr, "Opening file %s...\n", file);
  fprintf (stderr,
	   "Avg buf: %d; limit: %d; spread: %d\n", avglen, limit, spread);
  
  if (! (in = open (file, O_RDONLY))) {
    perror ("vsplit");
    exit (-1);
  }

read_more:
  while ((len = read (in, buf, BUFFER_SIZE * sizeof(short))) > 0) {
    b = 0;
    len /= 2;
    block_start = count;
    while (b < len) {
      if (abs(buf[b]) > high_val)
	high_val = abs(buf[b]);
      if (buf[b] == 32767) {
	highest++;
      }
      avg -= abs(avgbuf[avgb+1]-avgbuf[avgb]);
      avgbuf[avgb++] = buf[b];
      avg += abs(avgbuf[avgb-1]-avgbuf[avgb-2]);
      
      if (avgb >= avglen)
	avgb = 1;

      /*
      if (! (count % (44100*2*2))) {
	int c = count / (44100*2*2);
	fprintf (stderr, "%02d:%02d %.2f\n", c / 60, c % 60,
		 (float)avg / avglen);
      }
      */

      count += 4;

      if (in_song) {
	if (avg < limit_minus_spread) {
	  if (below >= 44100 / 2) {
	    in_song = 0;
	    close_pos = 0;
	  } else {
	    if (! end_pos) {
	      below = 0;
	      sprint ("End", count, part-1);
	      end_pos = count;
	    }
	    below++;
	  }
	} else {
	  end_pos = 0;
	  below = 0;
	}
      } else {
	if (end_pos && (! close_pos) &&
	    (count - gracelen > end_pos ||
	     avg > proper_limit)) {
	  sprint ("Close", count, part-1);
	  close_pos = count;
	}
	if (avg > limit_plus_spread) {
	  in_song = 1;
	  below = 0;
	  start_pos = count;
	  sprint ("Start", count, part);
	  /* Compute split_pos based on start_pos, end_pos and close_pos. */
	  if (close_pos) {
	    /* We have found the closing point. */
	    if (close_pos < start_pos - gracelen) {
	      split_pos = start_pos - gracelen;
	      stop[seq] = close_pos;
	    } else {
	      split_pos = start_pos - (start_pos - end_pos) / 2;
	      stop[seq] = split_pos;
	    }
	  } else if (end_pos) {
	    split_pos = start_pos - (start_pos - end_pos) / 2;
	    stop[seq] = split_pos;
	  } else {
	    /* First track. */
	    split_pos = (start_pos -
			 (gracelen > 0)? start_pos - gracelen: 0);
	  }

	  if (out) {
	    if (stop[seq] < block_start) {
	      ftruncate (out, stop[seq] - write_start);
	    } else {
	      sp_write (out, (char*)((unsigned int)buf +
				     max (write_start - block_start, 0)),
			(stop[seq] - max (block_start, write_start)));
	    }
	    close (out);
	    out = 0;
	  }
	  		
	  sprintf (sbuf, "%s-%02d.raw", output, part++);
	  if ((out = open (sbuf, O_RDWR | O_CREAT | O_TRUNC, 0644)) < 0) {
	    perror ("vsplit");
	    exit (-1);
	  }
	  
	  seq++;
	  start[seq] = split_pos;
	  write_start = split_pos;
	  end_pos = 0;
	  close_pos = 0;
	  if (split_pos < block_start) {
	    lseek (in, split_pos, SEEK_SET);
	    count = split_pos;
	    goto read_more;
	  }
	}
      } 
      b += 2;
    }
    if (out) {
      sp_write (out, (char*) ((unsigned int)buf
			      + max (write_start - block_start, 0)),
		(count - max (block_start, write_start)));
    }
  }
  if (len == -1) {
    perror ("vsplit");
    exit (-1);
  }

  /* Close the final track. */
  if (close_pos) {
    stop[seq] = close_pos;
  } else if (end_pos) {
    stop[seq] = (end_pos + (gracelen > count)? count: end_pos + gracelen);
  } else {
    stop[seq] = count;
  }

  if (out) {
    if (stop[seq] < block_start) {
      ftruncate (out, stop[seq] - write_start);
    } else if (in_song) {
      sp_write (out, (char*) ((unsigned int) buf +
			      max (write_start - block_start, 0)),
		(stop[seq] - max (block_start, write_start)));
    }
    close (out);
    out = 0;
  }

  printf ("We had %d highest (%d).\n", highest, high_val);
  
}

int main (int argc, char **argv) {
  int c;
  char *file;
  
  while (1) {
    int option_index = 0;
    static struct option long_options[] = {
      {"help", 1, 0, 'h'},
      {"floor", 1, 0, 'f'},
      {"length", 1, 0, 'l'},
      {"output", 1, 0, 'o'},
      {"verbose", 1, 0, 'v'},
      {"spread", 1, 0, 's'},
      {0, 0, 0, 0}
    };

    c = getopt_long (argc, argv, "hvl:o:f:s:c:", long_options, &option_index);
    if (c == -1)
      break;

    switch (c) {
    case 'h':
      printf ("\
Usage: vsplit [--help] [--floor <limit>] [--length <length>]
              [--output <output_file>] [--spread <number] <file>

--floor specifies the noise floor
--length says how long a pause is (in seconds)
--spread specifies how much above/below the floor a song is\n

Reasonable values are:

vsplit -f 15 -s 2 -l 0.5 -o output.raw input.raw");
      break;

    case 'f':
      limit = atoi (optarg);
      break;

    case 's':
      spread = atoi (optarg);
      break;

    case 'l':
      avglen = atof (optarg) * 44100 * 2;
      break;

    case 'o':
      output = optarg;
      break;

    }
  }

  if (! argv[optind]) {
    printf ("Usage: vsplit <file>\n");
    exit (-1);
  } else {
    file = argv[optind];
  }

  split (file);
  
  return (0);
}
  
