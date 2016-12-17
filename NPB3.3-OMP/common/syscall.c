#define _GNU_SOURCE         /* See feature_test_macros(7) */ 
#include <unistd.h> 
#include <sys/syscall.h>   /* For SYS_xxx definitions */ 

void enable_memmove_() {
	syscall(314,NULL,0,0);
}
