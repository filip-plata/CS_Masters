#ifndef DEBUG_H
#define DEBUG_H

#undef PDEBUG             /* undef it, just in case */
#ifdef HARDDOOM_DEBUG
#  ifdef __KERNEL__
     /* This one if debugging is on, and kernel space */
#    define PDEBUG(fmt, ...) printk( KERN_INFO "%s:%d:%s():harddoom2: " fmt, \
         __FILE__, __LINE__, __func__, ##__VA_ARGS__)
#  else
     /* This one for user space */
#    define PDEBUG(fmt, ...) fprintf(stderr, fmt, ##__VA_ARGS__)
#  endif
#else
#  define PDEBUG(fmt, args...) /* not debugging: nothing */
#endif

#undef PDEBUGG
#define PDEBUGG(fmt, args...) /* nothing: it's a placeholder */

#endif /* DEBUG_H */
