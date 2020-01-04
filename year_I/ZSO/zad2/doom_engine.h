#ifndef DOOM_ENGINE_H
#define DOOM_ENGINE_H

ssize_t doom_write(struct file *file, const char __user *bufer, size_t count, loff_t *filepos);
long doom_ioctl(struct file *file, unsigned int cmd, unsigned long arg);

#endif /* DOOM_ENGINE_H */
