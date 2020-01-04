#include <linux/module.h>
#include <linux/kernel.h>
#include <linux/ioctl.h>
#include <linux/device.h>
#include <linux/uaccess.h>
#include <linux/fs.h>
#include <linux/cdev.h>
#include <linux/list.h>
#include <linux/slab.h>

MODULE_LICENSE("GPL");

#define HELLO_IOCTL_SET_REPEATS _IO('H', 0x00)
#define HELLO_MAX_REPEATS 0x100
#define HELLO_MAX_WRITE_LEN 32

#define is_digit(X) ('0' <= (X) && (X) <= '9')

typedef struct hello_count {
	uid_t user;
	long hello_repeats;
	struct list_head list;
} hello_count;

static DEFINE_MUTEX(hello_lock);
static LIST_HEAD(hello_list);
static const char hello_reply[] = "test_soczewka1\n";
static size_t hello_len = sizeof hello_reply - 1;
static dev_t hello_major;
static struct cdev hello_cdev;
static struct cdev hello_once_cdev;
static struct class hello_class = {
	.name = "hello",
	.owner = THIS_MODULE,
};
static struct device *hello_device;
static struct device *hello_once_device;

int set_user_reads(long new_hello_repeats) {
	int error;
	struct list_head *p;
	hello_count * h_c = NULL;
	uid_t user = get_current_user()->uid.val;

	if ((error = mutex_lock_interruptible(&hello_lock)))
		return error;

	list_for_each(p, &hello_list) {
			h_c = list_entry(p, hello_count, list);
			if (h_c->user == user) {
				h_c->hello_repeats = new_hello_repeats;
				printk(KERN_INFO "Found user %u, repeats: %ld\n",
				       user, new_hello_repeats);
				mutex_unlock(&hello_lock);
				return 0;
			}
	};

	/* User not on list */
	h_c = (hello_count *)kmalloc(sizeof(hello_count), GFP_KERNEL);
	if (unlikely(h_c == NULL))
		return -ENOMEM;
	h_c->user = user;
	h_c->hello_repeats = new_hello_repeats;
	printk("Added %u\n", user);
	list_add(&(h_c->list), &hello_list);

	mutex_unlock(&hello_lock);

	return 0;
}

long get_user_reads(void) {
	int error;
	hello_count * h_c = NULL;
	struct list_head *p;
	uid_t user = get_current_user()->uid.val;

	if ((error = mutex_lock_interruptible(&hello_lock)))
		return error;

	list_for_each(p, &hello_list) {
			h_c = list_entry(p, hello_count, list);
			if (h_c->user == user) {
				printk(KERN_INFO "Found user %u, repeats: %ld\n",
				 			 user, h_c->hello_repeats);
				mutex_unlock(&hello_lock);
				return h_c->hello_repeats;
			}
	};

	mutex_unlock(&hello_lock);
	printk(KERN_INFO "Not found user %u, repeats: %ld\n",
	 			 user, h_c->hello_repeats);
	/* Default is one */
	return 1;
}

static ssize_t hello_once_read(struct file *file, char __user *buf, size_t count, loff_t *filepos)
{
	loff_t pos = *filepos;
	if (pos >= hello_len || pos < 0)
		return 0;
	if (count > hello_len - pos)
		count = hello_len - pos;
	if (copy_to_user(buf, hello_reply + pos, count))
		return -EFAULT;
	*filepos = pos + count;
	return count;
}

static ssize_t hello_read(struct file *file, char __user *buf, size_t count, loff_t *filepos)
{
	size_t file_len = hello_len;
	loff_t pos = *filepos;
	loff_t end;
	long repeats;

	if ((repeats = get_user_reads()) < 0)
		return repeats;

	file_len *= repeats;

	if (pos >= file_len || pos < 0)
		return 0;
	if (count > file_len - pos)
		count = file_len - pos;
	end = pos + count;
	while (pos < end)
		if (put_user(hello_reply[pos++ % hello_len], buf++))
			return -EFAULT;
	*filepos = pos;
	return count;
}

static ssize_t hello_write(struct file *file, const char __user *bufer, size_t count, loff_t *filepos) {
	int error;
	size_t i = 0;
	unsigned long repeats = 0;
	char buf[HELLO_MAX_WRITE_LEN];

	if (count > HELLO_MAX_WRITE_LEN || count == 0)
		return -EINVAL;

	if ((error = copy_from_user(buf, bufer, count)))
		return error;

	for (; i < count; i++) {
		if (!is_digit(buf[i]))
			break;
		repeats *= 10;
		repeats += buf[i] - '0';
	}

	if ((i < count - 1) || (i == count - 1 && buf[i] != '\n'))
	  return -EINVAL;

	if ((error = set_user_reads(repeats)))
		return error;

	return count;
}

static long hello_ioctl(struct file *file, unsigned int cmd, unsigned long arg)
{
	if (cmd != HELLO_IOCTL_SET_REPEATS)
		return -ENOTTY;
	if (arg > HELLO_MAX_REPEATS)
		return -EINVAL;
	return set_user_reads(arg);
}

static int hello_open(struct inode *ino, struct file *filep);
static int hello_release(struct inode *ino, struct file *filep);

static struct file_operations hello_once_fops = {
	.owner = THIS_MODULE,
	.read = hello_once_read,
	.open = hello_open,
	.release = hello_release,
};

static struct file_operations hello_fops = {
	.owner = THIS_MODULE,
	.read = hello_read,
	.write = hello_write,
	.open = hello_open,
	.unlocked_ioctl = hello_ioctl,
	.compat_ioctl = hello_ioctl,
	.release = hello_release,
};

static int hello_open(struct inode *ino, struct file *filep)
{
	return 0;
}

static int hello_release(struct inode *ino, struct file *filep)
{
	return 0;
}

static int hello_init(void)
{
	int err;
	if ((err = alloc_chrdev_region(&hello_major, 0, 4, "hello")))
		goto err_alloc;
	cdev_init(&hello_cdev, &hello_fops);
	if ((err = cdev_add(&hello_cdev, hello_major, 3)))
		goto err_cdev;
	cdev_init(&hello_once_cdev, &hello_once_fops);
	if ((err = cdev_add(&hello_once_cdev, hello_major + 3, 1)))
		goto err_cdev_2;
	if ((err = class_register(&hello_class)))
		goto err_class;
	hello_device = device_create(&hello_class, 0, hello_major, 0, "hello");
	if (IS_ERR(hello_device)) {
		err = PTR_ERR(hello_device);
		goto err_device;
	}
	hello_once_device = device_create(&hello_class, 0, hello_major + 1, 0, "hello_once");
	if (IS_ERR(hello_once_device)) {
		err = PTR_ERR(hello_once_device);
		goto err_device_2;
	}

	return 0;

err_device_2:
	device_destroy(&hello_class, hello_major);
err_device:
	class_unregister(&hello_class);
err_class:
	cdev_del(&hello_once_cdev);
err_cdev_2:
	cdev_del(&hello_cdev);
err_cdev:
	unregister_chrdev_region(hello_major, 2);
err_alloc:
	return err;
}

static void hello_cleanup(void)
{
	device_destroy(&hello_class, hello_major + 1);
	device_destroy(&hello_class, hello_major);
	class_unregister(&hello_class);
	cdev_del(&hello_once_cdev);
	cdev_del(&hello_cdev);
	unregister_chrdev_region(hello_major, 4);
}

module_init(hello_init);
module_exit(hello_cleanup);
