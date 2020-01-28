import random
import numpy as np
from tqdm.auto import tqdm
from tensorflow import keras
from sklearn.preprocessing import LabelBinarizer
from skimage.transform import resize

# Seed random
random.seed(13243252)

# Data max length
IMG_SIZE = (240, 320)
IMG_DIMS = (240, 320, 3)
MAX_LEN = 56

# Based on https://stanford.edu/~shervine/blog/keras-how-to-generate-data-on-the-fly
class DataGenerator(keras.utils.Sequence):
  'Generates data for Keras'
  def __init__(self, list_IDs, labels, filepaths, preload=True, shuffle=True):
    'Initialization'
    self.labels = labels
    self.filepaths = filepaths
    self.list_IDs = list_IDs
    self.shuffle = shuffle

    self.preload = preload
    self.preloaded = np.empty(len(self.filepaths), dtype=object)
    if self.preload:
      for ID in tqdm(list_IDs):
        X = self.load_file(ID)
        self.preloaded[ID] = X
  
    self.on_epoch_end()

  def __len__(self):
    'Denotes the number of batches per epoch'
    return len(self.list_IDs)

  def __getitem__(self, index):
    'Generate one batch of data'
    # Generate index
    ID = self.indexes[index]
  
    # Generate data
    X = []
    if self.preload:
      X = self.preloaded[ID]
    else:
      X = self.load_file(ID)
    y = self.labels[ID]
  
    extended_X = self.extend_clip(X)
  
    return np.array([extended_X]), np.array([y])

  def load_file(self, ID):
    xx = np.load(self.filepaths[ID])["clip"]
    XX = np.empty((len(xx), IMG_DIMS[0], IMG_DIMS[1], IMG_DIMS[2]))
    for i in range(len(xx)):
      XX[i] = resize(xx[i], IMG_SIZE).astype("float16")
    return XX
  
  def extend_clip(self, clip):
    clip_shape = clip.shape
    extended_len = MAX_LEN
    if clip_shape[0] >= extended_len:
      return clip[:extended_len]
    else:
      if clip_shape[0] != 0:
        extension = clip[clip_shape[0] - 1]
      else:
        extension = np.zeros(IMG_DIMS)
      return np.concatenate([clip, np.tile(extension, (MAX_LEN - clip_shape[0],1,1,1))])

  def on_epoch_end(self):
    'Updates indexes after each epoch'
    self.indexes = np.copy(self.list_IDs)
    if self.shuffle:
      np.random.shuffle(self.indexes)

# Load metadata
filepaths, labels = [], []
with open("meta_data.npz", "rb") as outfile:
  obj = np.load(outfile, allow_pickle=True)
  filepaths = obj["filepaths"]
  labels = obj["labels"]
labels = LabelBinarizer().fit_transform(labels)

ids = list(range(len(labels)))
del ids[359] # Dirty hack as this movie has zero length
random.shuffle(ids)
split = int(len(labels)*0.8)
train_ids = ids[:split]
test_ids = ids[split:]

train_generator = DataGenerator(train_ids, labels, filepaths, preload=True)
test_generator = DataGenerator(test_ids, labels, filepaths, preload=True)

#### Neural network part

import numpy as np
import tensorflow, os
from tensorflow.keras.models import Sequential
from tensorflow.keras.layers import Dense, Dropout, LSTM, Conv2D, MaxPooling2D, Flatten, Reshape, Lambda, TimeDistributed
from tensorflow.keras.optimizers import RMSprop
from tensorflow.keras.callbacks import ModelCheckpoint

os.environ["CUDA_VISIBLE_DEVICES"] = "3"
NUM_CLASSES = 125

preproces = Sequential()
preproces.add(Conv2D(64, kernel_size=(4, 4), strides=(1, 1), activation='relu'))
preproces.add(Conv2D(64, kernel_size=(4, 4), strides=(2, 2), activation='relu'))
preproces.add(Dropout(0.5))
preproces.add(Conv2D(64, kernel_size=(4, 4), strides=(1, 1), activation='relu'))
preproces.add(Conv2D(64, kernel_size=(4, 4), strides=(2, 2), activation='relu'))
preproces.add(Dropout(0.5))
preproces.add(Conv2D(64, kernel_size=(4, 4), strides=(1, 1), activation='relu'))
preproces.add(Conv2D(64, kernel_size=(4, 4), strides=(2, 2), activation='relu'))
preproces.add(Dropout(0.5))
preproces.add(Conv2D(64, kernel_size=(4, 4), strides=(1, 1), activation='relu'))
preproces.add(Conv2D(64, kernel_size=(4, 4), strides=(2, 2), activation='relu'))
preproces.add(Dropout(0.5))
preproces.add(Flatten())
preproces.add(Dense(256))

recurent = Sequential()
recurent.add(LSTM(512))
recurent.add(Dropout(0.25))
recurent.add(Dense(256))
recurent.add(Dropout(0.25))
recurent.add(Dense(NUM_CLASSES))

model = Sequential()
model.add(TimeDistributed(preproces, input_shape=(MAX_LEN, IMG_DIMS[0], IMG_DIMS[1], IMG_DIMS[2])))
model.add(recurent)
model.compile(loss='categorical_crossentropy', optimizer=RMSprop(learning_rate=1e-4, rho=0.9), metrics=["acc"])

filepath="/home/dg372207/VRNN/weights-improvement-{epoch:03d}-{val_acc:.6f}.hdf5"
checkpoint = ModelCheckpoint(filepath, monitor="val_acc", verbose=1)
callbacks_list = [checkpoint]

history = model.fit_generator(train_generator, initial_epoch=0, epochs=10, validation_data=test_generator, callbacks=callbacks_list)
