import random
import numpy as np
from tqdm.auto import tqdm
from tensorflow import keras
from sklearn.preprocessing import LabelBinarizer
from skimage.transform import resize

# Seed random
random.seed(13243252)

# Data max length
IMG_SIZE = (160, 256)
IMG_DIMS = IMG_SIZE + (3,)
MAX_LEN = 86

# Based on https://stanford.edu/~shervine/blog/keras-how-to-generate-data-on-the-fly
class DataGenerator(keras.utils.Sequence):
  'Generates data for Keras'
  def __init__(self, list_IDs, labels, filepaths, cache=True, shuffle=True):
    'Initialization'
    self.labels = labels
    self.filepaths = filepaths
    self.list_IDs = list_IDs
    self.shuffle = shuffle
    self.cache = cache
    self.cached = np.empty(len(self.filepaths), dtype=object)
  
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
    if self.cached[ID] is not None:
      X = self.cached[ID]
    else:
      X = self.load_file(ID)
      if self.cache:
        self.cached[ID] = X

    y = self.labels[ID]
  
    # extended_X = self.extend_clip(X)
  
    return np.array([X]), np.array([y])

  def load_file(self, ID):
    xx = np.load(self.filepaths[ID])["clip"]
    XX = np.empty((len(xx),) + IMG_DIMS)
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

train_generator = DataGenerator(train_ids, labels, filepaths, cache=True)
test_generator = DataGenerator(test_ids, labels, filepaths, cache=True)

#### Neural network part

import numpy as np
import tensorflow, os
from tensorflow.keras.models import Sequential
from tensorflow.keras.layers import Input, Dense, Dropout, LSTM, Conv2D, Conv3D, MaxPooling2D, UpSampling2D, Flatten, Reshape, Lambda, TimeDistributed
from tensorflow.keras.optimizers import RMSprop, Adam
from tensorflow.keras.callbacks import ModelCheckpoint

os.environ["CUDA_VISIBLE_DEVICES"] = "1"
NUM_CLASSES = 125

def build_rnn():
  model = Sequential()
  model.add(Conv3D(32, (4,4,4), strides=(1, 2, 2)))
  model.add(Conv3D(32, (4,4,4), strides=(1, 2, 2)))
  model.add(Conv3D(64, (4,4,4), strides=(1, 2, 2)))
  model.add(Conv3D(64, (4,4,4), strides=(1, 2, 2)))
  model.add(TimeDistributed(Flatten()))
  # recurrent part
  model.add(LSTM(32, return_sequences=True))
  model.add(Dropout(0.35))
  model.add(LSTM(512))
  model.add(Dense(NUM_CLASSES))
  # model.build(input_shape=(None, MAX_LEN,) + IMG_DIMS)
  return model

model = build_rnn()
model.compile(loss='categorical_crossentropy', optimizer=RMSprop(), metrics=["acc"])

filepath="/home/dg372207/VRNN/recurentv3-{epoch:03d}.hdf5"
checkpoint = ModelCheckpoint(filepath, verbose=1)
callbacks_list = [checkpoint]

# Start from begining
history = model.fit_generator(train_generator, initial_epoch=0, epochs=64, validation_data=test_generator, callbacks=callbacks_list)

# Start from checkpoint
initial_epoch = 1
file="/home/dg372207/VRNN/recurentv3-{:03d}.hdf5".format(initial_epoch)
model.load_weights(file)
history = model.fit_generator(train_generator, initial_epoch=initial_epoch, epochs=64, validation_data=test_generator, callbacks=callbacks_list)
