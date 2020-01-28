#!/usr/bin/env python3

import os
import argparse
import PIL
import shutil

import numpy as np
import matplotlib.pyplot as plt
from skimage import transform
from sklearn.metrics import confusion_matrix
import itertools

from tqdm import tqdm
from glob import glob

import tensorflow as tf
from tensorflow.keras import layers
from tensorflow.keras import models
from tensorflow.keras.callbacks import ModelCheckpoint

from tensorflow.keras.preprocessing.image import ImageDataGenerator

TARGET_CLASSES = 29
TARGET_SIZE = (64, 64)
TARGET_DIMS = TARGET_SIZE + (3,)
BATCH_SIZE = 32

PATH_TO_CKPT = "frozen_inference_graph_141 14-51-46-798.pb"

imgs_train_dir = os.path.join(os.getcwd(), 'data-static', 'train')
imgs_val_dir = os.path.join(os.getcwd(), 'data-static', 'test')

imgs_train_hand_focus_dir = os.path.join(os.getcwd(), 'data-static', 'train-hand-focused')
imgs_val_hand_focus_dir = os.path.join(os.getcwd(), 'data-static', 'test-hand-focused')


def hand_detector_load():
    detection_graph = tf.Graph()
    with detection_graph.as_default():
        od_graph_def = tf.compat.v1.GraphDef()
        with tf.io.gfile.GFile(PATH_TO_CKPT, 'rb') as fid:
            serialized_graph = fid.read()
            od_graph_def.ParseFromString(serialized_graph)
            tf.import_graph_def(od_graph_def, name='')
        sess = tf.compat.v1.Session(graph=detection_graph)

    return detection_graph, sess


def extract_hand_image(detection_graph, sess, image, hand_score_thr=0.2):
    image_tensor = detection_graph.get_tensor_by_name('image_tensor:0')
    detection_boxes = detection_graph.get_tensor_by_name(
        'detection_boxes:0')
    detection_scores = detection_graph.get_tensor_by_name(
        'detection_scores:0')
    detection_classes = detection_graph.get_tensor_by_name(
        'detection_classes:0')
    num_detections = detection_graph.get_tensor_by_name(
        'num_detections:0')

    im_width, im_height = image.size
    np_image = np.array(image).astype('float32')
    image_np_expanded = np.expand_dims(np_image, axis=0)

    (boxes, scores, classes, num) = sess.run(
        [detection_boxes, detection_scores,
         detection_classes, num_detections],
        feed_dict={image_tensor: image_np_expanded})

    if len(boxes) > 0:
        good_boxes = [boxes[0][idx] for idx, score in enumerate(scores[0]) if score > hand_score_thr]

        if len(good_boxes) > 0:
            best_box = good_boxes[0]
            (left, right, top, bottom) = (best_box[1] * im_width, best_box[3] * im_width,
                                          best_box[0] * im_height, best_box[2] * im_height)
            image = image.crop((left, top, right, bottom))

    return image.resize(TARGET_SIZE, PIL.Image.LANCZOS)


def fresh_model():
    model = models.Sequential()

    model.add(layers.Conv2D(64, kernel_size=4, strides=1, activation='relu', input_shape=TARGET_DIMS))
    model.add(layers.Conv2D(64, kernel_size=4, strides=2, activation='relu'))
    model.add(layers.Dropout(0.5))
    model.add(layers.Conv2D(128, kernel_size=4, strides=1, activation='relu'))
    model.add(layers.Conv2D(128, kernel_size=4, strides=2, activation='relu'))
    model.add(layers.Dropout(0.5))
    model.add(layers.Conv2D(256, kernel_size=4, strides=1, activation='relu'))
    model.add(layers.Conv2D(256, kernel_size=4, strides=2, activation='relu'))
    model.add(layers.Flatten())
    model.add(layers.Dropout(0.5))
    model.add(layers.Dense(512, activation='relu'))
    model.add(layers.Dense(TARGET_CLASSES, activation='softmax'))

    return model


def plot_confusion_matrix(y, y_pred):
    uniq_labels = sorted(os.listdir(imgs_train_dir))
    cm = confusion_matrix(y, y_pred)
    plt.figure(figsize=(24, 20))
    ax = plt.subplot()
    plt.imshow(cm, interpolation='nearest', cmap=plt.cm.Purples)
    plt.colorbar()
    plt.title("Confusion Matrix")
    tick_marks = np.arange(len(uniq_labels))
    plt.xticks(tick_marks, uniq_labels, rotation=45)
    plt.yticks(tick_marks, uniq_labels)
    plt.ylabel('True label')
    plt.xlabel('Predicted label')
    ax.title.set_fontsize(20)
    ax.xaxis.label.set_fontsize(16)
    ax.yaxis.label.set_fontsize(16)
    limit = cm.max() / 2.
    for i, j in itertools.product(range(cm.shape[0]), range(cm.shape[1])):
        plt.text(j, i, format(cm[i, j], 'd'), horizontalalignment="center",
                 color="white" if cm[i, j] > limit else "black")
    plt.show()


def parse_args():
    parser = argparse.ArgumentParser()

    parser.add_argument("--out-model",
                        help="Path where to save the trained model")

    parser.add_argument(
        "--in-model", help="Path to file from which load model")

    parser.add_argument('--train', dest='train',
                        action='store_true', help="Perform training")
    parser.add_argument('--no-train', dest='train',
                        action='store_false', help="Do not train")
    parser.set_defaults(train=True)

    parser.add_argument('--epochs', type=int, default=5,
                        help='Number of epochs of training')
    parser.add_argument("--history-log", type=str, default="train_history.json",
                        help="File where to dump history of training")

    parser.add_argument("--predict-image", help="Path to a image on which to run predictions")
    parser.add_argument("--plot-confusion", dest="confusion",
                        action='store_true', help="Plot confusion matrix")
    parser.set_defaults(confusion=False)

    parser.add_argument("--hand-focused-image")

    parser.add_argument("--hand-preprocess", dest="hand", action="store_true")
    parser.set_defaults(hand=False)

    return parser.parse_args()


def _preprocess_hand_helper(source_dir, target_dir, detector, sess):
    for f in tqdm(glob(os.path.join(source_dir, "*", "*.jpg"))):
        class_name = os.path.basename(os.path.dirname(f))
        try:
            os.makedirs(os.path.join(target_dir, class_name))
        except FileExistsError:
            pass
        extract_hand_image(detector, sess, PIL.Image.open(f)).save(
            os.path.join(target_dir, class_name, os.path.basename(f)))


def main():
    args = parse_args()
    model = fresh_model()

    hand_detection, tf_sess = hand_detector_load()

    if args.hand:
        _preprocess_hand_helper(imgs_train_dir, imgs_train_hand_focus_dir,
                                hand_detection, tf_sess)
        _preprocess_hand_helper(imgs_val_dir, imgs_val_hand_focus_dir,
                                hand_detection, tf_sess)

    if args.hand_focused_image:
        np_image = PIL.Image.open(args.hand_focused_image)
        img = extract_hand_image(hand_detection, tf_sess, np_image)
        img.save('/tmp/test.jpg')

    datagen = ImageDataGenerator(
        samplewise_center=True,
        samplewise_std_normalization=True,
        rotation_range=15,
        width_shift_range=0.2,
        height_shift_range=0.2)

    train_generator = datagen.flow_from_directory(
        imgs_train_hand_focus_dir, target_size=TARGET_SIZE, batch_size=BATCH_SIZE, shuffle=True)
    validation_generator = datagen.flow_from_directory(
        imgs_val_hand_focus_dir, target_size=TARGET_SIZE, batch_size=BATCH_SIZE)

    if args.in_model:
        model.load_weights(args.in_model)

    if args.train:
        checkpoint = ModelCheckpoint("asl_static_hand_focus.h5", monitor='loss', verbose=1,
                                     save_best_only=True, mode='auto', period=1)
        model.compile(loss='categorical_crossentropy', optimizer='adam',
                      metrics=['accuracy'])
        history = model.fit_generator(
            train_generator, epochs=args.epochs, validation_data=validation_generator,
            callbacks=[checkpoint])

    if args.out_model:
        model.save(args.out_model)

    if args.history_log and args.train:
        with open(args.history_log, 'w') as fp:
            print(str(history.history), file=fp)

    if args.predict_image:
        labels = sorted(os.listdir(imgs_train_dir))
        np_image = PIL.Image.open(args.predict_image)
        np_image = np.array(np_image).astype('float32')
        np_image = transform.resize(np_image, TARGET_SIZE)
        # center mean and std
        np_image -= np.mean(np_image, keepdims=True)
        np_image /= (np.std(np_image, keepdims=True) + 1e-6)
        np_image = np.expand_dims(np_image, axis=0)

        print(labels[model.predict_classes(np_image)[0]])

    if args.confusion:
        gen = datagen.flow_from_directory(
            imgs_val_dir, target_size=TARGET_SIZE,
            batch_size=BATCH_SIZE, shuffle=False)
        y_test_pred = model.predict_generator(gen, verbose=0)
        y_test_pred = np.argmax(y_test_pred, axis=1)
        plot_confusion_matrix(gen.classes, y_test_pred)


if __name__ == "__main__":
    main()
