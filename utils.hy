#!/usr/bin/env hy
(import requests)

(import argparse)
(import sys)
(import os)
(import json)
(import getopt)
(import re)
(import random)
(import datetime [datetime])


(defn get-post-files []
  (setv markdown-files [])

  (for [[root dirs files] (os.walk "source")
        -file files]
    (let [file (os.path.join root -file)]
      (when (and (in "posts" file) (file.endswith ".md"))
        (markdown-files.append file))))
  (return markdown-files))

(defn subset-font-file [post-files]
  (let [str-set #{}]
    (for [f post-files]
      (with [o (open f "r" :encoding "utf-8")]
        (for [w (o.read)]
          (str-set.add w))))

    (print f"{(len str-set)} characters in all files.")
    (print (.join "" str-set))
    (with [f (open "strdb.txt" "w")]
      (f.write (.join "" str-set)))))

(setv parser (argparse.ArgumentParser))
(parser.add_argument "-d" :dest "deploy" :action "store_true")
(setv args (parser.parse_args))

(when args.deploy
  (let [post-files (get-post-files)]
    (subset-font-file post-files)))
