#!/usr/bin/env hy
(import requests)

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
  (print markdown-files))

(get-post-files)
