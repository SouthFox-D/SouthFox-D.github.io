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

(defmacro -> [head #* args]
  (setv ret head)
  (for [node args]
    (setv ret (if (isinstance node hy.models.Expression)
                  `(~(get node 0) ~ret ~@(rest node))
                  `(~node ~ret))))
  ret)

(defmacro lfor-> [name head #* rest]
  (setv ret head)
  (for [node rest]
    (setv ret `(lfor ~name ~ret ~node)))
  ret)

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

(defn add-github-discussion [post-files]
  (let [repo-id "MDEwOlJlcG9zaXRvcnkyMjg3NDM0MjQ="
        category-id "DIC_kwDODaJZAM4CA7bf"
        url "https://api.github.com/graphql"
        header {"Authorization" f"Bearer {(os.environ.get "DISCUSSIONS_TOKEN")}"}
        query-data {"query" "query{repository(owner: \"southfox-d\", name: \"southfox-d.github.io\"){id discussions(first:100, categoryId: \"DIC_kwDODaJZAM4CA7bf\"){nodes{title}}}}"}
        query-request (requests.post url :headers header :json query-data)]
    (query-request.raise_for_status)
    (let [current-year (str (. (datetime.now) year))
          discussion-post (-> (r.json) :data :repository :discussions :nodes)
          discussion-post-titles (lfor x discussion-post (:title x))
          local-post-titles (lfor-> it post-files
                             (. (it.split "_posts/") [-1])
                             (it.strip ".md")
                             (+ it "/"))]
      (for [post-title local-post-titles]
        (when (not-in current-year post-title)
          (continue))
        (when (not-in post-title discussion-post-titles)
          (let [create-data {"query" f"mutation{{createDiscussion(input: {{repositoryId: \"{repo-id}\", categoryId: \"{category-id}\", body: \"{(+ "https://blog.southfox.me/" post-title)}\", title: \"{post-title}\" }}) {{discussion {{id}}}}}}"}
                create-request (requests.post url :headers header :json create-data)]
            (create-request.raise_for_status)
            (print f"Create post {post-title} discussion!")))))))

(setv parser (argparse.ArgumentParser))
(parser.add_argument "-d" :dest "deploy" :action "store_true")
(setv args (parser.parse_args))

(when args.deploy
  (let [post-files (get-post-files)]
    (subset-font-file post-files)
    (add-github-discussion post-files)))
