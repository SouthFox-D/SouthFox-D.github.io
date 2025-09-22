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

  (for [[root dirs files] (os.walk "posts")
        -file files]
    (let [file (os.path.join root -file)]
      (when (or (file.endswith ".md")
                (file.endswith ".org"))
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
        query-response (requests.post url :headers header :json query-data)]
    (query-response.raise_for_status)
    (let [current-year (str (. (datetime.now) year))
          discussion-post (-> (query-response.json) :data :repository :discussions :nodes)
          discussion-post-titles (lfor x discussion-post (:title x))
          local-post-titles (lfor-> it post-files
                             (. (it.split "posts/") [-1])
                             (it.strip ".md")
                             (it.strip ".org")
                             (+ it "/"))]
      (for [post-title local-post-titles]
        (when (not-in current-year post-title)
          (continue))
        (when (not-in post-title discussion-post-titles)
          (let [create-data {"query" f"mutation{{createDiscussion(input: {{repositoryId: \"{repo-id}\", categoryId: \"{category-id}\", body: \"{(+ "https://blog.southfox.me/" post-title)}\", title: \"{post-title}\" }}) {{discussion {{id}}}}}}"}
                create-response (requests.post url :headers header :json create-data)]
            (create-response.raise_for_status)
            (print f"Create post {post-title} discussion!")))))))

(defn download-ipfs-img [img-list]
  (let [ipfs-gateway ["https://ipfs.io/ipfs/" "https://dweb.link/ipfs/"]]
    (for [img-name img-list]
      (when (= "" img-name)
        (continue))
      (setv img-url (+ (random.choice ipfs-gateway) img-name))
      (setv r (requests.get img-url))
      (r.raise_for_status)
      (with [f (open (+ "./newimg/" img-name) "wb")]
        (f.write (. r content))))))

(defn backup-ipfs-img [post-files]
  (if (os.path.exists "imgList.json")
      (setv img-json (with [f (open "imgList.json" "r" :encoding "utf-8")]
                       (json.load f)))
      (setv img-json {"img" []}))
  (setv now-img-list [])

  (for [post-file post-files]
    (with [f (open post-file "r" :encoding "utf-8")]
      (let [post-text (f.read)
            img-result (re.findall r"!\[(.*?)\]\((.*?)\)" post-text)]
        (for [[_ img-url] img-result]
          (when (= "" img-url)
            (continue))
          (when (in "ipfs" img-url)
            (now-img-list.append (. (img_url.split "/") [-1])))))))
  (setv download-need-img (list (. (set now-img-list) (difference (set (:img img-json))))))
  (with [f (open "newimg/imgList.json" "w" :encoding "utf-8")]
    (json.dump {"img" (sorted now-img-list)} f))
  (download-ipfs-img download-need-img))

(setv parser (argparse.ArgumentParser))
(parser.add_argument "-d" :dest "deploy" :action "store_true")
(parser.add_argument "-b" :dest "backup" :action "store_true")
(setv args (parser.parse_args))

(let [post-files (get-post-files)]
  (when args.deploy
    (subset-font-file post-files)
    (add-github-discussion post-files))
  (when args.backup
    (os.makedirs "newimg" :exist_ok True)
    (backup-ipfs-img post-files)))
