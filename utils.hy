#!/usr/bin/env hy
(import requests)

(import argparse)
(import sys)
(import os)
(import json)
(import getopt)
(import re)
(import random)
(import subprocess)
(import itertools)
(import datetime [datetime timezone])

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

(defn fnv-1a [s]
  (setv h 0xcbf29ce484222325)
  (for [b (.encode s "utf-8")]
    (setv h (^ h b))
    (setv h (& (* h 0x100000001b3) 0xffffffffffffffff)))
  (format h "x"))

(defn post-public-id [domain slug]
  (fnv-1a f"{domain}+{slug}"))

(defn path->slug [path]
  (let [rel-path (.replace path "posts/" "")
        no-ext (re.sub r"\.org$" "/" rel-path)]
    (if (.startswith no-ext "/")
        no-ext
        (+ "/" no-ext))))

(defn parse-org-file [path]
  (setv result {:title None
                :path path
                :date None
                :feed-only False
                :draft False
                :slug None})
  (with [f (open path "r" :encoding "utf-8")]
    (setv lines (.readlines f))
    (setv in-front-matter True)
    (setv content-lines [])
    (for [line lines]
      (setv stripped (.strip line))
      (when in-front-matter
        (cond
          (.startswith stripped "#+title:")
          (setv (get result :title) (.strip (.replace stripped "#+title:" "" 1)))
          (.startswith stripped "#+TITLE:")
          (setv (get result :title) (.strip (.replace stripped "#+TITLE:" "" 1)))
          (.startswith stripped "#+date:")
          (setv (get result :date) (.strip (.replace stripped "#+date:" "" 1)))
          (.startswith stripped "#+DATE:")
          (setv (get result :date) (.strip (.replace stripped "#+DATE:" "" 1)))
          (.startswith stripped "#+slug:")
          (setv (get result :slug) (.strip (.replace stripped "#+slug:" "" 1)))
          (.startswith stripped "#+SLUG:")
          (setv (get result :slug) (.strip (.replace stripped "#+SLUG:" "" 1)))
          (.startswith stripped "#+feed_only:")
          (setv (get result :feed-only)
                (in "t" (.lower (.strip (.replace stripped "#+feed_only:" "" 1)))))
          (.startswith stripped "#+FEED_ONLY:")
          (setv (get result :feed-only)
                (in "t" (.lower (.strip (.replace stripped "#+FEED_ONLY:" "" 1)))))
          (.startswith stripped "#+draft:")
          (setv (get result :draft)
                (in "t" (.lower (.strip (.replace stripped "#+draft:" "" 1)))))
          (.startswith stripped "#+DRAFT:")
          (setv (get result :draft)
                (in "t" (.lower (.strip (.replace stripped "#+DRAFT:" "" 1)))))
          (.startswith stripped "#+")
          None
          True
          (do
            (setv in-front-matter False)
            (.append content-lines line))))
      (when (not in-front-matter)
        (.append content-lines line))))
  result)

(defn get-org-files []
  (setv files [])
  (for [[root dirs filenames] (os.walk "posts")]
    (for [name filenames]
      (when (.endswith name ".org")
        (.append files (os.path.join root name)))))
  files)

(defn get-latest-org-post []
  (setv org-files (get-org-files))
  (when (= (len org-files) 0)
    (return None))
  (setv parsed
        (lfor f org-files
              :setv post (parse-org-file f)
              :if (and (not (get post :feed-only))
                       (not (get post :draft)))
              post))
  (when (= (len parsed) 0)
    (return None))
  (setv sorted-posts
        (sorted parsed :key (fn [x] (or (get x :date) "")) :reverse True))
  (get sorted-posts 0))

(defn link-latest-post []
  (let [post (get-latest-org-post)
        domain "blog.southfox.me"
        gateway "https://southfox.gay"
        token (get os.environ "BLOG_AP_TOKEN")]
    (when (not post)
      (print "No org post found, skipping AP link")
      (return))
    (when (get post :feed-only)
      (print "Latest post is feed-only, skipping AP link")
      (return))
    (when (not token)
      (raise (ValueError "BLOG_AP_TOKEN not set")))
    (let [slug (or (get post :slug ) (path->slug (get post :path)))
          rid (post-public-id domain slug)
          url f"https://blog.southfox.me{slug}"
          headers {"Authorization" f"Bearer {token}"}
          payload {"domain" domain
                   "slug" slug
                   "title" (get post :title)
                   "content" ""
                   "url" url}
          response (requests.post
                     f"{gateway}/api/site/resource/{rid}"
                     :headers headers
                     :json payload)]
      (cond
        (= response.status_code 201)
        (print f"Published AP resource {rid}: {(get post :title)}")

        (= response.status_code 409)
        (print f"AP resource {rid} already exists, skipped")

        True
        (do
          (print f"Failed to publish AP resource {rid}: {response.status_code}")
          (print response.text)
          (response.raise_for_status))))))

(setv parser (argparse.ArgumentParser))
(setv subparsers (parser.add_subparsers :dest "command"))
(parser.add_argument "-p" :dest "pi" :action "store_true")
(parser.add-argument "-d" :dest "deploy_type"
                     :choices ["ci" "pi"]
                     :help "Deployment type: ci or pi")
(parser.add_argument "-b" :dest "backup" :action "store_true")

(setv new-parser (subparsers.add_parser "new"))
(new-parser.add_argument "file_id" :help "The ID (filename) of the new post")

(setv args (parser.parse_args))

(defn run-cmd [cmd [check True] [print? True]]
  (when print? (print "Run cmd: " cmd))
  (subprocess.run cmd :check check))

(let [post-files (get-post-files)]
  (when args.deploy_type
    (subset-font-file post-files)
    (run-cmd ["pyftsubset" "Zpix.ttf" "--text-file=strdb.txt"])
    (run-cmd ["fonttools" "ttLib.woff2" "compress" "-o" "Zpix.woff2" "Zpix.subset.ttf"])
    (run-cmd ["mv" "Zpix.woff2" "assets/fonts/Zpix.woff2"])
    (run-cmd ["mv" "Zpix.subset.ttf" "assets/fonts/Zpix.ttf"])
    (run-cmd ["haunt" "build"])
    (run-cmd ["pagefind_extended" "--site" "site"])
    (cond (= args.deploy_type "pi")
          (do
            (run-cmd ["rm" "-rf" "/var/www/blog/"])
            (run-cmd ["mv" "site/" "/var/www/blog/"]))
          (= args.deploy_type "ci")
          (do
            (os.chdir "site")
            (let [BLOG_DEPLOY_TOKEN (get os.environ "BLOG_DEPLOY_TOKEN")
                  xxx (get os.environ "BLOG_AP_TOKEN")]
              (run-cmd ["git" "init"])
              (run-cmd ["git" "config" "--global" "user.name" "SouthFox"])
              (run-cmd ["git" "config" "--global" "user.email" "master@southfox.me"])
              (run-cmd ["git" "add" "--all" "."])
              (run-cmd ["git" "commit" "-m" "Fox CI deploy"])
              (run-cmd ["git" "branch" "-M" "master"])
              (run-cmd ["git" "push" "--quiet" "--force"
                        f"https://{BLOG_DEPLOY_TOKEN}@github.com/SouthFox-D/SouthFox-D.github.io.git"
                        "master"]
                       :print? False))
            (os.chdir "..")
            (link-latest-post))))
  (when args.backup
    (os.makedirs "newimg" :exist_ok True)
    (backup-ipfs-img post-files)))
