(define-module (hole i18n)
  #:use-module (ice-9 string-fun)
  #:export (
            blog-language
            t_
            ))

(define blog-language (make-parameter "zh-CN"))

(define %lang-data
  '(("zh-CN" . ((archives . "归档")
                (tags . "标签")
                (search . "搜索")
                (about . "关于")
                (collections-titles . ("最近文章" "没那么近文章" "有点老文章" "尘封文章" "古旧文章" "可以说是黑历史的文章"))))
    ("en" . ((archives . "Archives")))))

(define (get-text key lang)
  (let ((lang-msgs (assoc-ref %lang-data lang)))
    (or (assoc-ref lang-msgs key)
        (begin
          (format #t "missing i18n key ~s in ~s ! ~%" (symbol->string key) lang)
          (symbol->string key)))))

(define (t_ key)
  (get-text key (blog-language)))
