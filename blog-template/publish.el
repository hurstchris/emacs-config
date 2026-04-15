;; To publish
;; 1) M-x eval-buffer
;; 2) (org-publish PROJECTNAME t)


(setq org-html-head
      "<link rel=\"stylesheet\" type=\"text/css\" href=\"/home/chris/org/test-blog/org/static/style.css\"/>")

(setq org-html-preamble t)

(setq org-html-preamble-format
      '(("en"
         "<nav class=\"navbar\">
<a href=\"index.html\">Home</a> |
<a href=\"about.html\">About</a> |
<a href=\"posts-sitemap.html\">Blog</a> |
<a href=\"cpp-notes.html\">C++ Notes</a> |
<a href=\"daily-notes.html\">Daily Notes</a>
</nav>")))

(setq org-html-postamble nil)

(setq org-publish-project-alist
      '(	
        ("blog-posts"
         :base-directory "~/org/test-blog/org/posts"
         :publishing-directory "~/org/test-blog/public"
         :recursive t
         :publishing-function org-html-publish-to-html
         :with-author t
         :with-toc t
         :section-numbers nil
         :time-stamp-file t
	 :auto-sitemap t
	 :sitemap-title "Posts"
	 :sitemap-sort-files anti-chronologically
	 :sitemap-filename "posts-sitemap.org"
	 )

	("blog-daily-notes"
         :base-directory "~/org/test-blog/org/daily-notes"
         :publishing-directory "~/org/test-blog/public"
         :recursive t
         :publishing-function org-html-publish-to-html
         :with-author t
         :with-toc t
         :section-numbers nil
         :time-stamp-file t
	 )

	("blog-cpp-notes"
         :base-directory "~/org/test-blog/org/cpp-notes"
         :publishing-directory "~/org/test-blog/public"
         :recursive t
         :publishing-function org-html-publish-to-html
         :with-author t
         :with-toc t
         :section-numbers nil
         :time-stamp-file t
	 )

        ("blog-pages"
         :base-directory "~/org/test-blog/org/pages"
         :publishing-directory "~/org/test-blog/public"
         :publishing-function org-html-publish-to-html
	 )

        ("blog-static"
         :base-directory "~/org/test-blog/org/static"
         :publishing-directory "~/org/test-blog/public"
         :recursive t
         :publishing-function org-html-publish-to-html)

        ("test-blog"
         :components ("blog-pages" "blog-static" "blog-posts" "blog-daily-notes" "blog-cpp-notes"))))
