(use-package elfeed
  :ensure t
  :init
  (setq elfeed-feeds
	'(("http://www.avclub.com/feed/rss/" film entertainment news) 
	  ("http://www.tor.com/rss/frontpage_full" literature)
	  ("http://feeds.feedburner.com/themillionsblog/fedw" literature)
	  ("http://www.newyorker.com/feed/books" literature)
	  ("http://feeds.feedburner.com/nybooks" literature)
	  ("http://longform.org/feed.rss")
	  ("http://feeds.feedburner.com/mcsweeneys/")

	  ("http://rss.escapistmagazine.com/news/0.xml"         entertainment videogames)
	  ("http://www.thejimquisition.com/feed/"               entertainment videogames)
	  ("http://www.engadget.com/tag/@gaming/rss.xml"        entertainment videogames)
	  ("http://feeds.feedburner.com/RockPaperShotgun"       entertainment videogames)
	  ("http://screenrant.com/feed/"                        entertainment movies)

	  ("https://news.ycombinator.com/rss"                sw news)
	  ("http://usesthis.com/feed/"                       sw)
	  ("http://endlessparentheses.com/atom.xml"          sw emacs)
	  ("http://emacshorrors.com/feed.atom"               sw emacs)
	  ("http://emacsninja.com/feed.atom"                 sw emacs)
	  ("http://feeds.feedburner.com/codinghorror"        sw)
	  ("http://syndication.thedailywtf.com/TheDailyWtf"  sw)
	  ("http://feeds.feedburner.com/thisdeveloperslife"  sw)
	  ("http://feeds.feedburner.com/oreilly/news"        sw)
	  ("http://www.joelonsoftware.com/rss.xml"           sw)
	  ("http://onethingwell.org/rss"                     sw tech)

	  ("http://syndication.thedailywtf.com/TheDailyWtf"  sw)
	  ("http://githubengineering.com/atom.xml"           sw tech)

	  ("http://engineering.riotgames.com/news/feed"      sw tech lol)
	  ("http://feeds.feedburner.com/blogspot/RLXA"       sw google tech)

	  ("http://pandodaily.com.feedsportal.com/c/35141/f/650422/index.rss"  tech)
	  ("https://medium.com/feed/backchannel"                               tech sw)
	  ("http://recode.net/feed/"                                           tech)
	  ("http://recode.net/category/reviews/feed/"                          tech)
	  ("http://feeds.feedburner.com/AndroidPolice"                         tech android)
	  ("http://bits.blogs.nytimes.com/feed/" tech)
	  ("https://blog.aaronbieber.com/feed.xml" sw)

	  ("http://cube-drone.com/rss.xml"                comic sw)
	  ("http://xkcd.com/rss.xml"                      comic)
	  ("http://comicfeeds.chrisbenard.net/view/dilbert/default"  comic)
	  ("http://feeds.feedburner.com/smbc-comics/PvLb" comic)
	  ("http://www.questionablecontent.net/QCRSS.xml" comic)
	  ("http://phdcomics.com/gradfeed.php"            comic)
	  ("http://feeds.feedburner.com/wondermark"       comic))))

(provide 'init-elfeed)
;;; init-elfeed.el ends here
