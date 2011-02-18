(ns musicbrainz
  (:require musicbrainz.artists)
  (:require [clojure.contrib.string :as s])
  (:require [clj-http.client :as c]))

(def base-url "http://musicbrainz.org/ws/1/")
(defn get-xml [url]
  (let [response (c/get url)
	reader (java.io.StringReader. (response :body))
	is (org.xml.sax.InputSource. reader)]
    (. is setEncoding "UTF-8")
    (clojure.xml/parse is)))

(defn xml2map [xml]
  (if (or (nil? xml) (string? xml))
    xml
    (cond (or (= (xml :tag) :metadata))
	  (recur (first (xml :content)))
	  (or (= (xml :tag) :release)
	      (= (xml :tag) :artist)
	      (= (xml :tag) :event)
	      (= (xml :tag) :label))
	  (let [a (xml :attrs)
		a (if (contains? a :ext:score)
		    (assoc a :ext:score (Integer/parseInt (a :ext:score)))
		    a)
		contents (map xml2map (xml :content))]
	    (apply merge a contents))
	  (or (= (xml :tag) :title)
	      (= (xml :tag) :asin)
	      (= (xml :tag) :name)
	      (= (xml :tag) :disambiguation)
	      (= (xml :tag) :sort-name))
	  {(xml :tag) (apply str (xml :content))}
	  (or (= (xml :tag) :text-representation)
	      (= (xml :tag) :disc-list)
	      (= (xml :tag) :life-span)
	      (= (xml :tag) :track-list))
	  {(xml :tag) (xml :attrs)}
	  (or (= (xml :tag) :release-event-list)
	      (= (xml :tag) :artist-list)
	      (= (xml :tag) :release-list))
	  {(xml :tag) (map xml2map (xml :content))}
	  :else
	  (println "UNKNOWN TAG" (xml :tag) xml)
	  )))

(defn search [resource params]
  (let [url (str base-url (name resource) "/?type=xml&" (s/join "&" (for [[k v] params] (str (name k) "=" (s/replace-str " " "+" (str v))))))]
    (. System/out (println url))
    (let [r (loop [try 0]
	      (if (> try 4)
		{}
		(let [r (try
			 (get-xml url)
			 (catch Exception e
			   {:error e}))]
		  (if (contains? r :error)
		    (do
		      (. System/out (println "error: retrying in 1 second"))
		      (. Thread (sleep 1000))
		      (recur (inc try)))
		    r))))
	  cnt (Integer/parseInt (get-in (first (r :content)) [:attrs :count] "0"))
	  offset (get params :offset 0)
	  limit (get params :limit 25)
	  m (xml2map r)]
      (merge-with concat
		  (or m {})
		  (if (and (> cnt (+ offset limit))
			   (= 100 (get (last (val (first m))) :ext:score 0)))
			   ; if there are more search results, get them too, 
                           ; but only if they potentially have a score of 100
		    (search resource (assoc params
				       :offset (+ offset limit)))
		    {})))))

(defn id3-to-map [id3]
  {:artist (.getArtist id3)
   :album (.getAlbum id3)
   :title (.getTitle id3)})

(defn mp3scan [directory]
  (remove nil?
	  (map
	   (fn [f]
	     (if (and (.isFile f)
		      (< (.length f) (* 1048576 100))) ; assuming no mp3s are over 100mbs (which avoids oom exceptions)
	       (try
		(let [f (.getCanonicalPath f)
		      m (com.mpatric.mp3agic.Mp3File. f)]
		  (cond (.hasId3v2Tag m)
			(id3-to-map (.getId3v2Tag m))
			(.hasId3v1Tag m)
			(id3-to-map (.getId3v1Tag m))
			:else
			nil))
		(catch com.mpatric.mp3agic.InvalidDataException e
		  nil)
		(catch java.io.IOException e
		  (if (re-find #"Negative seek offset" (.getMessage e))
		    nil
		    (throw e))))
	       nil))
	   (file-seq (java.io.File. directory)))))

(defn date-keyfn [i]
  (get (first (sort-by :date (i :release-event-list))) :date ""))

(defn run [dir]
  (reverse
   (sort-by
    date-keyfn
    (distinct ; to make sure we don't have the same result twice
     (apply
      concat
      (map (fn [artist]
	     (filter #(= (% :ext:score) 100)
		     ((search :release {:artist artist}) :release-list)))
	   (remove ; TODO: are there any other equivatents of this?
	    #(= "various artists" %)
	    (distinct
	     (map 
	      #(.toLowerCase %)
	  ;(map :artist (mp3scan dir))))))))
	  ; TODO: filter out invalid artist names
	  ; using hack until this is fixed
	      musicbrainz.artists/artists)))))))))

(defn scan-mp3s-for-artist [dir]
  (spit
   "artists.clj"
   (set (map :artist (mp3scan dir)))))

(defn full-report [dir]
  (spit 
   "report.html"
   (str 
    "<html><body><table>\n"
    "<tr><th>Artist</th><th>Album</th><th>Type</th><th>Released</th></tr>\n"
    (apply 
     str
     (for [r (filter #(re-find #"^(Album|EP) Official$" (% :type)) (run dir))]
       (str "<tr><td>" (r :name) "</td><td>" (r :title) "</td><td>" (r :type)
	    "</td><td>" (date-keyfn r) "</td></tr>\n")))
    "</table></body></html>\n")))