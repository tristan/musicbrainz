(ns musicbrainz
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
;  (println xml)
  (if (string? xml)
    xml
    (cond (or (= (xml :tag) :metadata))
	  (recur (first (xml :content)))
	  (or (= (xml :tag) :release)
	      (= (xml :tag) :artist)
	      (= (xml :tag) :event)
	      (= (xml :tag) :label))
	  (let [a (xml :attrs)
		contents (map xml2map (xml :content))]
	    ;(println a " - " contents))
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
  (let [url (str base-url (name resource) "/?type=xml&" (s/join "&" (for [[k v] params] (str (name k) "=" (s/replace-str " " "+" v)))))
	r (get-xml url)
	m (xml2map r)]
    m))

(defn id3-to-map [id3]
  {:artist (.getArtist id3)
   :album (.getAlbum id3)
   :title (.getTitle id3)})

(defn list-files [^java.io.File root]
  (apply concat
	 (filter #(.isFile %) (.listFiles root))
	 (map list-files (filter #(.isDirectory %) (.listFiles root)))))

(defn mp3scan [directory]
  (remove nil?
	  (map
	   (fn [f]
	     (if (.isFile f)
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
;	   (file-seq (java.io.File. directory)))))
	   (let [fl ;(list-files (java.io.File. directory))]
		 (file-seq (java.io.File. directory))]
	     fl))))