(ns om-starter.parser
  (:require
   [clj-http.client :as client]))

(defmulti readf (fn [_ k _] k))

(defmethod readf :app/title
  [{:keys [state] :as env} k params]
  (let [st @state]
    (if-let [[_ value] (find st k)]
      {:value (str "From server: " value)}
      {:value "not-found"})))

(defmethod readf :app/query
  [{:keys [state] :as env} k params]
  (let [st @state]
    (if-let [[_ value] (find st k)]
      {:value (str "a" value)}
      {:value "a"})))

(defn flickr-by-tags [tags]
  (let [q (clojure.string/join "," tags)
        url (str "https://api.flickr.com/services/feeds/photos_public.gne?format=json&nojsoncallback=1&tags=" q)
        req (client/get url {:as :json})
        body (:body req)]
    (:items body)))

(defn image-url [photo size]
  (let [url "http://farm%s.staticflickr.com/%s/%s_%s_%s.jpg"]
    (format url (:farm photo) (:server photo) (:id photo) (:secret photo) size)))

(defn search-flickr [q]
  (let [key (System/getenv "FLICKR_API_KEY")
        url-str "https://api.flickr.com/services/rest/?method=flickr.photos.search&api_key=%s&tags=%s&format=json&nojsoncallback=1"
        url (format url-str key q)
        req (client/get url {:as :json})
        body (:body req)
        photos (-> req :body :photos :photo)]
    (map #(conj % {:image-url (image-url % "m")}) photos)))

;; (search-flickr ["sweden"])

(defmethod readf :app/photos
  [{:keys [state]} k params]
  {:value (search-flickr (:q params))})

(defmethod readf :loading?
  [_ _ _]
  {:value false})

(defmulti mutatef (fn [_ k _] k))

(defmethod mutatef 'app/update-title
  [{:keys [state]} _ {:keys [new-title]}]
  {:value [:app/title]
   :action (fn [] (swap! state assoc :app/title (str new-title " server")))})
