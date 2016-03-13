(ns om-starter.core
  (:require [goog.dom :as gdom]
            [om.next :as om :refer-macros [defui]]
            [om-starter.util :as util]
            [om.dom :as dom]))

(enable-console-print!)

(defmulti mutate om/dispatch)

(defmethod mutate 'app/update-title
  [{:keys [state]} _ {:keys [new-title]}]
  {:remote true
   :value [:app/title]
   :action (fn [] (swap! state assoc :app/title new-title))})

(defmethod mutate 'app/loading?
  [{:keys [state]} _ _]
  {:value [:loading?]
   :action (fn [] (swap! state assoc :loading? true))})

(defmulti read om/dispatch)

(defmethod read :app/title
  [{:keys [state] :as env} _ {:keys [remote?]}]
  (let [st @state]
    (if-let [v (get st :app/title)]
      {:value v :remote true}
      {:remote true})))

(defmethod read :app/query
  [{:keys [state] :as env} _ {:keys [remote?]}]
  (let [st @state]
    (if-let [v (get st :app/query)]
      {:value v :remote true}
      {:remote true})))

(defmethod read :loading?
  [{:keys [state] :as env} _ _]
  (let [st @state]
    (let [v (get st :loading? false)]
      (if v
        {:value v :remote true}
        {:value v}))))

(defmethod read :app/photos
  [{:keys [state] :as env} key _]
  (let [st @state]
    (let [v (get st key [])]
      (if v
        {:value v :remote true}
        {:value v}))))

(defui SearchField
  Object
  (render [this]
          (let [{:keys [do-search] :as c} (om/get-computed this)]
            (dom/div #js {:className "jumbotron"}
                     (dom/div #js {:className "container input-group"}
                              (dom/input #js {:type "text"
                                              :ref :query
                                              :className "form-control"
                                              :placeholder "Search..."})
                              (dom/span #js {:className "input-group-btn"}
                                        (dom/button #js {:className "btn btn-primary"
                                                         :onClick (fn [e] (let [new-title (.-value (dom/node this :query))]
                                                                            (do-search new-title)))} "Search")))))))

(def search-field (om/factory SearchField))

(defn trunc [s n]
  (subs s 0 (min (count s) n)))

(defui Photo
  static om/IQuery
  (query [this]
         [:title :image-url])
  Object
  (render [this]
          (let [{:keys [title image-url]} (om/props this)]
            (dom/div #js {:className "photo"}
                     (dom/a #js {:href "" :className "thumbnail"}
                              (dom/img #js {:src image-url})
                              (dom/div #js {:className "caption"}
                                       (dom/h4 nil title)))))))

(def photo (om/factory Photo {:keyfn :id}))

(defui Root
  static om/IQueryParams
  (params [this]
          {:q "subaru"})
  static om/IQuery
  (query [this]
         '[:app/title (:app/photos {:q ?q})])
  Object
  (render [this]
          (let [{:keys [app/title app/photos]} (om/props this)]
            (dom/div nil
                     (search-field (om/computed {} {:do-search
                                                    (fn [s] (om/set-query! this {:params {:q s}}))}))
                     (if (not (empty? photos))
                       (dom/div #js {:className "container"}
                                (dom/div nil
                                         (dom/h3 nil "Photos")
                                         (dom/div nil (str "Showing " (count photos) " photos"))
                                         (dom/div #js {:className "flex"}
                                                    (map photo photos))))
                       (dom/div nil "Enter a search word to get images"))))))

(def parser (om/parser {:read read :mutate mutate}))

(def reconciler
  (om/reconciler
   {:state (atom {})
    :normalize true
    :merge-tree (fn [a b] (merge a b))
    :parser parser
    :send (util/transit-post "/api")}))

(om/add-root! reconciler Root (gdom/getElement "app"))
