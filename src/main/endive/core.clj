(ns endive.core
  (:refer-clojure :exclude [read-string])
  (:require [clojure.edn :as edn]
            [clojure.zip :as zip]
            [clojure.set :as set]
            [clojure.string :as str]
            [clojure.java.io :as io]
            [garden.core :as garden]
            [garden.util :as util]
            [garden.color :as color]
            [garden.units :as units]
            [garden.stylesheet :as stylesheet])
  (:import (java.io StringReader)
           (org.w3c.css.sac Selector Condition InputSource)
           (com.steadystate.css.parser CSSOMParser SACParserCSS3)))

(def readers
  {'in units/in
   'cm units/cm
   'pc units/pc
   'mm units/mm
   'pt units/pt
   'px units/px
   'percent units/percent
   'deg units/deg
   'grad units/grad
   'rad units/rad
   'turn units/turn
   's units/s
   'ms units/ms
   'Hz units/Hz
   'kHz units/kHz
   'dpi units/dpi
   'dpcm units/dpcm
   'dppx units/dppx

   'rgb color/rgb
   'rgba color/rgba
   'hsl color/hsl
   'hsla color/hsla

   'at-media #(apply stylesheet/at-media %)
   'at-import #(apply stylesheet/at-import %)
   'at-font-face #(apply stylesheet/at-font-face %)
   'at-keyframes #(apply stylesheet/at-keyframes %)})

(defn read-string [string]
  (edn/read-string {:readers readers} string))

(defn read-resource [path]
  (->> path io/resource slurp read-string list*))

(defn parse-sac-condition [condition]
  (condp = (.getConditionType condition)
    Condition/SAC_ID_CONDITION [{:type :id
                                 :value (.getValue condition)}]
    Condition/SAC_AND_CONDITION (vec
                                  (concat
                                    (parse-sac-condition
                                      (.getFirstCondition condition))
                                    (parse-sac-condition
                                      (.getSecondCondition condition))))
    ;; Condition/SAC_LANG_CONDITION
    Condition/SAC_CLASS_CONDITION [{:type :class
                                    :value (.getValue condition)}]
    ;; Condition/SAC_CONTENT_CONDITION
    ;; Condition/SAC_NEGATIVE_CONDITION
    ;; Condition/SAC_ONLY_TYPE_CONDITION
    ;; Condition/SAC_ATTRIBUTE_CONDITION
    ;; Condition/SAC_POSITIONAL_CONDITION
    ;; Condition/SAC_ONLY_CHILD_CONDITION
    Condition/SAC_PSEUDO_CLASS_CONDITION [{:type :pseudo-class
                                           :value (.getValue condition)}]
    ;; Condition/SAC_ONE_OF_ATTRIBUTE_CONDITION
    ;; Condition/SAC_BEGIN_HYPHEN_ATTRIBUTE_CONDITION
    nil
    ))

(defn parse-sac-selector [selector]
  (condp = (.getSelectorType selector)
    Selector/SAC_CHILD_SELECTOR [:>
                                 (parse-sac-selector
                                   (.getAncestorSelector selector))
                                 (parse-sac-selector
                                   (.getSimpleSelector selector))]
    ;; Selector/SAC_ANY_NODE_SELECTOR
    ;; Selector/SAC_NEGATIVE_SELECTOR
    ;; Selector/SAC_ROOT_NODE_SELECTOR
    ;; Selector/SAC_TEXT_NODE_SELECTOR
    Selector/SAC_DESCENDANT_SELECTOR [:_
                                      (parse-sac-selector
                                        (.getAncestorSelector selector))
                                      (parse-sac-selector
                                        (.getSimpleSelector selector))]
    Selector/SAC_CONDITIONAL_SELECTOR (vec
                                        (concat
                                          (parse-sac-selector
                                            (.getSimpleSelector selector))
                                          (parse-sac-condition
                                            (.getCondition selector))))
    ;; Selector/SAC_COMMENT_NODE_SELECTOR
    Selector/SAC_ELEMENT_NODE_SELECTOR [{:type :element
                                         :value (or (.getLocalName selector) "*")}]
    Selector/SAC_PSEUDO_ELEMENT_SELECTOR [{:type :pseudo-element
                                           :value (.getLocalName selector)}]
    ;; Selector/SAC_DIRECT_ADJACENT_SELECTOR
    ;; Selector/SAC_CDATA_SECTION_NODE_SELECTOR
    ;; Selector/SAC_PROCESSING_INSTRUCTION_NODE_SELECTOR
    nil))

(defn zip-to-end [z]
  (let [z' (zip/next z)]
    (if (zip/end? z') z (recur z'))))

(defn zip-to-beg [z]
  (if-let [z' (zip/prev z)] (recur z') z))

(defn zip-walk [f z]
  (loop [z z]
    (if (zip/end? z)
      (-> z zip-to-beg)
      (recur (zip/next (f z))))))

(defn move-pseudo-elements
  "Move pseudo-elements at their own node over to the left"
  [selectors]
  (->>
    (zip/vector-zip selectors)
    (zip-walk
      (fn [z]
        (if (zip/branch? z)
          (let [[op ls rs] (zip/node z)]
            (if (and
                  (= op :_)
                  (some (comp #{:pseudo-element} :type) rs)
                  (not
                    (some (comp #{:id :class :element} :type) rs)))
              (zip/replace z
                (->
                  ls zip/vector-zip
                  zip-to-end zip/up
                  (zip/edit (partial apply conj) rs)
                  zip-to-beg zip/node))
              z))
          z)))
    zip/node))

(defn remove-wildcard-selectors
  "Remove any wildcard (*) selectors followed by a class or id."
  [selectors]
  (->>
    (zip/vector-zip selectors)
    (zip-walk
      (fn [z]
        (if (zip/branch? z)
          (let [xs (zip/node z)]
            (if (and
                  (map? (first xs))
                  (some #{{:type :element :value "*"}} xs)
                  (some (comp #{:id :class} :type) xs))
              (zip/replace z
                (vec (remove #{{:type :element :value "*"}} xs)))
              z))
          z)))
    zip/node))

(defn parse-selectors [selectors]
  (let [parser (CSSOMParser.
                 (SACParserCSS3.))]
    (->>
      selectors
      StringReader.
      InputSource.
      (.parseSelectors parser)
      .getSelectors
      (map parse-sac-selector)
      (map move-pseudo-elements)
      (map remove-wildcard-selectors))))

(defn parse-selector [selector]
  (first (parse-selectors selector)))

(defmulti render-selector-leaf :type)

(defmethod render-selector-leaf :id [{:keys [value]}]
  (str "#" value))

(defmethod render-selector-leaf :class [{:keys [value]}]
  (str "." value))

(defmethod render-selector-leaf :pseudo-class [{:keys [value]}]
  (str ":" value))

(defmethod render-selector-leaf :pseudo-element [{:keys [value]}]
  (str "::" value))

(defmethod render-selector-leaf :element [{:keys [value]}]
  value)

(defmethod render-selector-leaf :default [{:keys [value]}]
  value)

(defmulti render-selector-branch first)

(defmethod render-selector-branch :_ [[_ & xs ]]
  (->> xs (map render-selector-branch) (str/join " ")))

(defmethod render-selector-branch :> [[_ & xs]]
  (->> xs (map render-selector-branch) (str/join " > ")))

(defmethod render-selector-branch :default [[& xs]]
  (->> xs (map render-selector-leaf) str/join))

(defn render-selector [selector]
  (->> selector render-selector-branch str/join))

(defn render-selectors [selectors]
  (->> selectors (map render-selector) (str/join ", ")))

(defn expand-rule [rule]
  (cond
    (util/at-media? rule) (update-in rule [:value :rules]
                            #(->> % (map expand-rule) (remove nil?)))
    :else (let [[selectors declarations] rule]
            (when (not-empty declarations)
              [(mapv
                 #(->> % (str/join " ") parse-selector)
                 selectors)
               (vec declarations)]))))

(defn expand-rules [rules]
  (->>
    (seq rules)
    (#'garden.compiler/expand-stylesheet)
    (map expand-rule)
    (remove nil?)))

(defn collapse-rule [rule]
  (cond
    (util/at-media? rule) (update-in rule [:value :rules]
                            (partial map collapse-rule))
    :else (let [[selectors declarations] rule]
            (vec
              (concat
                (map render-selector selectors) declarations)))))

(defn collapse-rules [rules]
  (map collapse-rule rules))

(defn- selector?
  "A branch in the selector tree holding selector parts, not more branches."
  [n]
  (and (vector? n) (not (keyword? (first n)))))

(defprotocol ICelector
  (->pred [this]))

(extend-protocol ICelector

  clojure.lang.IPersistentVector
  (->pred [this]
    (let [preds (map ->pred this)]
      (fn [s rule]
        (every? #(% s rule) preds))))

  clojure.lang.Fn
  (->pred [this] this)

  clojure.lang.Keyword
  (->pred [this]
    (->pred (name this)))

  java.lang.String
  (->pred [this]
    (if (= this "&")
      (constantly true)
      (let [parts (set (parse-selector (str/replace this #"^&" "")))
            ;;  parse-selector returns an element node AND pseudo-element
            ;;  node when parsing selectors like ::pseudo-element. This
            ;;  is a janky workaround.
            parts (if (and
                        (not (re-matches #"\*" this))
                        (contains? parts {:type :element :value "*"}))
                    (set
                      (remove #{{:type :element :value "*"}} parts))
                    parts)]
        (if (empty? parts)
          (constantly true)
          (fn [s _]
            (empty?
              (set/difference parts (set s)))))))))

(defn celects? [celectors rule]
  (some
    (fn [s]
      (->>
        celectors
        (map ->pred)
        (reduce
          (fn [z pred]
            (or
              (loop [z z]
                (when-not (zip/end? z)
                  (let [n (zip/node z)]
                    (if (and (selector? n) (pred n rule))
                      z
                      (recur (zip/next z))))))
              (reduced nil)))
          (zip/vector-zip s))))
    (first rule)))

(defrecord AtMediaCelector [media-queries celectors])

(defn at-media?
  ([media-queries]
   (at-media? media-queries nil))
  ([media-queries celectors]
   (->AtMediaCelector media-queries celectors)))

(defn at-media-celector? [celector]
  (instance? AtMediaCelector celector))

(defn at-media-update-rules [rule & args]
  (apply update-in rule [:value :rules] args))

(defn celects-media? [celector rule]
  (every?
    (partial contains?
      (set (-> rule :value :media-queries)))
    (-> celector :media-queries)))

(defn- rule? [rule]
  (or
    (util/at-rule? rule)
    (and
      (seq rule)
      (map? (get-in rule [1 0])))))

(defn at [rules f & steps]
  (reduce
    (fn [rules [celectors transform]]
      (mapcat
        (fn [rule]
          (let [r (cond
                    (and
                      (util/at-media? rule)
                      (at-media-celector? celectors))
                    (when (celects-media? celectors rule)
                      (if-let [celectors (:celectors celectors)]
                        (at-media-update-rules rule at f celectors transform)
                        (transform rule)))
                    (at-media-celector? celectors) (f rule)
                    (util/at-media? rule) (apply at-media-update-rules rule at f steps)
                    (celects? celectors rule) (transform rule)
                    :else (f rule))]
            (if (rule? r) [r] r)))
        rules))
    rules
    (partition 2 steps)))

(defn property?
  ([k]
   (property? k nil))
  ([k v]
   (fn [_ [_ declarations]]
     (let [f (if v #{v} identity)
           ks (set [k (name k)])]
       (some
         (fn [ds]
           (some #(-> ds (get %) f) ks))
         declarations)))))

;;; Transforms

(defn walk-selectors [f]
  (fn [[selectors declarations]]
    [(mapv
       (fn [s]
         (->>
           (zip/vector-zip s)
           (zip-walk
             (fn [z]
               (if (selector? (zip/node z))
                 (zip/edit z (comp vec f))
                 z)))
           zip/node))
       selectors)
     declarations]))

(defn map-declarations [f]
  (fn [[selectors declarations]]
    [selectors (mapv f declarations)]))

(defn remove-class [class]
  (let [s (str/replace (name class) #"^\." "")]
    (walk-selectors
      (partial remove #{{:type :class :value s}}))))

(defn remove-property [& ks]
  (map-declarations
    #(apply dissoc % (map name ks))))

(defn set-property [& kvs]
  (let [properties (->>
                     (partition 2 kvs)
                     (map
                       (fn [[k v]] [(name k) v]))
                     (into {}))]
    (fn [[selectors declarations :as rule]]
      [selectors
       (conj
         (->>
           declarations
           (map
             #(apply dissoc % (keys properties)))
           (remove empty?)
           vec)
         properties)])))

(defn remove-property* [declarations property]
  (mapv #(dissoc % property) declarations))

(defn find-property* [declarations property]
  (some #(get % property) declarations))

(defn splice-selectors [sa sb]
  (->
    sb zip/vector-zip zip-to-end zip/up
    (zip/edit
      (fn [n]
        (if (selector? sa)
          (apply conj n sa) [:_ n sb])))
    zip-to-beg zip/node vec))

(defn splice
  ([f]
   (splice f nil))
  ([f property]
   (fn [[selectors declarations]]
     (let [k (and property
               (name property))
           v (and property
               (find-property* declarations k))
           rules (if property (f v) (f))
           declarations (vec
                          (concat
                            (map second
                              (filter (comp empty? first) rules))
                            (remove-property* declarations k)))
           rules (remove (comp empty? first) rules)]
       (vec
         (concat
           [[selectors declarations]]
           (for [r rules
                 sa (first r)
                 sb selectors :while (not (util/at-rule? r))]
             [[(splice-selectors sa sb)] (second r)])
           (for [r rules :while (util/at-media? r)]
             (update-in r [:value :rules]
               (fn [rules]
                 (for [r rules
                       sa (first r)
                       sb selectors]
                   [[(splice-selectors sa sb)] (second r)]))))))))))

(defn replace-value [& kvs]
  (let [matches (->>
                  (partition 2 kvs)
                  (mapcat
                    (fn [[k v]] [[k v]
                                 [(name k) v]]))
                  (into {}))]
    (map-declarations
      (fn [ds]
        (->>
          ds
          (map
            (fn [[k v]]
              (cond
                (and
                  (vector? v)
                  (vector? (first v))) [k
                                        [(mapv #(get matches % %) (first v))]]
                  (set? v) [k (set (map #(get matches % %) v))]
                  (vector? v) [k (mapv #(get matches % %) v)]
                  :else [k (get matches v v)])))
          (into {}))))))

(defn at-media [media-queries]
  (fn [rule]
    (assoc-in rule [:value :media-queries] media-queries)))

;;; Macros

(defmacro snippet [source celector args & forms]
  `(fn ~args
     (at (at (expand-rules
               (read-resource ~source))
           (constantly nil)
           ~celector identity)
       identity ~@forms)))

(defmacro defsnippet [name & forms]
  `(def ~name (snippet ~@forms)))

(defmacro template [source args & forms]
  `(fn ~args
     (collapse-rules
       (at (expand-rules
             (read-resource ~source))
         identity
         ~@forms))))

(defmacro deftemplate [name & forms]
  `(def ~name (template ~@forms)))

;; Aliases

(def css garden/css)
